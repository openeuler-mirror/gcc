#!/usr/bin/env python3
"""Run GCC checks on a staged patch or a pre-commit revision range."""

import argparse
import io
import os
from pathlib import PurePosixPath
import subprocess
import sys


# Other runtime libraries can follow different upstream coding conventions.
GNU_ROOTS = {"gcc", "libcpp", "include", "c++tools"}
# .def macro tables are not ordinary C/C++; exclude them from GNU style.
GNU_SUFFIXES = {".c", ".C", ".cc", ".cpp", ".cxx", ".h", ".hh",
                ".hpp", ".hxx", ".inc"}
EXCLUSIONS = [":(exclude,glob)**/testsuite/**",
              ":(exclude,glob)**/*.patch", ":(exclude,glob)**/*.diff"]


def git(*args, check=True):
    return subprocess.run(["git", *args], check=check,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def revisions(filenames):
    before = os.environ.get("PRE_COMMIT_FROM_REF")
    after = os.environ.get("PRE_COMMIT_TO_REF")
    if bool(before) != bool(after):
        raise ValueError("Both PRE_COMMIT_FROM_REF and PRE_COMMIT_TO_REF "
                         "must be set for a revision-range check")
    if before:
        # Resolve before appending to diff: neither value can become an option.
        commits = [git("rev-parse", "--verify", "--end-of-options",
                       ref + "^{commit}").stdout.decode().strip()
                   for ref in (before, after)]
        # Match pre-commit's FROM...TO file selection, including when the
        # branches have diverged. A tip-to-tip diff can both invent additions
        # from target-only edits and hide identical additions on both branches.
        base = git("merge-base", *commits, check=False)
        if base.returncode:
            raise ValueError("Cannot establish the revision-range merge base. "
                             "Fetch the shared history or use "
                             "contrib/ci_pre_commit.py to recover shallow history.")
        return [base.stdout.decode().strip(), commits[1]]
    if os.environ.get("CI") or os.environ.get("JENKINS_URL"):
        raise ValueError("CI requires --from-ref and --to-ref; run "
                         "contrib/ci_pre_commit.py with the PR target branch")
    if git("ls-files", "--unmerged", "-z").stdout:
        raise ValueError("Resolve unmerged index entries before checking")
    if filenames and not git("diff", "--cached", "--name-only", "-z").stdout:
        raise ValueError("No staged patch to check. For committed changes use "
                         "--from-ref and --to-ref, not --files or --all-files")
    return ["--cached"]


def batch_paths(common, filenames):
    if not filenames:
        return ["--", ".", *EXCLUSIONS]

    selected = set(filenames)
    # Read names only across the full range. Limiting a rename to its new
    # path would turn all its historical lines into additions in the diff.
    fields = iter(git(*common, "--name-status", "-z", "--", ".",
                      *EXCLUSIONS).stdout.split(b"\0")[:-1])
    for status in fields:
        source = os.fsdecode(next(fields))
        if status.startswith((b"R", b"C")):
            target = os.fsdecode(next(fields))
            if target in filenames:
                selected.add(source)
    # Paths are repository-relative filenames, never Git pathspec patterns.
    return ["--", *(":(top,literal)" + path for path in sorted(selected)),
            *EXCLUSIONS]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--advisory", action="store_true",
                        help="Report GNU style findings without failing the hook")
    parser.add_argument("check", choices=("gnu-style",))
    parser.add_argument("filenames", nargs="*",
                        help="Restrict the patch to this batch of files")
    args = parser.parse_args()
    try:
        revs = revisions(args.filenames)
        # always_run also invokes hooks when pre-commit selected no files.
        # A direct invocation without filenames still checks the whole patch.
        if not args.filenames and os.environ.get("PRE_COMMIT"):
            return 0
        common = ["-c", "core.quotePath=false",
                  "diff", "--no-ext-diff", "--no-textconv", "--no-color",
                  "--find-renames", *revs]
        paths = batch_paths(common, args.filenames)
        from unidiff import PatchSet
        from check_GNU_style_lib import check_GNU_style_file

        patch = git(*common, "--src-prefix=a/", "--dst-prefix=b/",
                    "--unified=3", *paths).stdout.decode("utf-8", errors="replace")
        selected = []
        for changed in PatchSet(patch):
            if changed.is_binary_file or changed.is_removed_file:
                continue
            path = PurePosixPath(changed.path)
            if (path.parts[0] in GNU_ROOTS
                    and path.suffix in GNU_SUFFIXES
                    and "testsuite" not in path.parts):
                selected.append(str(changed))
        if selected:
            result = check_GNU_style_file(io.StringIO("".join(selected)),
                                         "stdio")
            if type(result) is not int or result not in (0, 1):
                raise ValueError(f"Unexpected GNU style checker result: {result!r}")
            if result == 1 and args.advisory:
                print("WARNING: GNU style findings are advisory; "
                      "review them with the repository maintainers. "
                      "They do not block this check.")
                return 0
            return result
        return 0
    except subprocess.CalledProcessError as error:
        sys.stderr.buffer.write(error.stderr)
        return error.returncode or 1
    except (ValueError, UnicodeError) as error:
        print(f"Patch check failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
