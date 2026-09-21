#!/usr/bin/env python3
"""Exercise patch checks against real temporary Git repositories.

Run with Python containing unidiff and termcolor. No GCC build is required.
"""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


CONTRIB = Path(__file__).resolve().parent


class PatchChecks(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="gcc-patch-check-")
        self.addCleanup(self.temp.cleanup)
        self.repo = Path(self.temp.name)
        self.env = os.environ.copy()
        for key in ("PRE_COMMIT_FROM_REF", "PRE_COMMIT_TO_REF", "GIT_INDEX_FILE",
                    "GIT_DIR", "GIT_WORK_TREE", "CI", "JENKINS_URL",
                    "PRE_COMMIT"):
            self.env.pop(key, None)
        self.git("init", "-b", "master")
        self.git("config", "user.name", "Patch Check Test")
        self.git("config", "user.email", "patch-check@example.invalid")
        self.git("config", "core.autocrlf", "false")
        self.git("config", "core.hooksPath", str(self.repo / "no-hooks"))
        self.write("gcc/sample.cc", "int old_name(void);  \n")
        self.commit()

    def git(self, *args):
        return subprocess.run(["git", *args], cwd=self.repo, env=self.env,
                              check=True, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE).stdout.decode().strip()

    def write(self, name, text):
        path = self.repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(text.encode())

    def commit(self):
        self.git("add", ".")
        self.git("commit", "-m", "Fixture")
        return self.git("rev-parse", "HEAD")

    def check(self, kind, expected=0, refs=None, filenames=(), advisory=False):
        env = self.env.copy()
        if refs:
            env.update(PRE_COMMIT_FROM_REF=refs[0], PRE_COMMIT_TO_REF=refs[1])
        result = subprocess.run([sys.executable, str(CONTRIB / "pre_commit.py"),
                                 *(["--advisory"] if advisory else []),
                                 kind, *filenames], cwd=self.repo, env=env,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = (result.stdout + result.stderr).decode(errors="replace")
        if expected == 0:
            self.assertEqual(result.returncode, 0, output)
        else:
            self.assertNotEqual(result.returncode, 0, output)
        return output

    def test_no_changes_and_unborn_index(self):
        self.check("whitespace")
        self.check("gnu-style")
        self.git("checkout", "--orphan", "new-root")
        self.git("rm", "-rf", ".")
        self.write("gcc/new.cc", "int new_name (void);\n")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_only_added_lines_not_historical_errors(self):
        self.write("gcc/sample.cc", "int old_name(void);  \n"
                   "int new_name (void);\n")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_staged_patch_ignores_unstaged_errors(self):
        self.write("gcc/new.cc", "int new_name (void);\n")
        self.git("add", ".")
        self.write("gcc/new.cc", "int new_name(void);  \n")
        self.check("whitespace")
        self.check("gnu-style")

    def test_new_whitespace_error(self):
        self.write("gcc/new.cc", "int new_name;  \n")
        self.git("add", ".")
        self.assertIn("trailing whitespace", self.check("whitespace", 1))

    def test_new_gnu_error(self):
        self.write("gcc/new.cc", "int new_name(void);\n")
        self.git("add", ".")
        self.assertIn("exactly one space", self.check("gnu-style", 1))

    def test_advisory_reports_style_findings_in_staged_and_ci_modes(self):
        base = self.git("rev-parse", "HEAD")
        self.write("gcc/new.cc", "int new_name(void);\n")
        self.git("add", ".")
        for committed in (False, True):
            refs = (base, self.commit()) if committed else None
            if committed:
                self.env["CI"] = "true"
            output = self.check("gnu-style", refs=refs, advisory=True,
                                filenames=("gcc/new.cc",))
            self.assertIn("exactly one space", output)
            self.assertIn("WARNING: GNU style findings are advisory", output)
            self.check("gnu-style", 1, refs=refs, filenames=("gcc/new.cc",))

    def test_advisory_does_not_suppress_setup_errors(self):
        self.check("gnu-style", 1, refs=("missing-branch", "HEAD"),
                   advisory=True)
        self.env["CI"] = "true"
        output = self.check("gnu-style", 1, advisory=True)
        self.assertIn("CI requires", output)
        output = self.check("whitespace", 1, advisory=True)
        self.assertIn("only supported for gnu-style", output)

    def test_advisory_does_not_suppress_checker_failures(self):
        self.write("gcc/new.cc", "int new_name (void);\n")
        self.git("add", ".")
        # Substitute a failed checker in a temporary copy of the adapter.
        adapter = self.repo / "checker" / "pre_commit.py"
        adapter.parent.mkdir()
        shutil.copyfile(CONTRIB / "pre_commit.py", adapter)
        for body, message in (
                ("raise RuntimeError('checker crashed')", "checker crashed"),
                ("return None", "Unexpected GNU style checker result"),
                ("return 2", "Unexpected GNU style checker result")):
            with self.subTest(body=body):
                self.write("checker/check_GNU_style_lib.py",
                           "def check_GNU_style_file(*args):\n    " + body + "\n")
                result = subprocess.run(
                    [sys.executable, "-B", str(adapter), "--advisory",
                     "gnu-style", "gcc/new.cc"], cwd=self.repo, env=self.env,
                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                output = result.stdout.decode(errors="replace")
                self.assertNotEqual(result.returncode, 0, output)
                self.assertIn(message, output)
                self.assertNotIn("findings are advisory", output)

    def test_gnu_library_returns_status_without_exiting(self):
        code = (
            "import sys; "
            "from check_GNU_style_lib import check_GNU_style_file; "
            "result = check_GNU_style_file(sys.stdin, None, sys.argv[1]); "
            "print('RETURN_CODE=' + str(result))"
        )
        env = dict(self.env, PYTHONPATH=str(CONTRIB))
        for text, expected in (("int new_name (void);\n", 0),
                               ("int new_name(void);  \n", 1)):
            self.write("gcc/new.cc", text)
            self.git("add", "gcc/new.cc")
            patch = subprocess.check_output(["git", "diff", "--cached"],
                                            cwd=self.repo, env=self.env)
            for format in ("stdio", "quickfix"):
                with self.subTest(expected=expected, format=format):
                    result = subprocess.run(
                        [sys.executable, "-c", code, format],
                        cwd=self.repo, env=env, input=patch,
                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                    output = result.stdout.decode(errors="replace")
                    self.assertEqual(result.returncode, 0, output)
                    self.assertIn(f"RETURN_CODE={expected}", output)
                    if format == "quickfix":
                        diagnostics = (self.repo / "errors.err").read_text()
                    else:
                        diagnostics = output
                    if expected:
                        self.assertIn("exactly one space", diagnostics)
                        self.assertIn("trailing whitespace", diagnostics.lower())
                    else:
                        self.assertNotIn("exactly one space", diagnostics)

    def test_gnu_command_line_preserves_exit_status(self):
        for text, expected in (("int new_name(void);  \n", 1),
                               ("int new_name (void);\n", 0)):
            self.write("gcc/new.cc", text)
            self.git("add", "gcc/new.cc")
            patch = subprocess.check_output(["git", "diff", "--cached"],
                                            cwd=self.repo, env=self.env)
            patch_file = self.repo / "input.patch"
            patch_file.write_bytes(patch)
            for format in ("stdio", "quickfix"):
                for source in ("-", str(patch_file)):
                    with self.subTest(expected=expected, format=format,
                                      source=source):
                        result = subprocess.run(
                            [sys.executable, str(CONTRIB / "check_GNU_style.py"),
                             source, "--format", format],
                            cwd=self.repo, env=self.env, input=patch,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                        output = result.stdout.decode(errors="replace")
                        self.assertEqual(result.returncode, expected, output)
                        if format == "quickfix":
                            diagnostics = (self.repo / "errors.err").read_text()
                            self.assertEqual(bool(diagnostics), bool(expected))
                        elif expected:
                            self.assertIn("exactly one space", output)

    def test_tabs_and_underscore_names(self):
        self.write("gcc/new.cc", "void\nnew_name (int opts_set,\n"
                   "\t  int other_arg)\n{\n  return;\n}\n")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_testsuite_and_patch_data_are_exempt(self):
        for name in ("gcc/testsuite/probe.c", "libcpp/testsuite/probe.c",
                     "contrib/example.patch"):
            self.write(name, "int deliberately_bad(void);  \n")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_non_gnu_languages_and_runtimes(self):
        self.write("contrib/helper.py", "print('hello')\n")
        self.write("libsanitizer/helper.cc", "int upstreamName(void);\n")
        self.git("add", ".")
        self.check("gnu-style")

    def test_def_macro_tables_skip_style_but_keep_whitespace_checks(self):
        base = self.git("rev-parse", "HEAD")
        names = tuple(f"{root}/sample.def"
                      for root in ("gcc", "libcpp", "include", "c++tools"))
        for name in names:
            self.write(name, 'DEF_ENTRY(SAMPLE, "sample")  \n')
        self.git("add", ".")
        for committed in (False, True):
            refs = (base, self.commit()) if committed else None
            if committed:
                self.env["CI"] = "true"
            for filenames in ((), names):
                for advisory in (False, True):
                    output = self.check("gnu-style", refs=refs,
                                        filenames=filenames, advisory=advisory)
                    self.assertEqual(output, "")
                output = self.check("whitespace", 1, refs=refs,
                                    filenames=filenames)
                for name in names:
                    self.assertIn(f"{name}:1: trailing whitespace", output)

    def test_def_exclusion_preserves_style_checks_in_mixed_patch(self):
        self.write("gcc/sample.def", 'DEF_ENTRY(SAMPLE, "sample")\n')
        names = ("gcc/new.c", "gcc/new.cc", "include/new.h", "gcc/new.inc")
        for name in names:
            self.write(name, "int new_name(void);\n")
        self.git("add", ".")
        for advisory in (False, True):
            output = self.check("gnu-style", 0 if advisory else 1,
                                filenames=("gcc/sample.def", *names),
                                advisory=advisory)
            self.assertNotIn("sample.def", output)
            for name in names:
                self.assertIn(name + ":1:", output)
            self.assertIn("exactly one space", output)

    def test_non_utf8_text_does_not_block_other_checks(self):
        (self.repo / "legacy.txt").write_bytes(b"caf\xe9\n")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_conflict_markers_are_rejected(self):
        self.write("gcc/new.cc", "<<<<<<< HEAD\nint first;\n=======\n"
                   "int second;\n>>>>>>> other\n")
        self.git("add", ".")
        self.check("whitespace", 1)

    def test_rename_does_not_recheck_old_lines(self):
        self.git("mv", "gcc/sample.cc", "gcc/renamed file.cc")
        self.check("whitespace")
        self.check("gnu-style")

    def test_filename_with_spaces_is_checked(self):
        self.write("gcc/new file.cc", "int bad_name(void);\n")
        self.git("add", ".")
        self.check("gnu-style", 1)

    def test_deletion_and_binary(self):
        self.git("rm", "gcc/sample.cc")
        (self.repo / "gcc").mkdir(exist_ok=True)
        (self.repo / "gcc/binary.cc").write_bytes(b"\0\xff\x01")
        self.git("add", ".")
        self.check("whitespace")
        self.check("gnu-style")

    def test_refs_ignore_unrelated_index_and_worktree(self):
        before = self.git("rev-parse", "HEAD")
        self.write("gcc/new.cc", "int new_name (void);\n")
        after = self.commit()
        self.write("gcc/other.cc", "int bad_name(void);  \n")
        self.git("add", ".")
        self.check("whitespace", refs=(before, after))
        self.check("gnu-style", refs=(before, after))

    def test_invalid_or_incomplete_refs_fail(self):
        self.check("whitespace", 1, refs=("missing-branch", "HEAD"))
        self.env["PRE_COMMIT_FROM_REF"] = "HEAD"
        self.check("gnu-style", 1)

    def divergent_refs(self, target_text, pr_text):
        base = self.git("rev-parse", "HEAD")
        self.write("gcc/sample.cc", target_text)
        self.write("target-only.txt", "Target branch change\n")
        target = self.commit()
        self.git("checkout", "-b", "pr", base)
        self.write("gcc/sample.cc", pr_text)
        head = self.commit()
        return target, head

    def test_diverged_refs_do_not_recheck_historical_lines(self):
        refs = self.divergent_refs(
            "int old_name (void);\n",
            "int old_name(void);  \nint pr_name (void);\n")
        for kind in ("whitespace", "gnu-style"):
            for filenames in ((), ("gcc/sample.cc",)):
                self.check(kind, refs=refs, filenames=filenames)

    def test_diverged_refs_do_not_hide_identical_new_errors(self):
        text = "int old_name(void);  \nint new_bad_name(void);  \n"
        refs = self.divergent_refs(text, text)
        for kind in ("whitespace", "gnu-style"):
            for filenames in ((), ("gcc/sample.cc",)):
                output = self.check(kind, 1, refs=refs, filenames=filenames)
                self.assertIn("new_bad_name", output)
                self.assertNotIn("old_name", output)

    def test_unrelated_refs_fail_even_with_empty_batch(self):
        before = self.git("rev-parse", "HEAD")
        self.git("checkout", "--orphan", "unrelated")
        self.write("gcc/sample.cc", "int other_name (void);\n")
        after = self.commit()
        for through_pre_commit in (False, True):
            if through_pre_commit:
                self.env["PRE_COMMIT"] = "1"
            for kind in ("whitespace", "gnu-style"):
                output = self.check(kind, 1, refs=(before, after))
                self.assertIn("Cannot establish", output)

    def test_actual_pre_commit_configuration_with_diverged_refs(self):
        for name in ("pre_commit.py", "check_GNU_style_lib.py"):
            dest = self.repo / "contrib" / name
            dest.parent.mkdir(exist_ok=True)
            shutil.copyfile(CONTRIB / name, dest)
        shutil.copyfile(CONTRIB.parent / ".pre-commit-config.yaml",
                        self.repo / ".pre-commit-config.yaml")
        self.commit()
        target, head = self.divergent_refs(
            "int old_name (void);\nint new_bad_name(void);  \n",
            "int old_name(void);  \nint pr_name (void);\n")
        command = [sys.executable, "-m", "pre_commit", "run",
                   "--hook-stage", "manual", "--color=never",
                   "--from-ref", target, "--to-ref"]
        for has_error in (False, True):
            if has_error:
                self.write("gcc/sample.cc", "int old_name(void);  \n"
                           "int pr_name (void);\nint new_bad_name(void);  \n")
                head = self.commit()
            result = subprocess.run(command + [head], cwd=self.repo,
                                    env=self.env, stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            output = result.stdout.decode(errors="replace")
            self.assertNotIn("old_name", output)
            if has_error:
                self.assertNotEqual(result.returncode, 0, output)
                self.assertIn("trailing whitespace", output)
                self.assertIn("exactly one space", output)
                self.assertIn("new_bad_name", output)
            else:
                self.assertEqual(result.returncode, 0, output)

    def test_file_only_run_with_clean_index_fails(self):
        output = self.check("whitespace", 1, filenames=("gcc/sample.cc",))
        self.assertIn("No staged patch", output)

    def test_ci_without_revision_range_fails(self):
        self.env["JENKINS_URL"] = "https://jenkins.example.invalid/"
        self.assertIn("CI requires", self.check("whitespace", 1))

    def test_staged_filenames_and_ci_revision_range(self):
        base = self.git("rev-parse", "HEAD")
        self.write("gcc/new.cc", "int new_name(void);\n")
        self.git("add", ".")
        self.check("gnu-style", 1, filenames=("gcc/new.cc",))
        head = self.commit()
        self.env["CI"] = "true"
        self.check("gnu-style", 1, refs=(base, head),
                   filenames=("gcc/new.cc",))

    def test_batches_report_each_error_once(self):
        base = self.git("rev-parse", "HEAD")
        names = [f"gcc/batch{i:02}.cc" for i in range(12)]
        for name in names:
            self.write(name, "int bad_name(void);  \n")
        self.git("add", ".")
        for committed in (False, True):
            refs = (base, self.commit()) if committed else None
            for kind in ("whitespace", "gnu-style"):
                outputs = []
                for start in range(0, len(names), 4):
                    batch = names[start:start + 4]
                    output = self.check(kind, 1, refs=refs, filenames=batch)
                    for name in set(names) - set(batch):
                        self.assertNotIn(name, output)
                    outputs.append(output)
                combined = "".join(outputs)
                for name in names:
                    if kind == "whitespace":
                        self.assertEqual(combined.count(
                            f"{name}:1: trailing whitespace"), 1, combined)
                    else:
                        # One parentheses error and one whitespace error.
                        self.assertEqual(combined.count(name + ":1:"), 2,
                                         combined)

    def test_batch_preserves_renames_and_new_line_checks(self):
        old = "int old_name(void);  \n" * 20
        self.write("gcc/sample.cc", old)
        base = self.commit()
        self.git("mv", "gcc/sample.cc", "gcc/renamed file.cc")
        self.write("gcc/other.cc", "int unrelated(void);  \n")
        self.git("add", ".")
        names = ("gcc/renamed file.cc",)
        for kind in ("whitespace", "gnu-style"):
            self.check(kind, filenames=names)
        self.write(names[0], old + "int added_name(void);  \n")
        self.git("add", ".")
        for kind in ("whitespace", "gnu-style"):
            output = self.check(kind, 1, filenames=names)
            self.assertNotIn("old_name", output)
            self.assertNotIn("unrelated", output)
            self.assertIn("added_name", output)
        head = self.commit()
        for kind in ("whitespace", "gnu-style"):
            output = self.check(kind, 1, refs=(base, head), filenames=names)
            self.assertNotIn("old_name", output)
            self.assertNotIn("unrelated", output)
            self.assertIn("added_name", output)

    def test_batch_uses_literal_paths_and_staged_contents(self):
        names = ("gcc/new [ab] file.cc",)
        self.write(names[0], "int good_name (void);\n")
        self.write("gcc/new a file.cc", "int bad_name(void);  \n")
        self.git("add", ".")
        self.write(names[0], "int unstaged_name(void);  \n")
        for kind in ("whitespace", "gnu-style"):
            self.check(kind, filenames=names)
        self.git("add", names[0])
        for kind in ("whitespace", "gnu-style"):
            output = self.check(kind, 1, filenames=names)
            self.assertIn("unstaged_name", output)
            self.assertNotIn("bad_name", output)

    def test_empty_pre_commit_batch_does_not_check_other_files(self):
        base = self.git("rev-parse", "HEAD")
        self.write("gcc/new.cc", "int bad_name(void);  \n")
        self.git("add", ".")
        for kind in ("whitespace", "gnu-style"):
            self.check(kind, 1)
        self.env["PRE_COMMIT"] = "1"
        for kind in ("whitespace", "gnu-style"):
            self.check(kind)
        head = self.commit()
        for kind in ("whitespace", "gnu-style"):
            self.check(kind, refs=(base, head))

    def test_actual_pre_commit_configuration_batches(self):
        # Exercise the production language: python hooks and pre-commit's
        # own filename partitioning, including an empty --files selection.
        for name in ("pre_commit.py", "check_GNU_style_lib.py"):
            dest = self.repo / "contrib" / name
            dest.parent.mkdir(exist_ok=True)
            shutil.copyfile(CONTRIB / name, dest)
        shutil.copyfile(CONTRIB.parent / ".pre-commit-config.yaml",
                        self.repo / ".pre-commit-config.yaml")
        self.commit()
        names = [f"gcc/batch{i:02}.cc" for i in range(12)]
        for name in names:
            self.write(name, "int bad_name(void);\n")
        self.git("add", ".")
        command = [sys.executable, "-m", "pre_commit", "run", "--color=never"]
        for args in ([], ["--files", "not-a-selected-file.cc"]):
            result = subprocess.run(command + args, cwd=self.repo,
                                    env=self.env, stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            output = result.stdout.decode(errors="replace")
            if args:
                self.assertEqual(result.returncode, 0, output)
            else:
                self.assertEqual(result.returncode, 0, output)
                for name in names:
                    self.assertEqual(output.count(name + ":"), 1, output)

    def test_ci_advisory_is_visible_and_whitespace_still_blocks(self):
        for name in ("pre_commit.py", "check_GNU_style_lib.py"):
            dest = self.repo / "contrib" / name
            dest.parent.mkdir(exist_ok=True)
            shutil.copyfile(CONTRIB / name, dest)
        shutil.copyfile(CONTRIB.parent / ".pre-commit-config.yaml",
                        self.repo / ".pre-commit-config.yaml")
        base = self.commit()
        self.git("remote", "add", "origin", self.repo.as_uri())
        self.git("update-ref", "refs/remotes/origin/master", base)
        env = dict(self.env, JENKINS_URL="https://jenkins.example.invalid/")
        for whitespace in (False, True):
            self.write("gcc/new.cc", "int new_name(void);"
                       + ("  \n" if whitespace else "\n"))
            self.commit()
            result = subprocess.run(
                [sys.executable, str(CONTRIB / "ci_pre_commit.py"),
                 "--target-branch", "master", "--no-fetch"],
                cwd=self.repo, env=env, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT)
            output = result.stdout.decode(errors="replace")
            self.assertIn("exactly one space", output)
            self.assertIn("WARNING: GNU style findings are advisory", output)
            if whitespace:
                self.assertNotEqual(result.returncode, 0, output)
                self.assertIn("trailing whitespace", output)
            else:
                self.assertEqual(result.returncode, 0, output)

    def test_ci_recovers_shallow_history_and_propagates_failure(self):
        # Copy the real entry points; use the current Python's dependencies
        # to keep this fixture offline. Production language: python is tested
        # separately by running the actual configuration against PR #402.
        for name in ("pre_commit.py", "check_GNU_style_lib.py"):
            dest = self.repo / "contrib" / name
            dest.parent.mkdir(exist_ok=True)
            shutil.copyfile(CONTRIB / name, dest)
        self.write(".pre-commit-config.yaml", "repos:\n- repo: local\n"
                   "  hooks:\n  - id: whitespace\n    name: whitespace\n"
                   f"    entry: '\"{Path(sys.executable).as_posix()}\" "
                   "contrib/pre_commit.py whitespace'\n"
                   "    language: system\n    pass_filenames: true\n"
                   "    always_run: true\n")
        self.commit()
        base = self.git("rev-parse", "HEAD")
        for i in range(5):
            self.write("target.txt", f"{i}\n")
            self.commit()
        self.git("checkout", "-b", "pr", base)
        self.write("gcc/new.cc", "int new_name;  \n")
        self.commit()
        # Keep the merge base beyond both shallow tips.
        for i in range(12):
            self.write("pr.txt", f"{i}\n")
            self.commit()
        with tempfile.TemporaryDirectory(prefix="gcc-shallow-check-") as clone:
            subprocess.run(["git", "clone", "--depth", "1", "--branch", "pr",
                            self.repo.as_uri(), clone], check=True,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            offline = subprocess.run(
                [sys.executable, str(CONTRIB / "ci_pre_commit.py"),
                 "--target-branch", "master", "--no-fetch"],
                cwd=clone, env=self.env, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT)
            self.assertNotEqual(offline.returncode, 0)
            self.assertIn(b"Cannot establish", offline.stdout)
            result = subprocess.run(
                [sys.executable, str(CONTRIB / "ci_pre_commit.py"),
                 "--target-branch", "master", "--deepen", "10"],
                cwd=clone, env=self.env, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT)
            output = result.stdout.decode(errors="replace")
            self.assertIn("more commits", output)
            self.assertIn("trailing whitespace", output)
            self.assertNotEqual(result.returncode, 0, output)

        # Jenkins initializes a repository and fetches a PR URL directly,
        # without creating origin or its remote-tracking branches.
        with tempfile.TemporaryDirectory(prefix="gcc-jenkins-check-") as clone:
            commands = [
                ["git", "init", clone],
                ["git", "-C", clone, "fetch", "--depth", "4",
                 self.repo.as_uri(), "refs/heads/pr:refs/pull/402/MERGE"],
                ["git", "-C", clone, "checkout", "refs/pull/402/MERGE"],
            ]
            for command in commands:
                subprocess.run(command, check=True, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
            env = dict(self.env, JENKINS_URL="https://jenkins.example.invalid/")
            result = subprocess.run(
                [sys.executable, str(CONTRIB / "ci_pre_commit.py"),
                 "--target-branch", "master", "--deepen", "10",
                 "--repository-url", self.repo.as_uri(),
                 "--pr-ref", "refs/heads/pr"],
                cwd=clone, env=env, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT)
            output = result.stdout.decode(errors="replace")
            self.assertIn("more commits", output)
            self.assertIn("trailing whitespace", output)
            self.assertNotEqual(result.returncode, 0, output)


if __name__ == "__main__":
    unittest.main()
