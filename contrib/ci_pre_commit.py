#!/usr/bin/env python3
"""Check a PR patch against its target branch using pre-commit."""

import argparse
import os
import subprocess
import sys


def git(*args, check=True):
    return subprocess.run(["git", *args], check=check,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--target-branch", default=os.getenv("TARGET_BRANCH"))
    parser.add_argument("--remote", default="origin")
    parser.add_argument("--repository-url",
                        help="Fetch directly from this target repository "
                             "when the CI checkout has no named remote")
    parser.add_argument("--pr-ref",
                        help="Server-side PR head ref to deepen alongside "
                             "the target branch, e.g. "
                             "refs/merge-requests/402/head")
    parser.add_argument("--head-ref", default="HEAD")
    parser.add_argument("--no-fetch", action="store_true",
                        help="Use already fetched remote refs (offline mode)")
    parser.add_argument("--deepen", type=int, default=100,
                        help="History added per retry; at most three retries")
    args = parser.parse_args()
    if not args.target_branch or args.deepen < 1:
        parser.error("A target branch and a positive --deepen are required")
    try:
        git("check-ref-format", "refs/heads/" + args.target_branch)
        if args.pr_ref:
            git("check-ref-format", args.pr_ref)
        remotes = git("remote").stdout.decode().splitlines()
        if not args.repository_url and args.remote not in remotes:
            raise ValueError("--remote must name a configured Git remote")
        head = git("rev-parse", "--verify", "--end-of-options",
                   args.head_ref + "^{commit}").stdout.decode().strip()
        remote = args.repository_url or args.remote
        namespace = "pre-commit-target" if args.repository_url else args.remote
        target = f"refs/remotes/{namespace}/{args.target_branch}"
        refspec = f"+refs/heads/{args.target_branch}:{target}"
        if not args.no_fetch:
            git("fetch", "--no-tags", "--", remote, refspec)
        base = git("merge-base", target, head, check=False)
        for _ in range(3):
            if base.returncode == 0 or args.no_fetch:
                break
            if git("rev-parse", "--is-shallow-repository").stdout.strip() != b"true":
                break
            print(f"No merge base yet; fetching {args.deepen} more commits",
                  flush=True)
            refs = [refspec]
            if args.pr_ref:
                refs.append(f"+{args.pr_ref}:refs/pre-commit/pr-head")
            git("fetch", "--no-tags", f"--deepen={args.deepen}",
                "--", remote, *refs)
            if args.pr_ref:
                fetched = git("rev-parse", "refs/pre-commit/pr-head")
                if fetched.stdout.decode().strip() != head:
                    raise ValueError("PR head changed while fetching history; "
                                     "retry CI for the new commit")
            base = git("merge-base", target, head, check=False)
        if base.returncode:
            raise ValueError("Cannot establish the PR merge base. Fetch more "
                             "history for the PR and target branch, then retry.")
        before = base.stdout.decode().strip()
        print(f"Checking PR patch {before}..{head}", flush=True)
        subprocess.run([sys.executable, "-m", "pre_commit", "validate-config",
                        ".pre-commit-config.yaml"], check=True)
        return subprocess.call([sys.executable, "-m", "pre_commit", "run",
                                "--hook-stage", "manual", "--from-ref", before,
                                "--to-ref", head, "--show-diff-on-failure"])
    except subprocess.CalledProcessError as error:
        if error.stderr:
            sys.stderr.buffer.write(error.stderr)
        return error.returncode or 1
    except (ValueError, OSError) as error:
        print(f"CI patch check failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
