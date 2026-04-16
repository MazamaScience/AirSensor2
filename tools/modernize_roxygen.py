#!/usr/bin/env python3

from __future__ import annotations

import argparse
import pathlib
import re
import shutil
import sys


# Match a single roxygen line, preserving the prefix and spacing after #'
ROXYGEN_LINE_RE = re.compile(r"^(\s*#')(\s?)(.*)$")

# Ordered from most specific to most general.
PATTERNS: list[tuple[re.Pattern[str], str]] = [
    # \code{pkg::\link[pkg:fun]{fun}} -> [pkg::fun()]
    (
        re.compile(
            r"\\code\{([A-Za-z0-9._]+)::\\link\[([A-Za-z0-9._]+):([A-Za-z0-9._]+)\]\{([A-Za-z0-9._]+)\}\}"
        ),
        r"[\1::\4()]",
    ),
    # \code{\link[pkg]{fun}} -> [pkg::fun()]
    (
        re.compile(r"\\code\{\\link\[([A-Za-z0-9._]+)\]\{([A-Za-z0-9._]+)\}\}"),
        r"[\1::\2()]",
    ),

    # \code{\link{pkg::fun}} -> [pkg::fun()]
    (
        re.compile(r"\\code\{\\link\{([A-Za-z0-9._]+)::([A-Za-z0-9._]+)\}\}"),
        r"[\1::\2()]",
    ),

    # \code{\link{fun}} -> [fun()]
    (
        re.compile(r"\\code\{\\link\{([A-Za-z0-9._]+)\}\}"),
        r"[\1()]",
    ),

    # \link[pkg]{fun} -> [pkg::fun()]
    (
        re.compile(r"\\link\[([A-Za-z0-9._]+)\]\{([A-Za-z0-9._]+)\}"),
        r"[\1::\2()]",
    ),

    # \link{pkg::fun} -> [pkg::fun()]
    (
        re.compile(r"\\link\{([A-Za-z0-9._]+)::([A-Za-z0-9._]+)\}"),
        r"[\1::\2()]",
    ),

    # \link{fun} -> [fun()]
    (
        re.compile(r"\\link\{([A-Za-z0-9._]+)\}"),
        r"[\1()]",
    ),

    # \href{url}{label} -> [label](url)
    (
        re.compile(r"\\href\{([^}]*)\}\{([^}]*)\}"),
        r"[\2](\1)",
    ),

    # \code{/link{thing}} -> [thing]
    (
        re.compile(r"\\code\{/?link\{([A-Za-z0-9._]+)\}\}"),
        r"[\1]",
    ),
    # Simple inline markup
    (
        re.compile(r"\\code\{([^{}]*)\}"),
        r"`\1`",
    ),
    (
        re.compile(r"\\emph\{([^{}]*)\}"),
        r"*\1*",
    ),
    (
        re.compile(r"\\strong\{([^{}]*)\}"),
        r"**\1**",
    ),
]


def detect_newline(text: str) -> str:
    """
    Detect the newline convention used in the file.
    """
    if "\r\n" in text:
        return "\r\n"
    if "\r" in text:
        return "\r"
    return "\n"


def transform_roxygen_line(line_body: str) -> str:
    """
    Apply ordered substitutions to the body of a roxygen line.
    """
    new_body = line_body
    for pattern, replacement in PATTERNS:
        new_body = pattern.sub(replacement, new_body)
    return new_body


def transform_roxygen_text(text: str) -> str:
    """
    Transform roxygen comment lines in-place while preserving all original
    line endings and all non-roxygen lines exactly as they were.
    """
    newline = detect_newline(text)
    parts = text.split(newline)

    # split() drops the separator, so we rejoin with the same newline later.
    # This preserves blank lines and the file's original newline convention.
    for i, part in enumerate(parts):
        match = ROXYGEN_LINE_RE.match(part)
        if match is None:
            continue

        prefix = match.group(1)
        spacer = match.group(2)
        body = match.group(3)

        new_body = transform_roxygen_line(body)
        parts[i] = f"{prefix}{spacer}{new_body}"

    return newline.join(parts)


def process_file(
    path: pathlib.Path,
    dry_run: bool = False,
    backup_suffix: str = ".bak",
) -> bool:
    with path.open("r", encoding="utf-8", newline="") as f:
        original = f.read()

    updated = transform_roxygen_text(original)

    if updated == original:
        return False

    if dry_run:
        return True

    # No backups while using git
    ###ackup_path = path.with_name(path.name + backup_suffix)
    ###shutil.copy2(path, backup_path)

    newline = detect_newline(original)
    with path.open("w", encoding="utf-8", newline=newline) as f:
        f.write(updated)

    return True


def find_r_files(root: pathlib.Path) -> list[pathlib.Path]:
    """
    Find all .R files directly under the R/ directory.
    """
    r_dir = root / "R"
    if not r_dir.exists():
        raise FileNotFoundError(f"Could not find directory: {r_dir}")
    return sorted(r_dir.glob("*.R"))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Modernize old roxygen2 inline markup in R/*.R files."
    )
    parser.add_argument(
        "--root",
        default=".",
        help="Package root directory containing R/ (default: current directory).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Report which files would change, but do not modify anything.",
    )
    parser.add_argument(
        "--backup-suffix",
        default=".bak",
        help="Suffix for backup files (default: .bak).",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    root = pathlib.Path(args.root).resolve()

    try:
        files = find_r_files(root)
    except FileNotFoundError as exc:
        print(str(exc), file=sys.stderr)
        return 1

    changed_count = 0

    for path in files:
        changed = process_file(
            path=path,
            dry_run=args.dry_run,
            backup_suffix=args.backup_suffix,
        )
        if changed:
            changed_count += 1
            verb = "would update" if args.dry_run else "updated"
            print(f"{verb}: {path}")

    if changed_count == 0:
        print("No files needed changes.")
    else:
        summary = "would be updated" if args.dry_run else "updated"
        print(f"\nTotal files {summary}: {changed_count}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
