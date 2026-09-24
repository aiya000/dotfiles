#!/usr/bin/env python3
"""Validate a staged skill folder and zip it for upload to claude.ai.

Usage: package.py SKILL_DIR... --out OUT_DIR
       package.py SKILL_DIR... --check

Each SKILL_DIR becomes OUT_DIR/<name>.zip with the folder itself at the top of
the archive (<name>/SKILL.md, not SKILL.md), which is what the upload expects.

The checks are the ones claude.ai applies on upload, so a zip this script
produces is not rejected for its shape:

- exactly one SKILL.md, at the top of the folder
- frontmatter keys limited to name, description, allowed-tools, compatibility,
  license, metadata
- name: kebab-case, <= 64 chars, equal to the folder name
- description: present, <= 1024 chars, no < or >
- compatibility: <= 500 chars

Symlinks are followed (their targets are archived), and caches such as
__pycache__, node_modules, *.pyc and .DS_Store are left out. Standard library
only, so it runs wherever python3 does.
"""

import argparse
import fnmatch
import os
import re
import sys
import zipfile
from pathlib import Path

ALLOWED_KEYS = {"name", "description", "allowed-tools", "compatibility", "license", "metadata"}
EXCLUDE_DIRS = {"__pycache__", "node_modules", ".git"}
EXCLUDE_FILES = {".DS_Store"}
EXCLUDE_GLOBS = {"*.pyc", "*.swp", "*~"}


def parse_frontmatter(text):
    """Return {key: value} for the top-level keys of the YAML frontmatter.

    Uses PyYAML when it is installed; otherwise a minimal reader that is enough
    for `key: value` lines and indented continuations (metadata: blocks).
    """
    match = re.match(r"^---\n(.*?)\n---", text, re.DOTALL)
    if not match:
        raise ValueError("no YAML frontmatter (the file must start with ---)")
    body = match.group(1)
    try:
        import yaml  # type: ignore

        data = yaml.safe_load(body)
        if not isinstance(data, dict):
            raise ValueError("frontmatter must be a YAML mapping")
        return data
    except ImportError:
        pass
    data = {}
    for line in body.splitlines():
        if not line.strip() or line.startswith((" ", "\t", "#")):
            continue
        key, sep, value = line.partition(":")
        if not sep:
            raise ValueError(f"cannot read frontmatter line: {line!r}")
        value = value.strip()
        if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
            value = value[1:-1]
        data[key.strip()] = value
    return data


def validate(skill_dir):
    """Return a list of problems; empty means the folder can be uploaded."""
    problems = []
    skill_md = skill_dir / "SKILL.md"
    if not skill_md.is_file():
        return ["SKILL.md not found at the top of the folder"]

    nested = [
        p.relative_to(skill_dir)
        for p in skill_dir.rglob("SKILL.md")
        if p != skill_md and not EXCLUDE_DIRS.intersection(p.relative_to(skill_dir).parts)
    ]
    if nested:
        problems.append(
            "more than one SKILL.md (claude.ai accepts exactly one); rename: "
            + ", ".join(map(str, nested))
        )

    try:
        fm = parse_frontmatter(skill_md.read_text(encoding="utf-8"))
    except ValueError as e:
        return problems + [str(e)]

    extra = sorted(set(fm) - ALLOWED_KEYS)
    if extra:
        problems.append(
            f"frontmatter key(s) rejected on upload: {', '.join(extra)} "
            f"(allowed: {', '.join(sorted(ALLOWED_KEYS))})"
        )

    name = str(fm.get("name", "")).strip()
    if not name:
        problems.append("name is missing")
    else:
        if not re.fullmatch(r"[a-z0-9]+(-[a-z0-9]+)*", name):
            problems.append(f"name {name!r} must be kebab-case (a-z, 0-9, single hyphens)")
        if len(name) > 64:
            problems.append(f"name is {len(name)} chars (max 64)")
        if name != skill_dir.name:
            problems.append(f"name {name!r} differs from the folder name {skill_dir.name!r}")

    desc = fm.get("description")
    if not isinstance(desc, str) or not desc.strip():
        problems.append("description is missing")
    else:
        if len(desc.strip()) > 1024:
            problems.append(f"description is {len(desc.strip())} chars (max 1024)")
        if "<" in desc or ">" in desc:
            problems.append("description contains < or >")

    compat = fm.get("compatibility")
    if compat is not None and (not isinstance(compat, str) or len(compat) > 500):
        problems.append("compatibility must be a string of at most 500 chars")

    return problems


def excluded(rel):
    if EXCLUDE_DIRS.intersection(rel.parts[:-1]):
        return True
    return rel.name in EXCLUDE_FILES or any(fnmatch.fnmatch(rel.name, g) for g in EXCLUDE_GLOBS)


def package(skill_dir, out_dir):
    out_dir.mkdir(parents=True, exist_ok=True)
    target = out_dir / f"{skill_dir.name}.zip"
    partial = target.with_suffix(".zip.partial")
    count = 0
    with zipfile.ZipFile(partial, "w", zipfile.ZIP_DEFLATED) as zf:
        for root, dirs, files in os.walk(skill_dir, followlinks=True):
            dirs[:] = sorted(d for d in dirs if d not in EXCLUDE_DIRS)
            for f in sorted(files):
                path = Path(root) / f
                rel = path.relative_to(skill_dir)
                if excluded(rel):
                    continue
                zf.write(path, Path(skill_dir.name) / rel)
                count += 1
    partial.replace(target)
    return target, count


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("skills", nargs="+", type=Path, help="staged skill folders")
    ap.add_argument("--out", type=Path, help="directory for the zips (required unless --check)")
    ap.add_argument("--check", action="store_true", help="validate only, write nothing")
    args = ap.parse_args()
    if not args.check and args.out is None:
        ap.error("--out is required unless --check is given")

    failed = 0
    for skill in args.skills:
        skill = skill.resolve()
        problems = validate(skill)
        if problems:
            failed += 1
            print(f"NG  {skill.name}")
            for p in problems:
                print(f"    - {p}")
            continue
        if args.check:
            print(f"OK  {skill.name}")
            continue
        target, count = package(skill, args.out.resolve())
        print(f"OK  {skill.name} -> {target} ({count} files)")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
