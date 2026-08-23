#!/usr/bin/env python3
"""Build every release declared in docs/versions.json into one Pages site."""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import tempfile
from pathlib import Path

from build_docs import build_site


def load_versions(path: Path) -> tuple[str, list[dict[str, str]]]:
    try:
        metadata = json.loads(path.read_text(encoding="utf-8"))
        current = str(metadata["current"])
        versions = [
            {"release": str(entry["release"]), "source_ref": str(entry["source_ref"])}
            for entry in metadata["versions"]
        ]
        if current not in {entry["release"] for entry in versions}:
            raise ValueError("current release is absent from versions")
        return current, versions
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        raise ValueError(f"invalid version metadata {path}: {exc}") from exc


def run_git(root: Path, arguments: list[str]) -> None:
    result = subprocess.run(["git", "-C", str(root), *arguments], text=True, capture_output=True, check=False)
    if result.returncode:
        raise RuntimeError(f"git {' '.join(arguments)} failed:\n{result.stdout}{result.stderr}")


def build_all(
    root: Path,
    site_root: Path,
    offline_dir: Path | None = None,
    development_current: bool = True,
) -> int:
    """Build all declared versions, optionally previewing the current checkout.

    Development builds may use the checkout for the declared current release so
    untagged documentation can be previewed. Released builds must set
    ``development_current`` to ``False`` so every version, including current,
    is read from its immutable ``source_ref``.
    """
    root = root.resolve()
    versions_path = root / "docs" / "versions.json"
    current, versions = load_versions(versions_path)
    page_count = 0
    with tempfile.TemporaryDirectory(prefix="threadpool-fp-docs-") as temporary:
        checkout_root = Path(temporary)
        for entry in versions:
            release = entry["release"]
            source_root = root
            worktree = None
            if not development_current or release != current:
                worktree = checkout_root / release
                run_git(root, ["worktree", "add", "--detach", str(worktree), entry["source_ref"]])
                source_root = worktree
            try:
                archive = None
                if release == current and offline_dir is not None:
                    archive = offline_dir / f"threadpool-fp-docs-{release}.zip"
                page_count += build_site(
                    source_root / "docs",
                    site_root / release,
                    site_root,
                    versions_path,
                    offline_archive=archive,
                    release=release,
                )
            finally:
                if worktree is not None:
                    run_git(root, ["worktree", "remove", "--force", str(worktree)])
                    shutil.rmtree(worktree, ignore_errors=True)
    print(f"Built {len(versions)} documentation release path(s), {page_count} page(s) total")
    return len(versions)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--site-root", type=Path, default=Path("build/docs-site"))
    parser.add_argument("--offline-dir", type=Path)
    build_mode = parser.add_mutually_exclusive_group()
    build_mode.add_argument(
        "--development-current",
        action="store_true",
        help="Build the declared current version from this checkout (the default); historical versions use source_ref tags.",
    )
    build_mode.add_argument(
        "--released",
        action="store_true",
        help="Build every declared version from its immutable source_ref; required for published release documentation.",
    )
    args = parser.parse_args()
    root = args.root.resolve()
    site_root = args.site_root if args.site_root.is_absolute() else root / args.site_root
    offline_dir = args.offline_dir if args.offline_dir is None or args.offline_dir.is_absolute() else root / args.offline_dir
    build_all(root, site_root, offline_dir, development_current=not args.released)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())