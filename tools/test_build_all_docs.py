#!/usr/bin/env python3
"""Regression tests for building all declared documentation releases."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from build_all_docs import build_all  # noqa: E402


class BuildAllDocsTests(unittest.TestCase):
    @staticmethod
    def git(root: Path, *arguments: str) -> None:
        subprocess.run(["git", "-C", str(root), *arguments], check=True, text=True, capture_output=True)

    @staticmethod
    def write_release_source(root: Path, body: str, banner: str) -> None:
        source = root / "docs"
        source.mkdir(exist_ok=True)
        (source / "index.md").write_text(f"# Documentation\n\n{body}\n", encoding="utf-8")
        (source / "layout.json").write_text(
            json.dumps(
                {
                    "schema_version": 2,
                    "release": "0.9.1",
                    "site_title": "ThreadPool-FP documentation",
                    "description": "Temporary documentation fixture.",
                    "required_pages": ["index.md"],
                    "navigation": [{"title": "Getting Started", "pages": [{"path": "index.md", "title": "Introduction"}]}],
                    "homepage": {"banner": {"project_path": "assets/banner.svg", "alt": "Temporary banner"}},
                }
            ),
            encoding="utf-8",
        )
        (source / "versions.json").write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "current": "0.9.1",
                    "site_url": "https://example.invalid/threadpool-fp",
                    "repository_url": "https://github.com/example/threadpool-fp",
                    "versions": [{"release": "0.9.1", "source_ref": "v0.9.1"}],
                }
            ),
            encoding="utf-8",
        )
        assets = root / "assets"
        assets.mkdir(exist_ok=True)
        (assets / "banner.svg").write_text(banner, encoding="utf-8")

    def test_builds_the_current_release_without_a_git_worktree(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "docs"
            source.mkdir()
            (source / "index.md").write_text("# Documentation\n", encoding="utf-8")
            (source / "layout.json").write_text(json.dumps({"schema_version": 1, "release": "0.9.1"}), encoding="utf-8")
            (source / "versions.json").write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "current": "0.9.1",
                        "site_url": "https://example.invalid/threadpool-fp",
                        "repository_url": "https://github.com/example/threadpool-fp",
                        "versions": [{"release": "0.9.1", "source_ref": "main"}],
                    }
                ),
                encoding="utf-8",
            )

            count = build_all(root, root / "site", root / "artifacts")

            self.assertEqual(1, count)
            self.assertTrue((root / "site" / "0.9.1" / "index.html").is_file())
            self.assertTrue((root / "artifacts" / "threadpool-fp-docs-0.9.1.zip").is_file())

    def test_released_mode_reads_current_docs_and_assets_from_the_declared_tag(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.git(root, "init")
            self.git(root, "config", "user.email", "docs@example.invalid")
            self.git(root, "config", "user.name", "Documentation tests")
            self.write_release_source(root, "Released documentation reference.", "<svg>released-banner</svg>")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Release documentation")
            self.git(root, "tag", "v0.9.1")

            self.write_release_source(root, "Development documentation reference.", "<svg>development-banner</svg>")
            self.git(root, "add", ".")
            self.git(root, "commit", "-m", "Development documentation")

            released_site = root / "released-site"
            self.assertEqual(1, build_all(root, released_site, development_current=False))
            released_html = (released_site / "0.9.1" / "index.html").read_text(encoding="utf-8")
            released_banner = (released_site / "0.9.1" / "assets" / "homepage-banner.svg").read_text(encoding="utf-8")
            self.assertIn("Released documentation reference.", released_html)
            self.assertNotIn("Development documentation reference.", released_html)
            self.assertEqual("<svg>released-banner</svg>", released_banner)

            development_mode = root / "development-site"
            self.assertEqual(1, build_all(root, development_mode, development_current=True))
            development_html = (development_mode / "0.9.1" / "index.html").read_text(encoding="utf-8")
            development_banner = (development_mode / "0.9.1" / "assets" / "homepage-banner.svg").read_text(encoding="utf-8")
            self.assertIn("Development documentation reference.", development_html)
            self.assertEqual("<svg>development-banner</svg>", development_banner)


if __name__ == "__main__":
    unittest.main()