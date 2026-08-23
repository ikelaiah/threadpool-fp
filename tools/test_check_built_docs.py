#!/usr/bin/env python3
"""Regression tests for generated ThreadPool-FP documentation validation."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from build_docs import build_site  # noqa: E402
from check_built_docs import check_site  # noqa: E402


class CheckBuiltDocsTests(unittest.TestCase):
    def build_fixture(self, root: Path) -> Path:
        source = root / "docs"
        output = root / "site" / "0.9.1"
        source.mkdir()
        (source / "index.md").write_text("# Index\n\n[Guide](guide.md)\n", encoding="utf-8")
        (source / "guide.md").write_text("# Guide\n\nAll good.\n", encoding="utf-8")
        (source / "hidden.md").write_text("# Hidden\n\n[Guide](guide.md)\n", encoding="utf-8")
        (source / "layout.json").write_text(
            json.dumps(
                {
                    "schema_version": 2,
                    "release": "0.9.1",
                    "site_title": "ThreadPool-FP documentation",
                    "description": "Fixture site.",
                    "required_pages": ["index.md", "guide.md", "hidden.md"],
                    "navigation": [{"title": "Getting Started", "pages": [{"path": "index.md", "title": "Introduction"}, {"path": "guide.md", "title": "Guide"}]}],
                    "hidden_pages": ["hidden.md"],
                }
            ),
            encoding="utf-8",
        )
        versions = source / "versions.json"
        versions.write_text(
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
        build_site(source, output, output.parent, versions)
        return output.parent

    def test_accepts_a_complete_versioned_site(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            self.assertEqual([], check_site(self.build_fixture(Path(directory))))

    def test_reports_a_missing_generated_link_target(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            page = site / "0.9.1" / "index.html"
            page.write_text(page.read_text(encoding="utf-8").replace('guide.html', 'missing.html'), encoding="utf-8")
            self.assertTrue(any("missing link target" in error for error in check_site(site)))

    def test_reports_duplicate_ids_and_unsafe_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            page = site / "0.9.1" / "guide.html"
            page.write_text(
                page.read_text(encoding="utf-8").replace(
                    "</main>",
                    '<p id="guide">Duplicate identifier</p><a href="javascript:alert(1)">Unsafe</a></main>',
                ),
                encoding="utf-8",
            )
            errors = check_site(site)
            self.assertTrue(any("duplicate id" in error for error in errors))
            self.assertTrue(any("unsafe link" in error for error in errors))

    def test_requires_the_documentation_assets_and_version_targets(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            (site / "0.9.1" / "assets" / "site.js").unlink()
            errors = check_site(site)
            self.assertTrue(any("missing required asset" in error for error in errors))

    def test_rejects_a_wrong_release_metadata_value(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            site = self.build_fixture(Path(directory))
            page = site / "0.9.1" / "guide.html"
            page.write_text(
                page.read_text(encoding="utf-8").replace('content="0.9.1"', 'content="9.9.9"'),
                encoding="utf-8",
            )
            errors = check_site(site)
            self.assertTrue(any("release metadata" in error for error in errors))


if __name__ == "__main__":
    unittest.main()