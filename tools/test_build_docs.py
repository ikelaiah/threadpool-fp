#!/usr/bin/env python3
"""Regression tests for the lightweight documentation publisher."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from build_docs import build_site  # noqa: E402


class BuildDocsTests(unittest.TestCase):
    def write_fixture(self, root: Path) -> tuple[Path, Path, Path]:
        source = root / "docs"
        output = root / "site" / "0.9.1"
        site_root = output.parent
        (source / "start").mkdir(parents=True)
        (root / "assets").mkdir()
        (root / "assets" / "banner.svg").write_text(
            '<svg xmlns="http://www.w3.org/2000/svg"><title>ThreadPool-FP</title></svg>\n',
            encoding="utf-8",
        )
        (source / "index.md").write_text(
            "# ThreadPool-FP documentation\n\n"
            "Start with the [guide](start/guide.md).\n",
            encoding="utf-8",
        )
        (source / "start" / "guide.md").write_text(
            "# A tiny guide\n\n"
            "> [!NOTE]\n"
            "> Callbacks run concurrently.\n\n"
            "## Repeat\n\n"
            "```pascal\n"
            "GlobalThreadPool.WaitForAll;\n"
            "```\n\n"
            "### Details\n\n"
            "The call waits.\n\n"
            "## Repeat\n\n"
            "The stable duplicate heading uses a distinct anchor.\n",
            encoding="utf-8",
        )
        (source / "hidden.md").write_text(
            "# Hidden release note\n\n"
            "[Source guide](start/guide.md)\n",
            encoding="utf-8",
        )
        (source / "layout.json").write_text(
            json.dumps(
                {
                    "schema_version": 2,
                    "release": "0.9.1",
                    "site_title": "ThreadPool-FP documentation",
                    "description": "Practical ThreadPool-FP documentation.",
                    "required_pages": ["index.md", "start/guide.md", "hidden.md"],
                    "navigation": [
                        {
                            "title": "Getting Started",
                            "pages": [
                                {"path": "index.md", "title": "Introduction"},
                                {"path": "start/guide.md", "title": "Beginner Guide"},
                            ],
                        }
                    ],
                    "hidden_pages": ["hidden.md"],
                    "project": [{"title": "GitHub repository", "url": "https://github.com/example/threadpool-fp"}],
                    "homepage": {
                        "tagline": "Parallel work for Free Pascal.",
                        "banner": {
                            "project_path": "assets/banner.svg",
                            "alt": "ThreadPool-FP banner",
                        },
                        "actions": [{"label": "Get Started", "path": "start/guide.md"}],
                    },
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
        return source, output, site_root

    def test_builds_documentation_shell_navigation_and_pascal_code(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            guide = (output / "start" / "guide.html").read_text(encoding="utf-8")
            landing = (site_root / "index.html").read_text(encoding="utf-8")
            self.assertIn('href="start/guide.html"', index)
            self.assertIn('class="doc-sidebar"', guide)
            self.assertIn('aria-label="Breadcrumb"', guide)
            self.assertIn('class="page-navigation"', guide)
            self.assertIn('class="on-page"', guide)
            self.assertIn('class="copy-code"', guide)
            self.assertIn('class="admonition admonition-note"', guide)
            self.assertIn('class="heading-anchor"', guide)
            self.assertIn('id="repeat-2"', guide)
            self.assertIn('id="version-select"', guide)
            self.assertIn('class="homepage-banner"', index)
            self.assertIn('src="assets/homepage-banner.svg"', index)
            self.assertIn('alt="ThreadPool-FP banner"', index)
            self.assertIn('<pre><code class="language-pascal">', guide)
            self.assertTrue((output / "assets" / "site.css").is_file())
            self.assertTrue((output / "assets" / "site.js").is_file())
            self.assertEqual(
                (source.parent / "assets" / "banner.svg").read_bytes(),
                (output / "assets" / "homepage-banner.svg").read_bytes(),
            )
            self.assertTrue((output / "search-index.json").is_file())
            self.assertTrue((output / "search-index.js").is_file())
            stylesheet = (output / "assets" / "site.css").read_text(encoding="utf-8")
            self.assertIn(':root[data-theme="dark"]', stylesheet)
            self.assertIn(".homepage-banner {", stylesheet)
            self.assertIn("ThreadPoolSearchIndex", (output / "assets" / "site.js").read_text(encoding="utf-8"))
            entries = {entry["url"]: entry for entry in json.loads((output / "search-index.json").read_text(encoding="utf-8"))}
            self.assertEqual("Getting Started", entries["start/guide.html"]["section"])
            self.assertEqual("Documentation", entries["hidden.html"]["section"])
            self.assertTrue((output / "hidden.html").is_file())
            self.assertNotIn("hidden.html", (output / "index.html").read_text(encoding="utf-8"))
            self.assertIn("ThreadPool-FP documentation", landing)

    def test_rejects_a_broken_internal_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "index.md").write_text(
                "# ThreadPool-FP documentation\n\n[Missing](missing.md)\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "broken internal link"):
                build_site(source, output, site_root, source / "versions.json")

    def test_rejects_an_unsafe_markdown_url(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            (source / "index.md").write_text(
                "# ThreadPool-FP documentation\n\n[Unsafe](javascript:alert(1))\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(ValueError, "unsafe link"):
                build_site(source, output, site_root, source / "versions.json")

    def test_rejects_a_missing_homepage_banner_asset(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))
            layout_path = source / "layout.json"
            layout = json.loads(layout_path.read_text(encoding="utf-8"))
            layout["homepage"]["banner"]["project_path"] = "assets/missing.svg"
            layout_path.write_text(json.dumps(layout), encoding="utf-8")

            with self.assertRaisesRegex(ValueError, "homepage banner asset does not exist"):
                build_site(source, output, site_root, source / "versions.json")

    def test_links_project_markdown_to_its_repository_source(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, output, site_root = self.write_fixture(root)
            (root / "CHANGELOG.md").write_text("# Changelog\n", encoding="utf-8")
            (source / "index.md").write_text(
                "# ThreadPool-FP documentation\n\n[Changelog](../CHANGELOG.md#changelog)\n",
                encoding="utf-8",
            )

            build_site(source, output, site_root, source / "versions.json")

            index = (output / "index.html").read_text(encoding="utf-8")
            self.assertIn("https://github.com/example/threadpool-fp/blob/main/CHANGELOG.md#changelog", index)

    def test_builds_a_preserved_release_from_its_own_source(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source, _output, site_root = self.write_fixture(root)
            (source / "layout.json").write_text(
                json.dumps({"schema_version": 1, "release": "0.9.0"}),
                encoding="utf-8",
            )
            versions = source / "versions.json"
            metadata = json.loads(versions.read_text(encoding="utf-8"))
            metadata["versions"].append({"release": "0.9.0", "source_ref": "v0.9.0"})
            versions.write_text(json.dumps(metadata), encoding="utf-8")
            output = site_root / "0.9.0"

            build_site(source, output, site_root, versions, release="0.9.0")

            release = json.loads((output / "release.json").read_text(encoding="utf-8"))
            self.assertEqual("0.9.0", release["release"])

    def test_rejects_an_undeclared_selected_release(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            source, output, site_root = self.write_fixture(Path(directory))

            with self.assertRaisesRegex(ValueError, "not declared"):
                build_site(source, output, site_root, source / "versions.json", release="2.0.0")


if __name__ == "__main__":
    unittest.main()