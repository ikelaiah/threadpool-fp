#!/usr/bin/env python3
"""Regression tests for documentation source checks."""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path


TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

from check_docs import check_recipe_examples  # noqa: E402


class CheckDocsTests(unittest.TestCase):
    def test_accepts_a_recipe_block_that_matches_its_source_program(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            example = root / "examples" / "documentation" / "hello.pas"
            example.parent.mkdir(parents=True)
            program = "program Hello;\nbegin\nend.\n"
            example.write_text(program, encoding="utf-8")
            recipe = root / "docs" / "guides" / "recipes.md"
            recipe.parent.mkdir(parents=True)
            recipe.write_text(
                "## Hello\n\n```pascal\n" + program + "```\n\n"
                "[Source program](../../examples/documentation/hello.pas)\n",
                encoding="utf-8",
            )
            self.assertEqual([], check_recipe_examples(recipe, root))

    def test_reports_recipe_code_that_has_drifted_from_its_source_program(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            example = root / "examples" / "documentation" / "hello.pas"
            example.parent.mkdir(parents=True)
            example.write_text("program Hello;\nbegin\nend.\n", encoding="utf-8")
            recipe = root / "docs" / "start" / "recipes.md"
            recipe.parent.mkdir(parents=True)
            recipe.write_text(
                "## Hello\n\n```pascal\nprogram Hello;\nbegin\n  Writeln('drift');\nend.\n```\n\n"
                "[Source program](../../examples/documentation/hello.pas)\n",
                encoding="utf-8",
            )
            self.assertTrue(any("does not match" in error for error in check_recipe_examples(recipe, root)))


if __name__ == "__main__":
    unittest.main()