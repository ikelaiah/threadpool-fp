#!/usr/bin/env python3
"""Check that recipe code blocks remain identical to their compiled programs."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


SECTION_PATTERN = re.compile(r"^##\s+.*?(?=^##\s+|\Z)", re.MULTILINE | re.DOTALL)
PASCAL_BLOCK_PATTERN = re.compile(r"```pascal\n(.*?)```", re.DOTALL)
SOURCE_PATTERN = re.compile(r"\[Source program\]\(([^)]+\.pas)\)")


def normalise(text: str) -> str:
    return text.replace("\r\n", "\n")


def check_recipe_examples(recipe: Path, root: Path) -> list[str]:
    recipe = recipe.resolve()
    root = root.resolve()
    errors: list[str] = []
    content = recipe.read_text(encoding="utf-8")
    sections = SECTION_PATTERN.findall(content)
    if not sections:
        return [f"{recipe}: contains no recipe sections"]
    for section in sections:
        blocks = PASCAL_BLOCK_PATTERN.findall(section)
        sources = SOURCE_PATTERN.findall(section)
        if not blocks and not sources:
            continue
        heading = section.splitlines()[0]
        if len(blocks) != len(sources):
            errors.append(f"{recipe}: {heading} has {len(blocks)} Pascal block(s) and {len(sources)} source link(s)")
            continue
        for block, source_link in zip(blocks, sources):
            source = (recipe.parent / source_link).resolve()
            try:
                source.relative_to(root)
            except ValueError:
                errors.append(f"{recipe}: {heading} source escapes repository: {source_link}")
                continue
            if not source.is_file():
                errors.append(f"{recipe}: {heading} source program is missing: {source_link}")
                continue
            if normalise(block) != normalise(source.read_text(encoding="utf-8")):
                errors.append(f"{recipe}: {heading} code block does not match {source_link}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--recipes", type=Path, default=Path("docs/guides/recipes.md"))
    args = parser.parse_args()
    root = args.root.resolve()
    recipe = args.recipes.resolve() if args.recipes else root / "docs" / "guides" / "recipes.md"
    errors = check_recipe_examples(recipe, root)
    if errors:
        print("\n".join(errors))
        return 1
    print(f"Recipe source checks passed: {recipe}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())