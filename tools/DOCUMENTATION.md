# Maintaining the ThreadPool-FP documentation site

The static publisher is intentionally dependency-free. It reads the Markdown
under `docs/`, produces a versioned GitHub Pages artifact, and packages an
offline copy.

## Layout

- `docs/` — the Markdown source. Every `.md` file here is either a first-class
  navigation page or a hidden page (still generated, indexed, and reachable,
  but not in the sidebar).
- `docs/layout.json` — the curated documentation tree. It declares the site
  identity, navigation sections, hidden pages, project links, and the
  homepage hero/cards/banner. The builder rejects layouts that do not cover
  every Markdown file in `docs/`.
- `docs/versions.json` — version metadata: the current release, the site and
  repository URLs, and the ordered list of published documentation releases.
- `tools/build_docs.py` — Markdown-to-HTML builder and link/layout validation.
- `tools/build_all_docs.py` — builds every release declared in
  `docs/versions.json` from its source ref, then writes the landing page and
  the offline ZIP.
- `tools/check_docs.py` — verifies that recipe code blocks in
  `docs/guides/recipes.md` exactly match their source programs.
- `tools/check_built_docs.py` — validates generated HTML: internal links,
  anchors, duplicate ids, release metadata, search index, and assets.
- `tools/docs_assets/` — the shared `site.css` and `site.js` shipped into every
  built release.
- `tools/test_*.py` — regression tests for the tooling.

## Add or reorganise a page

1. Add the Markdown page under `docs/`.
2. Add its path to `required_pages` in `docs/layout.json`.
3. Add it to a `navigation` section for a primary page, or to `hidden_pages`
   when keeping an existing URL without placing it in the main sidebar.
4. Update cross-links in relevant pages (internal links are validated at build
   time, so a broken link fails the build).
5. Run the tooling checks below.

`hidden_pages` are still generated and indexed so existing bookmarks remain
useful; they are simply not first-class navigation. Historical material such
as release notes, archived plans, and decision records should land there.

## Add a documentation recipes program

1. Add a self-contained program under `examples/documentation/` with a
   matching `<name>.output` file containing its deterministic stdout.
2. Build and run it (`python tools/test_docs_examples.py`) so the `.output`
   file is correct, and run it a few times to confirm the output is stable.
3. Add a section to `docs/guides/recipes.md` whose Pascal block is
   byte-identical to the program and ends with a
   `[Source program](../../examples/documentation/<name>.pas)` link.
4. `tools/check_docs.py` fails if the block and source drift apart.

## Add a version

Add a `release` and `source_ref` entry to `docs/versions.json`, and update
`current`. A release is only published from this file after its `source_ref`
actually contains the compatible documentation source tree (including
`layout.json` and `versions.json`). Because ThreadPool-FP's documentation
source was introduced with the 0.9.1 web documentation, earlier tags cannot
yet be exposed as historical documentation; re-tagging is not used to fake
history.

`build_all_docs.py` builds the declared current release from the checkout by
default (development preview); pass `--released` to build every version,
including current, from its immutable `source_ref` for published output.

## Local verification

```text
python tools/test_build_docs.py
python tools/test_build_all_docs.py
python tools/test_check_built_docs.py
python tools/test_check_docs.py
python tools/check_docs.py
python tools/test_docs_examples.py
python tools/build_all_docs.py --site-root site --offline-dir artifacts
python tools/check_built_docs.py --site site
```

The resulting archive is `artifacts/threadpool-fp-docs-<version>.zip`, with a
matching `.sha256` checksum. Open `site/index.html` through a local web server
when visually checking the site (the asset and search paths are relative, so
a plain double-click also works).