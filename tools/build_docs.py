#!/usr/bin/env python3
"""Build ThreadPool-FP Markdown documentation as a small, dependency-free site."""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import os
import re
import shutil
import zipfile
from dataclasses import dataclass
from pathlib import Path
from urllib.parse import quote, urlsplit


OUTPUT_MARKER = ".threadpool-fp-docs-output"
DOC_ASSETS = Path(__file__).resolve().parent / "docs_assets"
LINK_PATTERN = re.compile(r"(?<!!)\[([^\]]+)\]\(([^)]+)\)")
IMAGE_PATTERN = re.compile(r"!\[([^\]]*)\]\(([^)]+)\)")
HEADING_PATTERN = re.compile(r"^(#{1,6})\s+(.+?)\s*$")
FENCE_PATTERN = re.compile(r"^```([^`]*)\s*$")
LIST_PATTERN = re.compile(r"^\s*([-*+]|\d+\.)\s+(.+)$")
ADMONITION_PATTERN = re.compile(r"^\[!(NOTE|TIP|IMPORTANT|WARNING)\]\s*$", re.IGNORECASE)
SAFE_EXTERNAL_SCHEMES = {"http", "https", "mailto"}


@dataclass(frozen=True)
class SiteConfig:
    current: str
    release: str
    source_ref: str
    repository_url: str
    site_url: str
    versions: list[dict[str, str]]


@dataclass(frozen=True)
class NavigationPage:
    path: str
    title: str
    section: str


@dataclass(frozen=True)
class NavigationSection:
    title: str
    pages: tuple[NavigationPage, ...]


@dataclass(frozen=True)
class ProjectLink:
    title: str
    url: str | None = None
    project_path: str | None = None


@dataclass(frozen=True)
class HomepageBanner:
    project_path: str
    alt: str


@dataclass(frozen=True)
class DocumentationLayout:
    site_title: str
    description: str
    navigation: tuple[NavigationSection, ...]
    project_links: tuple[ProjectLink, ...]
    homepage: dict[str, object]
    banner: HomepageBanner | None = None
    legacy: bool = False

    @property
    def pages(self) -> tuple[NavigationPage, ...]:
        return tuple(page for section in self.navigation for page in section.pages)


@dataclass(frozen=True)
class RenderedDocument:
    body: str
    headings: tuple[tuple[int, str, str], ...]
    text: str


def load_config(versions_path: Path, release: str | None = None) -> SiteConfig:
    try:
        data = json.loads(versions_path.read_text(encoding="utf-8"))
        current = str(data["current"])
        versions = data["versions"]
        if not isinstance(versions, list) or not versions:
            raise ValueError("versions must be a non-empty list")
        selected_release = release or current
        entry = next((item for item in versions if item["release"] == selected_release), None)
        if entry is None:
            raise ValueError(f"release {selected_release!r} is not declared")
        if not isinstance(entry, dict):
            raise ValueError("current version must be an object")
        return SiteConfig(
            current=current,
            release=selected_release,
            source_ref=str(entry["source_ref"]),
            repository_url=str(data["repository_url"]).rstrip("/"),
            site_url=str(data["site_url"]).rstrip("/"),
            versions=[{"release": str(item["release"]), "source_ref": str(item["source_ref"])} for item in versions],
        )
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        raise ValueError(f"invalid version metadata {versions_path}: {exc}") from exc


def slug(value: str) -> str:
    value = re.sub(r"\[([^\]]+)\]\([^)]+\)", r"\1", value)
    value = re.sub(r"[`*_]", "", value).strip().lower()
    value = re.sub(r"[^a-z0-9]+", "-", value).strip("-")
    return value or "section"


def plain_markdown(value: str) -> str:
    return re.sub(r"[`*_]", "", LINK_PATTERN.sub(r"\1", value)).strip()


def unique_identifier(title: str, known: set[str]) -> str:
    base = slug(title)
    identifier = base
    suffix = 2
    while identifier in known:
        identifier = f"{base}-{suffix}"
        suffix += 1
    known.add(identifier)
    return identifier


def heading_entries(markdown: str) -> list[tuple[int, str, str]]:
    entries: list[tuple[int, str, str]] = []
    identifiers: set[str] = set()
    in_fence = False
    for line in markdown.splitlines():
        if FENCE_PATTERN.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        match = HEADING_PATTERN.match(line)
        if match:
            title = plain_markdown(match.group(2))
            entries.append((len(match.group(1)), title, unique_identifier(title, identifiers)))
    return entries


def markdown_anchors(path: Path) -> set[str]:
    return {identifier for _level, _title, identifier in heading_entries(path.read_text(encoding="utf-8"))}


def is_unsafe_url(target: str) -> bool:
    scheme = urlsplit(target).scheme.lower()
    return bool(scheme and scheme not in SAFE_EXTERNAL_SCHEMES)


def is_external(target: str) -> bool:
    parsed = urlsplit(target)
    return bool(parsed.scheme or parsed.netloc or target.startswith(("mailto:", "#")))


def split_target(target: str) -> tuple[str, str]:
    path, separator, fragment = target.partition("#")
    return path, fragment if separator else ""


def project_relative(candidate: Path, project_root: Path) -> str:
    return candidate.resolve().relative_to(project_root.resolve()).as_posix()


def relative_url(source: Path, target: Path) -> str:
    return os.path.relpath(target, source).replace(os.sep, "/")


def safe_document_path(value: object, label: str) -> str:
    if not isinstance(value, str) or not value or "\\" in value:
        raise ValueError(f"{label} must be a non-empty slash-separated path")
    path = Path(value)
    if path.is_absolute() or ".." in path.parts or path.suffix.lower() != ".md":
        raise ValueError(f"{label} must name a Markdown file within docs")
    return path.as_posix()


def safe_project_asset_path(value: object, label: str) -> str:
    if not isinstance(value, str) or not value or "\\" in value:
        raise ValueError(f"{label} must be a non-empty slash-separated path")
    path = Path(value)
    if path.is_absolute() or path.drive or ".." in path.parts or path.suffix.lower() != ".svg":
        raise ValueError(f"{label} must name an SVG file within the repository")
    return path.as_posix()


def legacy_navigation(source: Path) -> tuple[NavigationSection, ...]:
    grouped: dict[str, list[NavigationPage]] = {"Getting Started": [], "Guides": [], "Reference": []}
    for document in sorted(source.rglob("*.md")):
        relative = document.relative_to(source).as_posix()
        if relative == "index.md" or relative.startswith("start/"):
            section = "Getting Started"
        elif relative.startswith("guides/"):
            section = "Guides"
        elif relative.startswith("reference/"):
            section = "Reference"
        else:
            section = "Documentation"
            grouped.setdefault(section, [])
        title = next((title for level, title, _anchor in heading_entries(document.read_text(encoding="utf-8")) if level == 1), document.stem)
        grouped[section].append(NavigationPage(relative, title, section))
    return tuple(NavigationSection(title, tuple(pages)) for title, pages in grouped.items() if pages)


def load_layout(source: Path, config: SiteConfig) -> DocumentationLayout:
    layout_path = source / "layout.json"
    try:
        data = json.loads(layout_path.read_text(encoding="utf-8"))
        schema = data.get("schema_version")
        if schema == 1:
            if str(data.get("release")) != config.release:
                raise ValueError("release must match the selected version")
            required = data.get("required_pages", [])
            if not isinstance(required, list):
                raise ValueError("required_pages must be an array of paths")
            missing = [str(page) for page in required if not (source / str(page)).is_file()]
            if missing:
                raise ValueError(f"missing required documentation page(s): {', '.join(missing)}")
            return DocumentationLayout("ThreadPool-FP documentation", "Practical ThreadPool-FP documentation for Free Pascal and Lazarus.", legacy_navigation(source), tuple(), {}, legacy=True)
        if schema != 2:
            raise ValueError("schema_version must be 1 or 2")
        if str(data.get("release")) != config.release:
            raise ValueError("release must match the selected version")
        site_title = str(data.get("site_title", "")).strip()
        description = str(data.get("description", "")).strip()
        if not site_title or not description:
            raise ValueError("site_title and description are required")
        raw_navigation = data.get("navigation")
        if not isinstance(raw_navigation, list) or not raw_navigation:
            raise ValueError("navigation must be a non-empty array")
        navigation: list[NavigationSection] = []
        paths: set[str] = set()
        for section in raw_navigation:
            if not isinstance(section, dict) or not isinstance(section.get("title"), str):
                raise ValueError("each navigation section needs a title")
            title = section["title"].strip()
            raw_pages = section.get("pages")
            if not title or not isinstance(raw_pages, list) or not raw_pages:
                raise ValueError(f"navigation section {title!r} needs pages")
            pages: list[NavigationPage] = []
            for item in raw_pages:
                if not isinstance(item, dict) or not isinstance(item.get("title"), str):
                    raise ValueError(f"navigation section {title!r} has an invalid page")
                path = safe_document_path(item.get("path"), "navigation page path")
                if path in paths:
                    raise ValueError(f"navigation page appears more than once: {path}")
                if not (source / path).is_file():
                    raise ValueError(f"navigation page does not exist: {path}")
                paths.add(path)
                pages.append(NavigationPage(path, item["title"].strip(), title))
            navigation.append(NavigationSection(title, tuple(pages)))
        raw_hidden_pages = data.get("hidden_pages", [])
        if not isinstance(raw_hidden_pages, list):
            raise ValueError("hidden_pages must be an array of paths")
        hidden_paths = {safe_document_path(page, "hidden page") for page in raw_hidden_pages}
        duplicate_paths = sorted(paths & hidden_paths)
        if duplicate_paths:
            raise ValueError(f"page cannot be both navigated and hidden: {', '.join(duplicate_paths)}")
        missing_hidden = sorted(path for path in hidden_paths if not (source / path).is_file())
        if missing_hidden:
            raise ValueError(f"hidden page does not exist: {', '.join(missing_hidden)}")
        documents = {path.relative_to(source).as_posix() for path in source.rglob("*.md")}
        configured_paths = paths | hidden_paths
        if configured_paths != documents:
            missing = sorted(documents - configured_paths)
            extra = sorted(configured_paths - documents)
            detail = [f"missing page entries: {', '.join(missing)}" if missing else "", f"unknown page entries: {', '.join(extra)}" if extra else ""]
            raise ValueError("; ".join(item for item in detail if item))
        required = data.get("required_pages", [])
        if not isinstance(required, list):
            raise ValueError("required_pages must be an array of paths")
        required_paths = {safe_document_path(page, "required page") for page in required}
        if required_paths != configured_paths:
            raise ValueError("required_pages must match navigation and hidden pages")
        project_links: list[ProjectLink] = []
        for item in data.get("project", []):
            if not isinstance(item, dict) or not isinstance(item.get("title"), str):
                raise ValueError("project links need a title")
            url = item.get("url")
            project_path = item.get("project_path")
            if bool(url) == bool(project_path):
                raise ValueError("project links need exactly one of url or project_path")
            if url is not None and (not isinstance(url, str) or is_unsafe_url(url) or not is_external(url)):
                raise ValueError("project link url must be a safe absolute URL")
            if project_path is not None and (not isinstance(project_path, str) or not project_path or Path(project_path).is_absolute() or ".." in Path(project_path).parts):
                raise ValueError("project_path must stay within the repository")
            project_links.append(ProjectLink(item["title"].strip(), url, project_path))
        homepage = data.get("homepage", {})
        if not isinstance(homepage, dict):
            raise ValueError("homepage must be an object")
        banner = homepage.get("banner")
        homepage_banner = None
        if banner is not None:
            if not isinstance(banner, dict):
                raise ValueError("homepage banner must be an object")
            project_path = safe_project_asset_path(banner.get("project_path"), "homepage banner project_path")
            alt = banner.get("alt")
            if not isinstance(alt, str) or not alt.strip():
                raise ValueError("homepage banner alt must be a non-empty string")
            asset = (source.parent / project_path).resolve()
            try:
                asset.relative_to(source.parent.resolve())
            except ValueError as exc:
                raise ValueError("homepage banner project_path must stay within the repository") from exc
            if not asset.is_file():
                raise ValueError(f"homepage banner asset does not exist: {project_path}")
            homepage_banner = HomepageBanner(project_path, alt.strip())
        return DocumentationLayout(site_title, description, tuple(navigation), tuple(project_links), homepage, homepage_banner)
    except (OSError, TypeError, ValueError, json.JSONDecodeError) as exc:
        raise ValueError(f"invalid documentation layout {layout_path}: {exc}") from exc


def validate_source_links(source: Path, documents: list[Path], project_root: Path) -> None:
    document_set = {path.resolve() for path in documents}
    for document in documents:
        content = document.read_text(encoding="utf-8")
        targets = [*LINK_PATTERN.findall(content), *IMAGE_PATTERN.findall(content)]
        for _label, raw_target in targets:
            target = raw_target.strip()
            if is_unsafe_url(target):
                raise ValueError(f"unsafe link in {document}: {target}")
            if is_external(target):
                continue
            relative_path, fragment = split_target(target)
            candidate = (document.parent / relative_path).resolve() if relative_path else document.resolve()
            try:
                candidate.relative_to(project_root.resolve())
            except ValueError as exc:
                raise ValueError(f"broken internal link in {document}: {target}") from exc
            if not candidate.is_file():
                raise ValueError(f"broken internal link in {document}: {target}")
            if candidate.suffix.lower() == ".md" and candidate.is_relative_to(source.resolve()) and candidate not in document_set:
                raise ValueError(f"broken internal link in {document}: {target}")
            if fragment and candidate.suffix.lower() == ".md" and candidate.is_relative_to(source.resolve()) and fragment not in markdown_anchors(candidate):
                raise ValueError(f"broken internal link anchor in {document}: {target}")


def source_url(config: SiteConfig, project_path: str) -> str:
    return f"{config.repository_url}/blob/{quote(config.source_ref, safe='')}/{quote(project_path.replace(os.sep, '/'), safe='/')}"


def link_resolver(document: Path, html_page: Path, source: Path, output: Path, project_root: Path, config: SiteConfig):
    def resolve(raw_target: str) -> str:
        target = raw_target.strip()
        if is_unsafe_url(target):
            return "#"
        if is_external(target):
            return target
        relative_path, fragment = split_target(target)
        candidate = (document.parent / relative_path).resolve() if relative_path else document.resolve()
        if candidate.suffix.lower() == ".md" and candidate.is_relative_to(source.resolve()):
            href = relative_url(html_page.parent, output / candidate.relative_to(source).with_suffix(".html"))
        elif candidate == document.resolve() and not relative_path:
            href = ""
        else:
            href = source_url(config, project_relative(candidate, project_root))
        return href + (f"#{fragment}" if fragment else "")
    return resolve


def image_resolver(document: Path, html_page: Path, source: Path, output: Path):
    def resolve(raw_target: str) -> str:
        target = raw_target.strip()
        if is_unsafe_url(target):
            return ""
        if is_external(target):
            return target
        relative_path, _fragment = split_target(target)
        candidate = (document.parent / relative_path).resolve()
        if not candidate.is_relative_to(source.resolve()):
            raise ValueError(f"documentation image must remain within docs: {target}")
        return relative_url(html_page.parent, output / candidate.relative_to(source))
    return resolve


def render_inline_plain(text: str) -> str:
    result = html.escape(text)
    result = re.sub(r"`([^`]+)`", r"<code>\1</code>", result)
    result = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", result)
    return re.sub(r"(?<!\*)\*([^*]+)\*", r"<em>\1</em>", result)


def render_inline(text: str, resolve_link, resolve_image=None) -> str:
    tokens: list[str] = []
    def stash(value: str) -> str:
        tokens.append(value)
        return f"\x00{len(tokens) - 1}\x00"
    def render_link(match: re.Match[str]) -> str:
        label, target = match.groups()
        href = resolve_link(target)
        external_class = ' class="external-link"' if is_external(href) and not href.startswith("#") else ""
        return stash(f'<a{external_class} href="{html.escape(href, quote=True)}">{render_inline_plain(label)}</a>')
    def render_image(match: re.Match[str]) -> str:
        alt, target = match.groups()
        src = resolve_image(target) if resolve_image else target
        return stash(
            f'<img class="doc-image" src="{html.escape(src, quote=True)}" '
            f'alt="{html.escape(plain_markdown(alt), quote=True)}" loading="lazy">'
        )
    rendered = html.escape(
        IMAGE_PATTERN.sub(render_image, LINK_PATTERN.sub(render_link, text))
    )
    rendered = re.sub(r"`([^`]+)`", lambda match: f"<code>{html.escape(match.group(1))}</code>", rendered)
    rendered = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", rendered)
    rendered = re.sub(r"(?<!\*)\*([^*]+)\*", r"<em>\1</em>", rendered)
    for index, token in enumerate(tokens):
        rendered = rendered.replace(f"\x00{index}\x00", token)
    return rendered


def is_table_separator(line: str) -> bool:
    cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
    return bool(cells) and all(re.fullmatch(r":?-{3,}:?", cell) for cell in cells)


def table_cells(line: str, resolve_link, resolve_image) -> list[str]:
    return [render_inline(cell.strip(), resolve_link, resolve_image) for cell in line.strip().strip("|").split("|")]


def code_label(language: str) -> str:
    return {"pascal": "Pascal", "text": "Expected output", "output": "Expected output", "console": "Console"}.get(language, language.upper() if language else "Code")


def markdown_to_html(markdown: str, resolve_link, resolve_image=None) -> RenderedDocument:
    lines, chunks, search_text, paragraph = markdown.splitlines(), [], [], []
    index = 0
    heading_ids: set[str] = set()
    headings: list[tuple[int, str, str]] = []
    def flush_paragraph() -> None:
        if paragraph:
            raw = " ".join(paragraph)
            chunks.append(f"<p>{render_inline(raw, resolve_link, resolve_image)}</p>")
            search_text.append(plain_markdown(raw))
            paragraph.clear()
    while index < len(lines):
        line = lines[index]
        fence, heading, list_match = FENCE_PATTERN.match(line), HEADING_PATTERN.match(line), LIST_PATTERN.match(line)
        if fence:
            flush_paragraph()
            language = re.sub(r"[^a-z0-9_-]", "", fence.group(1).strip().lower())
            index += 1
            code: list[str] = []
            while index < len(lines) and not FENCE_PATTERN.match(lines[index]):
                code.append(lines[index]); index += 1
            if index == len(lines):
                raise ValueError("unclosed code fence")
            language_class = f' class="language-{html.escape(language, quote=True)}"' if language else ""
            kind = " code-output" if language in {"text", "output", "console"} else ""
            chunks.append(f'<div class="code-block{kind}" data-language="{html.escape(language, quote=True)}"><div class="code-toolbar"><span class="code-language">{html.escape(code_label(language))}</span><button class="copy-code" type="button" aria-label="Copy code to clipboard">Copy</button></div><pre><code{language_class}>{html.escape(chr(10).join(code))}</code></pre></div>')
            search_text.extend(code)
        elif heading:
            flush_paragraph()
            level, title = len(heading.group(1)), heading.group(2)
            text_title = plain_markdown(title)
            identifier = unique_identifier(text_title, heading_ids)
            headings.append((level, text_title, identifier)); search_text.append(text_title)
            anchor = f'<a class="heading-anchor" href="#{identifier}" aria-label="Link to {html.escape(text_title, quote=True)}">#</a>' if level >= 2 else ""
            chunks.append(f'<h{level} id="{identifier}"><span>{render_inline(title, resolve_link, resolve_image)}</span>{anchor}</h{level}>')
        elif line.strip().startswith("|") and index + 1 < len(lines) and is_table_separator(lines[index + 1]):
            flush_paragraph(); headers = table_cells(line, resolve_link, resolve_image); index += 2; rows: list[list[str]] = []
            while index < len(lines) and lines[index].strip().startswith("|"):
                rows.append(table_cells(lines[index], resolve_link, resolve_image)); search_text.extend(plain_markdown(cell) for cell in lines[index].strip().strip("|").split("|")); index += 1
            header_html = "".join(f'<th scope="col">{cell}</th>' for cell in headers)
            body_html = "".join("<tr>" + "".join(f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            chunks.append(f'<div class="table-wrap" tabindex="0" role="region" aria-label="Documentation table"><table><thead><tr>{header_html}</tr></thead><tbody>{body_html}</tbody></table></div>'); index -= 1
        elif line.lstrip().startswith(">"):
            flush_paragraph(); quoted: list[str] = []
            while index < len(lines) and lines[index].lstrip().startswith(">"):
                quoted.append(re.sub(r"^\s*>\s?", "", lines[index])); index += 1
            marker = ADMONITION_PATTERN.match(quoted[0].strip()) if quoted else None
            content = " ".join(item.strip() for item in quoted[1 if marker else 0:] if item.strip())
            if marker:
                kind = marker.group(1).lower()
                chunks.append(f'<aside class="admonition admonition-{kind}" role="note"><p class="admonition-title">{kind.title()}</p><p>{render_inline(content, resolve_link, resolve_image)}</p></aside>')
            else:
                chunks.append(f"<blockquote><p>{render_inline(content, resolve_link, resolve_image)}</p></blockquote>")
            search_text.append(plain_markdown(content)); index -= 1
        elif list_match:
            flush_paragraph(); ordered = list_match.group(1).endswith("."); tag = "ol" if ordered else "ul"; items: list[str] = []
            while index < len(lines):
                item_match = LIST_PATTERN.match(lines[index])
                if not item_match or item_match.group(1).endswith(".") != ordered:
                    break
                item = item_match.group(2); items.append(f"<li>{render_inline(item, resolve_link, resolve_image)}</li>"); search_text.append(plain_markdown(item)); index += 1
            chunks.append(f"<{tag}>" + "".join(items) + f"</{tag}>"); index -= 1
        elif not line.strip():
            flush_paragraph()
        else:
            paragraph.append(line.strip())
        index += 1
    flush_paragraph()
    return RenderedDocument("\n".join(chunks), tuple(headings), re.sub(r"\s+", " ", " ".join(search_text)).strip())


def nav_href(page: Path, output: Path, document_path: str) -> str:
    return relative_url(page.parent, output / Path(document_path).with_suffix(".html"))


def render_navigation(layout: DocumentationLayout, current_path: str, page: Path, output: Path, config: SiteConfig) -> str:
    sections: list[str] = []
    for section in layout.navigation:
        links = []
        for item in section.pages:
            current = ' aria-current="page"' if item.path == current_path else ""
            current_class = " is-current" if item.path == current_path else ""
            links.append(f'<li><a class="nav-link{current_class}"{current} href="{html.escape(nav_href(page, output, item.path), quote=True)}">{html.escape(item.title)}</a></li>')
        sections.append(f'<section class="sidebar-section"><h2>{html.escape(section.title)}</h2><ul>{"".join(links)}</ul></section>')
    if layout.project_links:
        links = []
        for item in layout.project_links:
            href = item.url if item.url else source_url(config, str(item.project_path))
            links.append(f'<li><a class="nav-link external-link" href="{html.escape(href, quote=True)}">{html.escape(item.title)}</a></li>')
        sections.append(f'<section class="sidebar-section"><h2>Project</h2><ul>{"".join(links)}</ul></section>')
    return f'<nav class="docs-navigation" aria-label="Documentation navigation">{"".join(sections)}</nav>'


def render_toc(headings: tuple[tuple[int, str, str], ...]) -> str:
    entries = [(level, title, identifier) for level, title, identifier in headings if level in {2, 3}]
    if len(entries) < 2:
        return ""
    items = "".join(f'<li class="toc-level-{level}"><a href="#{html.escape(identifier, quote=True)}">{html.escape(title)}</a></li>' for level, title, identifier in entries)
    return f'<nav class="on-page" aria-label="On this page"><p>On this page</p><ol>{items}</ol></nav>'


def render_breadcrumbs(item: NavigationPage | None, page: Path, output: Path) -> str:
    if item is None or item.path == "index.md":
        return ""
    root = html.escape(relative_url(page.parent, output / "index.html"), quote=True)
    return f'<nav class="breadcrumbs" aria-label="Breadcrumb"><ol><li><a href="{root}">Docs</a></li><li>{html.escape(item.section)}</li><li aria-current="page">{html.escape(item.title)}</li></ol></nav>'


def render_page_navigation(pages: tuple[NavigationPage, ...], current: NavigationPage | None, page: Path, output: Path) -> str:
    if current is None:
        return ""
    index = pages.index(current); previous = pages[index - 1] if index else None; following = pages[index + 1] if index + 1 < len(pages) else None
    if not previous and not following:
        return ""
    previous_html = f'<a class="previous-page" href="{html.escape(nav_href(page, output, previous.path), quote=True)}"><span>Previous</span><strong>← {html.escape(previous.title)}</strong></a>' if previous else ""
    next_html = f'<a class="next-page" href="{html.escape(nav_href(page, output, following.path), quote=True)}"><span>Next</span><strong>{html.escape(following.title)} →</strong></a>' if following else ""
    return f'<nav class="page-navigation" aria-label="Page navigation">{previous_html}{next_html}</nav>'


def homepage_content(layout: DocumentationLayout, page: Path, output: Path) -> str:
    tagline = html.escape(str(layout.homepage.get("tagline", layout.description)))
    actions, cards = [], []
    for action in layout.homepage.get("actions", []):
        if isinstance(action, dict) and isinstance(action.get("label"), str) and isinstance(action.get("path"), str):
            try:
                actions.append(f'<a class="button-link" href="{html.escape(nav_href(page, output, safe_document_path(action["path"], "homepage action")), quote=True)}">{html.escape(action["label"])}</a>')
            except ValueError:
                continue
    for card in layout.homepage.get("cards", []):
        if isinstance(card, dict) and all(isinstance(card.get(key), str) for key in ("eyebrow", "title", "description", "path")):
            try:
                href = nav_href(page, output, safe_document_path(card["path"], "homepage card"))
            except ValueError:
                continue
            cards.append(f'<a class="home-card" href="{html.escape(href, quote=True)}"><span>{html.escape(card["eyebrow"])}</span><strong>{html.escape(card["title"])}</strong><p>{html.escape(card["description"])}</p></a>')
    hero = f'<section class="home-hero"><p class="eyebrow">Documentation</p><h1>{html.escape(layout.site_title.replace(" documentation", ""))}</h1><p>{tagline}</p><div class="hero-actions">{"".join(actions)}</div></section>'
    cards_html = f'<section class="home-grid" aria-label="Documentation paths">{"".join(cards)}</section>' if cards else ""
    banner_html = ""
    if layout.banner:
        banner_url = relative_url(page.parent, output / "assets" / "homepage-banner.svg")
        banner_html = f'<figure class="homepage-banner"><img src="{html.escape(banner_url, quote=True)}" alt="{html.escape(layout.banner.alt, quote=True)}"></figure>'
    return hero + cards_html + banner_html


def remove_first_heading(body: str) -> str:
    return re.sub(r"^<h1\b[^>]*>.*?</h1>\n?", "", body, count=1, flags=re.DOTALL)


def page_shell(title: str, rendered: RenderedDocument, config: SiteConfig, layout: DocumentationLayout, current: NavigationPage | None, page: Path, output: Path, relative: str) -> str:
    stylesheet = relative_url(page.parent, output / "assets" / "site.css"); script = relative_url(page.parent, output / "assets" / "site.js"); search_script = relative_url(page.parent, output / "search-index.js")
    home = relative == "index.md"; body = remove_first_heading(rendered.body) if home else rendered.body
    if home:
        body = homepage_content(layout, page, output) + body
    navigation, toc = render_navigation(layout, relative, page, output, config), ("" if home else render_toc(rendered.headings))
    breadcrumbs, pagination = ("" if home else render_breadcrumbs(current, page, output)), render_page_navigation(layout.pages, current, page, output)
    root = relative_url(page.parent, output / "index.html")
    options = []
    for item in config.versions:
        release = item["release"]; label = f"v{release}" + (" (current)" if release == config.current else ""); selected = " selected" if release == config.release else ""
        options.append(f'<option value="{html.escape(relative_url(page.parent, output.parent / release / "index.html"), quote=True)}"{selected}>{html.escape(label)}</option>')
    canonical = f"{config.site_url}/{config.release}/{page.relative_to(output).as_posix()}"
    current_release = ' <span class="current-release">Current</span>' if config.release == config.current else ""
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><meta name="color-scheme" content="light dark">
<meta name="description" content="{html.escape(layout.description, quote=True)}"><meta name="threadpool-release" content="{html.escape(config.release, quote=True)}"><link rel="canonical" href="{html.escape(canonical, quote=True)}">
<title>{html.escape(title)} — ThreadPool-FP</title><script>try{{var t=localStorage.getItem('threadpool-theme');if(t==='light'||t==='dark')document.documentElement.dataset.theme=t}}catch(e){{}}</script><link rel="stylesheet" href="{html.escape(stylesheet, quote=True)}"></head>
<body data-doc-root="{html.escape(relative_url(page.parent, output), quote=True)}"><a class="skip-link" href="#content">Skip to content</a>
<header class="site-header"><div class="topbar"><a class="brand" href="{html.escape(root, quote=True)}"><span class="brand-mark" aria-hidden="true">T</span><span>ThreadPool-FP</span></a><nav class="top-links" aria-label="Primary"><a href="{html.escape(root, quote=True)}">Documentation</a><a class="external-link" href="{html.escape(config.repository_url, quote=True)}">GitHub</a></nav><div class="search-box"><label for="search">Search documentation</label><input id="search" type="search" placeholder="Search documentation" autocomplete="off" aria-controls="search-results" aria-expanded="false"><kbd aria-hidden="true">/</kbd></div><div class="header-actions"><label class="version-label" for="version-select">Version</label><select id="version-select" aria-label="Documentation version">{"".join(options)}</select><button id="theme-toggle" type="button" aria-label="Switch color theme" title="Switch color theme"><span aria-hidden="true">◐</span><span>Theme</span></button></div></div><div id="search-results" class="search-results" role="region" aria-label="Search results" aria-live="polite" hidden></div><details class="mobile-navigation"><summary>Browse documentation</summary>{navigation}</details></header>
<div class="doc-shell{' no-toc' if not toc else ''}"><aside class="doc-sidebar"><div class="sidebar-sticky">{navigation}</div></aside><main id="content" class="doc-content" tabindex="-1">{breadcrumbs}<article class="doc-prose{' homepage' if home else ''}">{body}</article>{pagination}<footer class="site-footer"><span>ThreadPool-FP v{html.escape(config.release)}{current_release}</span><span>Free Pascal / Lazarus</span><a class="external-link" href="{html.escape(config.repository_url, quote=True)}">GitHub</a><a class="external-link" href="{html.escape(source_url(config, 'LICENSE.md'), quote=True)}">MIT License</a></footer></main>{f'<aside class="doc-toc"><div class="toc-sticky">{toc}</div></aside>' if toc else ''}</div><script src="{html.escape(search_script, quote=True)}"></script><script src="{html.escape(script, quote=True)}"></script></body></html>\n"""


def prepare_output(output: Path, release: str) -> None:
    marker = output / OUTPUT_MARKER
    if output.exists() and any(output.iterdir()):
        if not marker.is_file():
            raise ValueError(f"refusing to replace unmarked documentation output: {output}")
        shutil.rmtree(output)
    output.mkdir(parents=True, exist_ok=True); marker.write_text(release + "\n", encoding="utf-8")


def copy_assets(output: Path, project_root: Path, layout: DocumentationLayout) -> None:
    assets = output / "assets"; assets.mkdir()
    for name in ("site.css", "site.js"):
        source = DOC_ASSETS / name
        if not source.is_file():
            raise ValueError(f"missing documentation asset: {source}")
        shutil.copy2(source, assets / name)
    if layout.banner:
        shutil.copy2(project_root / layout.banner.project_path, assets / "homepage-banner.svg")


def copy_document_assets(source: Path, output: Path) -> None:
    source_assets = source / "assets"
    if not source_assets.is_dir():
        return
    for source_asset in sorted(path for path in source_assets.rglob("*") if path.is_file()):
        target = output / source_asset.relative_to(source)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source_asset, target)


def write_landing_page(site_root: Path, config: SiteConfig) -> None:
    target = f"{config.current}/index.html"; site_root.mkdir(parents=True, exist_ok=True)
    (site_root / "index.html").write_text(f"""<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><meta http-equiv="refresh" content="0; url={target}"><title>ThreadPool-FP documentation</title></head><body><main><p>Opening <a href="{target}">ThreadPool-FP {html.escape(config.current)} documentation</a>.</p></main></body></html>\n""", encoding="utf-8")
    (site_root / ".nojekyll").write_text("", encoding="utf-8")
    (site_root / "versions.json").write_text(json.dumps({"schema_version": 1, "current": config.current, "versions": config.versions}, indent=2) + "\n", encoding="utf-8")


def write_offline_archive(site_root: Path, archive: Path, release: str) -> str:
    archive.parent.mkdir(parents=True, exist_ok=True); root_name = f"threadpool-fp-docs-{release}"
    with zipfile.ZipFile(archive, "w", compression=zipfile.ZIP_DEFLATED) as bundle:
        for path in sorted(path for path in site_root.rglob("*") if path.is_file()):
            if path.resolve() == archive.resolve() or path.name == OUTPUT_MARKER:
                continue
            info = zipfile.ZipInfo(f"{root_name}/{path.relative_to(site_root).as_posix()}"); info.date_time = (1980, 1, 1, 0, 0, 0); info.compress_type = zipfile.ZIP_DEFLATED; info.external_attr = 0o644 << 16; bundle.writestr(info, path.read_bytes())
    digest = hashlib.sha256(archive.read_bytes()).hexdigest(); archive.with_name(archive.name + ".sha256").write_text(f"{digest}  {archive.name}\n", encoding="ascii")
    return digest


def build_site(source: Path, output: Path, site_root: Path, versions_path: Path, offline_archive: Path | None = None, release: str | None = None) -> int:
    source, output, site_root = source.resolve(), output.resolve(), site_root.resolve()
    config = load_config(versions_path.resolve(), release); layout = load_layout(source, config)
    if output.name != config.release or output.parent != site_root:
        raise ValueError("versioned output must be site-root/<current release>")
    documents = sorted(source.rglob("*.md"))
    if not documents:
        raise ValueError(f"no Markdown documents found in {source}")
    validate_source_links(source, documents, source.parent); prepare_output(output, config.release); copy_assets(output, source.parent, layout); copy_document_assets(source, output)
    search_entries: list[dict[str, object]] = []; by_path = {item.path: item for item in layout.pages}
    for document in documents:
        relative = document.relative_to(source); relative_path = relative.as_posix(); page = output / relative.with_suffix(".html"); page.parent.mkdir(parents=True, exist_ok=True)
        rendered = markdown_to_html(
            document.read_text(encoding="utf-8"),
            link_resolver(document, page, source, output, source.parent, config),
            image_resolver(document, page, source, output),
        )
        fallback = by_path.get(relative_path, NavigationPage(relative_path, relative.stem, "Documentation"))
        title = next((text for level, text, _identifier in rendered.headings if level == 1), fallback.title)
        page.write_text(page_shell(title, rendered, config, layout, by_path.get(relative_path), page, output, relative_path), encoding="utf-8")
        item = by_path.get(relative_path)
        search_entries.append({"title": title, "section": item.section if item else "Documentation", "headings": [text for level, text, _identifier in rendered.headings if level >= 2], "url": relative.with_suffix(".html").as_posix(), "text": rendered.text})
    search_json = json.dumps(search_entries, ensure_ascii=False, separators=(",", ":"))
    (output / "search-index.json").write_text(json.dumps(search_entries, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    (output / "search-index.js").write_text("globalThis.ThreadPoolSearchIndex=" + search_json.replace("<", "\\u003c").replace(">", "\\u003e").replace("&", "\\u0026") + ";\n", encoding="utf-8")
    (output / "release.json").write_text(json.dumps({"schema_version": 2, "release": config.release, "source_ref": config.source_ref, "page_count": len(documents)}, indent=2) + "\n", encoding="utf-8")
    write_landing_page(site_root, config)
    if offline_archive:
        write_offline_archive(site_root, offline_archive.resolve(), config.release)
    print(f"Built {len(documents)} ThreadPool-FP documentation pages in {output}")
    return len(documents)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__); parser.add_argument("--source", type=Path, default=Path("docs")); parser.add_argument("--versions", type=Path, default=Path("docs/versions.json")); parser.add_argument("--output", type=Path); parser.add_argument("--site-root", type=Path); parser.add_argument("--offline-archive", type=Path); parser.add_argument("--release", help="build this release from its matching documentation source")
    args = parser.parse_args(); config = load_config(args.versions.resolve(), args.release); site_root = args.site_root or Path("build/docs-site"); output = args.output or site_root / config.release
    build_site(args.source, output, site_root, args.versions, args.offline_archive, args.release)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())