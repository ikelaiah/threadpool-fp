(() => {
  "use strict";

  const search = document.querySelector("#search");
  const results = document.querySelector("#search-results");
  const root = document.body.dataset.docRoot || "";
  const themeToggle = document.querySelector("#theme-toggle");
  const versionSelect = document.querySelector("#version-select");
  const items = Array.isArray(globalThis.ThreadPoolSearchIndex) ? globalThis.ThreadPoolSearchIndex : [];
  const colorPreference = window.matchMedia("(prefers-color-scheme: dark)");

  function storedTheme() {
    try {
      const value = localStorage.getItem("threadpool-theme");
      return value === "light" || value === "dark" ? value : null;
    } catch (_error) {
      return null;
    }
  }

  function currentTheme() {
    return document.documentElement.dataset.theme || (colorPreference.matches ? "dark" : "light");
  }

  function updateThemeControl() {
    const next = currentTheme() === "dark" ? "light" : "dark";
    themeToggle?.setAttribute("aria-label", `Switch to ${next} theme`);
    themeToggle?.setAttribute("title", `Switch to ${next} theme`);
    const icon = themeToggle?.querySelector("span[aria-hidden='true']");
    if (icon) icon.textContent = currentTheme() === "dark" ? "☀" : "☾";
  }

  updateThemeControl();
  themeToggle?.addEventListener("click", () => {
    const next = currentTheme() === "dark" ? "light" : "dark";
    document.documentElement.dataset.theme = next;
    try { localStorage.setItem("threadpool-theme", next); } catch (_error) { /* Session-only theme is still useful. */ }
    updateThemeControl();
  });
  colorPreference.addEventListener?.("change", () => { if (!storedTheme()) updateThemeControl(); });

  versionSelect?.addEventListener("change", () => { window.location.assign(versionSelect.value); });

  function closeResults() {
    if (!results || !search) return;
    results.hidden = true;
    results.replaceChildren();
    search.setAttribute("aria-expanded", "false");
  }

  function score(item, query) {
    const title = String(item.title || "").toLowerCase();
    const headings = Array.isArray(item.headings) ? item.headings.join(" ").toLowerCase() : "";
    const body = String(item.text || "").toLowerCase();
    if (title.includes(query)) return 3;
    if (headings.includes(query)) return 2;
    return body.includes(query) ? 1 : 0;
  }

  function renderResults() {
    if (!search || !results) return;
    const query = search.value.trim().toLowerCase();
    if (!query) return closeResults();
    const matches = items.map((item) => ({ item, score: score(item, query) }))
      .filter((match) => match.score > 0)
      .sort((a, b) => b.score - a.score || String(a.item.title).localeCompare(String(b.item.title)))
      .slice(0, 12);
    results.replaceChildren();
    if (matches.length) {
      const list = document.createElement("ul");
      for (const { item } of matches) {
        const row = document.createElement("li");
        const link = document.createElement("a");
        link.href = `${root}/${item.url}`.replace(/^\.\//, "");
        const title = document.createElement("strong");
        title.textContent = item.title;
        const detail = document.createElement("span");
        detail.textContent = item.section || "Documentation";
        link.append(title, detail);
        row.append(link);
        list.append(row);
      }
      results.append(list);
    } else {
      const empty = document.createElement("p");
      empty.className = "search-empty";
      empty.textContent = `No documentation found for “${search.value.trim()}”.`;
      results.append(empty);
    }
    results.hidden = false;
    search.setAttribute("aria-expanded", "true");
  }

  search?.addEventListener("input", renderResults);
  search?.addEventListener("keydown", (event) => {
    if (event.key === "Escape") { closeResults(); search.blur(); }
  });
  document.addEventListener("keydown", (event) => {
    const target = event.target;
    const typing = target instanceof HTMLInputElement || target instanceof HTMLTextAreaElement || target?.isContentEditable;
    if (event.key === "/" && !typing) { event.preventDefault(); search?.focus(); }
  });
  document.addEventListener("click", (event) => {
    if (results && search && !results.contains(event.target) && event.target !== search) closeResults();
  });

  function fallbackCopy(text) {
    const area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.append(area);
    area.select();
    const copied = document.execCommand("copy");
    area.remove();
    if (!copied) throw new Error("Clipboard copy was unavailable.");
  }

  document.querySelectorAll(".copy-code").forEach((button) => {
    button.addEventListener("click", async () => {
      const code = button.closest(".code-block")?.querySelector("code");
      if (!code) return;
      const original = button.textContent;
      try {
        const text = code.textContent || "";
        if (navigator.clipboard?.writeText) {
          await navigator.clipboard.writeText(text);
        } else {
          fallbackCopy(text);
        }
        button.textContent = "Copied";
      } catch (_error) {
        try {
          fallbackCopy(code.textContent || "");
          button.textContent = "Copied";
        } catch (_fallbackError) {
          button.textContent = "Copy failed";
        }
      }
      window.setTimeout(() => { button.textContent = original; }, 1600);
    });
  });
})();