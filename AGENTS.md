# AGENTS.md

## Translation Protocol

When translating posts, translate the full article unless the user explicitly asks for an abridged version.

### Zola Layout

- Chinese is the default language.
- Use Zola's language suffix convention:
  - `content/posts/foo.md` -> `content/posts/foo.en.md`
  - `content/posts/foo/index.md` -> `content/posts/foo/index.en.md`
  - `content/posts/_index.md` -> `content/posts/_index.en.md`
- English posts should build under `/en/posts/...`.
- Chinese RSS and English RSS must remain separate.

### Content Rules

- Preserve front matter semantics: keep `date`, `updated`, `taxonomies`, `extra.math`, and `extra.comment`; translate `title` and `extra.headline`.
- Add a machine-translation disclaimer near the top unless instructed otherwise.
- Preserve every heading, code block, math block, diagram, image, shortcode, footnote, aside, table, and meaningful link.
- Do not silently summarize, omit, or compress sections.
- Keep failed attempts, dead ends, intermediate code snippets, appendices, and diagrams; they are often the point of technical posts.
- If exact translation of a quoted passage is not possible, mark that explicitly in the translated article and preserve as much context as allowed.

### Voice Rules

- Do not polish away intentional roughness.
- Preserve jokes, memes, internet slang, deliberate typos, messy punctuation, copied prompts, and register shifts.
- Use a culturally similar English expression when a literal translation loses the effect.
  - Example: `布兑` should become something meme-like such as `Wait, wat?!`, not plain `Wrong.`
- Raw prompts should remain prompt-like; do not rewrite them as polished prose.

### Code Fences

- Do not remove language tags just to silence Zola warnings.
- If the exact language is unsupported by Zola/syntect, choose the closest supported highlighter.
- Example: Pie/Scheme-like snippets can use `lisp` if `scheme` is unsupported.

## Required Audit

Before reporting completion, compare each source post with its translation:

```bash
for f in content/posts/ORIGINALS...; do
  en=${f%.md}.en.md
  case $f in */index.md) en=${f%/index.md}/index.en.md;; esac
  printf '%s -> %s\n' "$f" "$en"
  printf 'headings '; rg -c '^#{1,6} ' "$f" "$en"
  printf 'fences '; rg -c '^```' "$f" "$en"
  printf 'images '; rg -c '!\[' "$f" "$en"
  printf 'links '; rg -c '\[[^]]+\]\(' "$f" "$en"
  printf 'footnotes '; rg -c '^\[\^' "$f" "$en"
  printf 'mathdisplay '; rg -c '^\$\$' "$f" "$en"
done
```

Treat mismatches as suspicious until manually explained.

Inspect headings directly when needed:

```bash
rg -n '^#{1,6} ' content/posts/source.md content/posts/source.en.md
```

Build before final response:

```bash
zola build --output-dir /tmp/blog-public-clean --force
```

If `zola check` fails only because external links cannot be reached, report that explicitly.
