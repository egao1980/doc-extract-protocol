# doc-extract-protocol

Lispy **CLOS** document extraction for [cl-stack](https://github.com/egao1980/cl-stack). Version **0.3** reads markdown through `markdown-protocol` and keeps inline spans. Version **0.2** added the canonical `extracted-document` block tree (`demiurge-doc/1`).

| System | Role | Repo |
|--------|------|------|
| `doc-extract-protocol` (`stack-doc-extract`) | Protocol + HTML / DOCX / XLSX + C3d model | this repo |
| `doc-extract-backend-pdf` | pdfium CFFI overlay | [`egao1980/doc-extract-backend-pdf`](https://github.com/egao1980/doc-extract-backend-pdf) |

```lisp
(asdf:load-system "doc-extract-protocol")

(let ((b (stack-doc-extract:make-default-doc-extract-backend)))
  (stack-doc-extract:extract-document b
    "<html><h1>Hi</h1><p>there</p></html>" :format :html))
```

| GF | Result |
|----|--------|
| `extract-document` | `extracted-document` (primary, 0.2) |
| `extract-text` | plain string |
| `extract-metadata` | plist (`:title` `:format` …) |
| `extract-sections` | list of `{title, text, level}` (`extracted-section`) |

## Canonical model (`demiurge-doc/1`)

Every backend normalizes into a typed block tree (`schema-protocol` `defschema`):

- Uniform envelope: `id`, `provenance`, `attrs` (`:role`, `:flow`), `layer` (`:body` / `:furniture`), `confidence`
- Blocks: `section`, `text-block`, `list-block`, `code-block`, `table-block`, `image-block`, `key-value-block`
- Offsets are Unicode **code-point** indices into the owning block's text
- `(document-text doc &key layer)` is derived only — never stored
- Lossless interchange: `dump-document` / `parse-extracted-document` (JSON data model) and `document->sexp` / `sexp->document`
- Markup: `dump-markup` / `parse-markup` — CommonMark + `<!-- {#id …} -->` (lossy-tolerant). Markdown is read by [`markdown-protocol`](https://github.com/egao1980/markdown-protocol); emphasis, strong, strike, code spans and links become `annotation-span`s (`:emphasis` `:strong` `:strike` `:code` `:link` with the url as `target`) over the block's plain text, and `dump-markup` writes them back as markdown. `inlines-text-spans` maps markdown-protocol inlines to (text, spans)
- Fidelity: `document-equivalent-p` at `:text`, `:structure`, `:strict`
- Ids: `compute-block-id` / `ensure-ids` — portable SHA-256, override via `*block-digest-fn*`

`sections->document` upgrades 0.1 section lists. Chunker helper: `split-block-tree` (stamps `:block-id` + `:section-path`; skips furniture).

## Extractor registry (C3c)

`(register-extractor backend &key formats priority)` / `(find-extractor format)`. Missing format signals `no-extractor-for-format` with restart `use-backend`. Colocated HTML and office backends self-register at load.

HTML uses `html-protocol` when `*html-backend*` is bound; otherwise a tag-strip + heading heuristic. DOCX/XLSX use `compression-protocol` zip when loaded; otherwise pass unzipped XML as `:format :docx-xml` / `:xlsx-xml`. PDF is not implemented here.

Conditions: `doc-extract-error`, `doc-extract-unsupported`, `no-extractor-for-format`. Restarts: `use-value`, `retry`, `use-backend`.

## License

MIT — see [LICENSE](LICENSE).
