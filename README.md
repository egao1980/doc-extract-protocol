# doc-extract-protocol

Lispy **CLOS** document extraction for [cl-stack](https://github.com/egao1980/cl-stack). Version **0.4** reads DOCX and XLSX with `xml-protocol` (worksheets, tables, headings). Version **0.3** reads markdown through `markdown-protocol` and keeps inline spans. Version **0.2** added the canonical `extracted-document` block tree (`demiurge-doc/1`).

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

HTML uses `html-protocol` when `*html-backend*` is bound; otherwise a tag-strip + heading heuristic. PDF is not implemented here.

## DOCX and XLSX

Office Open XML packages (transitional and strict) are read with `compression-protocol` (zip, deflate by `compression-backend-chipz`) and parsed with `xml-protocol` (`xml-backend-native`). Both are dependencies, so `(asdf:load-system "doc-extract-protocol")` is enough. Parts are found through the package relationships (`_rels/.rels`, `*.rels`), so non-default part names work.

| Format | `extract-document` | `extract-sections` | `extract-text` |
|--------|--------------------|--------------------|----------------|
| `:docx` | paragraphs as `text-block`s, `w:tbl` as `table-block`s (`w:gridSpan` → col-span, `w:vMerge` → row-span, `w:tblHeader` → header cells, nested tables kept), nested under a `section` per heading | one per heading, plus an untitled one for content before the first | one line per paragraph or table row, cells separated by Tab |
| `:xlsx` | one `section` per sheet (title = sheet name, workbook order) holding a `table-block` cropped to the used range; merged cells set spans | one per sheet | rows as lines, cells separated by Tab; no sheet names |

Headings come from `w:outlineLvl`, the paragraph style's level in `word/styles.xml` (built-in name `heading N`, so localized style ids map), or a `HeadingN` style id. Run text joins without separators; `w:tab` and `w:ptab` are a Tab, `w:br` a newline. A text box (`w:txbxContent`, DrawingML or VML) is not run text: its paragraphs become blocks right after the paragraph that anchors it (python-docx leaves text boxes out of `paragraph.text`). Tracked deletions, field codes and `mc:AlternateContent` fallbacks are not text; insertions, hyperlinks and content controls are.

Cells: shared strings by index (rich-text runs joined, phonetic runs dropped), inline strings, numbers as written in `<v>`, the cached value of a formula (never the formula), `TRUE` / `FALSE`, error values such as `#DIV/0!`. Dates and number formats are not applied yet: a date cell gives its serial number ([#7](https://github.com/egao1980/doc-extract-protocol/issues/7)).

Tables are bounded by `*max-table-cells*` (default 4,000,000 rows × columns), because the block model and its renderers hold a table as a full grid. A larger table (a sheet with a stray value in `XFD1048576`, a huge `w:gridSpan`) keeps only the rows and columns in which a cell starts, so positions are no longer the sheet's; spans and merged regions count the kept ones. If that is still too large, `doc-extract-error`, with a `continue` restart that leaves the table out.

`:docx-xml` / `:xlsx-xml` take one unzipped part as a string, pathname or octets: `word/document.xml`, or a worksheet, `xl/sharedStrings.xml` or `xl/workbook.xml`. A lone worksheet cannot resolve shared strings. Damaged input signals `doc-extract-error`, where python-docx / openpyxl raise `XMLSyntaxError`, `BadZipFile` or `zlib.error`. This covers malformed XML (a character reference that is not an XML character too), a source that is not a zip, a package without its main part, and a part whose deflate data, size or CRC-32 is damaged or that runs past the end of the file. `doc-extract-error-cause` holds the xml-protocol or compression-protocol condition underneath.

Conditions: `doc-extract-error`, `doc-extract-unsupported`, `no-extractor-for-format`. Restarts: `use-value`, `retry`, `use-backend`; `continue` for a table over `*max-table-cells*`.

## License

MIT — see [LICENSE](LICENSE).
