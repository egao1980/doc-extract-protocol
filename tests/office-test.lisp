(in-package #:doc-extract-protocol/tests)

;;; Issue #6: XLSX worksheets, DOCX table cells, XML parsing.
;;; Fixtures come from openpyxl / python-docx (tests/fixtures/make-office-fixtures.py).
;;; compression-backend-chipz (a test dependency) provides the zip reader.

(defun %office-fixture (name)
  (asdf:system-relative-pathname "doc-extract-protocol"
                                 (concatenate 'string "tests/fixtures/" name)))

(defun %tokens (text)
  (let ((tokens '()) (start nil))
    (loop for i from 0 to (length text)
          for c = (if (< i (length text)) (char text i) #\Space)
          do (if (member c '(#\Space #\Tab #\Newline #\Return))
                 (when start
                   (push (subseq text start i) tokens)
                   (setf start nil))
                 (unless start (setf start i))))
    (nreverse tokens)))

(defun %tokens-in-order-p (text &rest needles)
  "Each NEEDLE is a whitespace-delimited token of TEXT, in this order
   (other tokens, such as table separators, may come between)."
  (let ((tokens (%tokens text)))
    (every (lambda (needle)
             (let ((tail (member needle tokens :test #'string=)))
               (when tail (setf tokens (rest tail)) t)))
           needles)))

(defun %table-blocks (doc)
  (let ((tables '()))
    (labels ((walk (blocks)
               (dolist (b blocks)
                 (when (typep b 'doc-extract-protocol:table-block)
                   (push b tables))
                 (walk (doc-extract-protocol:block-children b)))))
      (walk (doc-extract-protocol:extracted-document-blocks doc)))
    (nreverse tables)))

(defun %cell-text (table row col)
  (let ((cell (find-if (lambda (c)
                         (and (= row (doc-extract-protocol:table-cell-row c))
                              (= col (doc-extract-protocol:table-cell-col c))))
                       (doc-extract-protocol:table-block-cells table))))
    (when cell
      (format nil "~{~a~^ ~}"
              (mapcar #'doc-extract-protocol:block-plain-text
                      (doc-extract-protocol:table-cell-content cell))))))

(defun %office-text (name format)
  (doc-extract-protocol:extract-text (%b) (%office-fixture name) :format format))

(deftest office-zip-reader-loaded
  (ok (doc-extract-protocol:zip-available-p)
      "compression-backend-chipz provides compression-protocol zip"))

(deftest xlsx-openpyxl-inline-strings-and-numbers
  "openpyxl writes t=\"inlineStr\" cells and no xl/sharedStrings.xml."
  (let ((text (%office-text "openpyxl-report.xlsx" :xlsx)))
    (ok (%tokens-in-order-p text "region" "q1" "q2" "North" "120" "135" "South" "98" "110")
        (format nil "cell values in cell order, got ~s" text))))

(deftest xlsx-shared-strings-formulas-and-sheets
  "Excel layout: shared strings by index (not sharedStrings.xml order), rich-text
   runs, numbers, cached formula values, entities, every sheet in workbook order."
  (let ((text (%office-text "excel-report.xlsx" :xlsx)))
    (ok (%tokens-in-order-p text "region" "q2" "total" "North" "135" "270" "R&D" "only")
        (format nil "cell values in cell order, got ~s" text))
    (ok (not (search "B2*2" text)) "formula text is not cell text"))
  (ok (equal '("Totals" "Notes")
             (mapcar #'doc-extract-protocol:section-title
                     (doc-extract-protocol:extract-sections
                      (%b) (%office-fixture "excel-report.xlsx") :format :xlsx)))
      "one section per sheet, titled with the sheet name"))

(deftest xlsx-sheet-is-table-block
  (let* ((doc (doc-extract-protocol:extract-document
               (%b) (%office-fixture "openpyxl-report.xlsx") :format :xlsx))
         (tables (%table-blocks doc))
         (table (first tables)))
    (ok (= 1 (length tables)) "one table-block per sheet")
    (when table
      (ok (= 3 (doc-extract-protocol:table-block-rows table)))
      (ok (= 3 (doc-extract-protocol:table-block-cols table)))
      (ok (equal "North" (%cell-text table 1 0)))
      (ok (equal "110" (%cell-text table 2 2))))))

(deftest docx-table-cells-are-separate
  "<w:t> must not match <w:tbl>, <w:tc>, <w:tr>."
  (let ((text (%office-text "python-docx-report.docx" :docx)))
    (ok (not (search "regionq2" text)) (format nil "cells run together: ~s" text))
    (ok (%tokens-in-order-p text "region" "q2" "North" "135")
        (format nil "table cells in order, got ~s" text))))

(deftest docx-runs-and-entities
  "Runs of one paragraph join without a separator; &amp; is decoded."
  (let ((text (%office-text "python-docx-report.docx" :docx)))
    (ok (search "Quarterly report" text))
    (ok (search "Sales grew in both regions." text)
        (format nil "runs \"Sales gr\" + \"ew\" + \" in both regions.\", got ~s" text))
    (ok (search "R&D grew too." text) (format nil "entity, got ~s" text))))

;;; Edge cases (doc-extract-protocol#6 fix): hand-written parts and packages.

(defparameter *w-ns* "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

(defparameter *s-ns* "http://schemas.openxmlformats.org/spreadsheetml/2006/main")

(defun %wdoc (body &key (ns *w-ns*))
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><w:document xmlns:w=\"~a\"><w:body>~a</w:body></w:document>"
          ns body))

(defun %wp (text &key style outline)
  (format nil "<w:p>~@[~a~]<w:r><w:t xml:space=\"preserve\">~a</w:t></w:r></w:p>"
          (cond (style (format nil "<w:pPr><w:pStyle w:val=\"~a\"/></w:pPr>" style))
                (outline (format nil "<w:pPr><w:outlineLvl w:val=\"~d\"/></w:pPr>" outline)))
          text))

(defun %wtc (inner &optional (tcpr ""))
  (format nil "<w:tc>~a~a</w:tc>" tcpr inner))

(defparameter *docx-structure-xml*
  (%wdoc
   (concatenate
    'string
    (%wp "Preface")
    (%wp "Intro" :style "Heading1")
    "<w:p><w:r><w:t>a</w:t><w:tab/><w:t>b</w:t><w:br/><w:t>c</w:t></w:r></w:p>"
    "<w:p><w:ins><w:r><w:t>kept</w:t></w:r></w:ins><w:del><w:r><w:delText>gone</w:delText></w:r></w:del><w:r><w:fldChar w:fldCharType=\"begin\"/></w:r><w:r><w:instrText> HYPERLINK \"x\" </w:instrText></w:r><w:hyperlink><w:r><w:t xml:space=\"preserve\"> link</w:t></w:r></w:hyperlink></w:p>"
    (%wp "Deeper" :outline 1)
    "<w:sdt><w:sdtPr><w:alias w:val=\"ctl\"/></w:sdtPr><w:sdtContent>" (%wp "In control") "</w:sdtContent></w:sdt>"
    "<w:p/>"
    (%wp "Next" :style "Heading1")
    "<w:tbl><w:tblPr/><w:tblGrid><w:gridCol/><w:gridCol/><w:gridCol/></w:tblGrid>"
    "<w:tr><w:trPr><w:tblHeader/></w:trPr>"
    (%wtc (%wp "wide") "<w:tcPr><w:gridSpan w:val=\"2\"/></w:tcPr>")
    (%wtc (%wp "h3"))
    "</w:tr><w:tr>"
    (%wtc (%wp "tall") "<w:tcPr><w:vMerge w:val=\"restart\"/></w:tcPr>")
    (%wtc (concatenate 'string (%wp "x") (%wp "y")))
    (%wtc (format nil "<w:tbl><w:tr>~a</w:tr></w:tbl>" (%wtc (%wp "inner"))))
    "</w:tr><w:tr>"
    (%wtc "<w:p/>" "<w:tcPr><w:vMerge/></w:tcPr>")
    (%wtc (%wp "z"))
    (%wtc "<w:p/>")
    "</w:tr></w:tbl><w:sectPr/>")))

(defun %find-cell (table row col)
  (find-if (lambda (c)
             (and (= row (doc-extract-protocol:table-cell-row c))
                  (= col (doc-extract-protocol:table-cell-col c))))
           (doc-extract-protocol:table-block-cells table)))

(deftest docx-runs-fields-and-revisions
  "w:tab / w:br, tracked insertions kept, deletions and field codes dropped,
   hyperlink text kept, content controls read through, empty paragraphs
   skipped."
  (let ((text (doc-extract-protocol:extract-text (%b) *docx-structure-xml* :format :docx-xml)))
    (ok (search (format nil "a~Cb~%c" #\Tab) text) text)
    (ok (search "kept link" text) text)
    (ng (search "gone" text))
    (ng (search "HYPERLINK" text))
    (ok (search "In control" text))
    (ng (search (format nil "~%~%") text) "no blank lines")))

(deftest docx-headings-nest-sections
  "HeadingN style ids and w:outlineLvl open sections; content before the
   first heading gets an untitled section."
  (let ((secs (doc-extract-protocol:extract-sections (%b) *docx-structure-xml*
                                                     :format :docx-xml)))
    (ok (equal '("" "Intro" "Deeper" "Next")
               (mapcar #'doc-extract-protocol:section-title secs)))
    (ok (equal '(1 1 2 1) (mapcar #'doc-extract-protocol:section-level secs)))
    (ok (equal "Preface" (doc-extract-protocol:section-text (first secs))))
    (ok (equal (format nil "a~Cb~%c~%kept link" #\Tab)
               (doc-extract-protocol:section-text (second secs))))
    (ok (equal "In control" (doc-extract-protocol:section-text (third secs)))))
  (let* ((doc (doc-extract-protocol:extract-document (%b) *docx-structure-xml*
                                                     :format :docx-xml))
         (blocks (doc-extract-protocol:extracted-document-blocks doc))
         (intro (second blocks)))
    (ok (= 3 (length blocks)) "Preface, Intro, Next at top level")
    (ok (typep (first blocks) 'doc-extract-protocol:text-block))
    (ok (find-if (lambda (b) (typep b 'doc-extract-protocol:section))
                 (doc-extract-protocol:section-children intro))
        "Deeper nests under Intro")))

(deftest docx-table-spans-header-and-nesting
  (let* ((doc (doc-extract-protocol:extract-document (%b) *docx-structure-xml*
                                                     :format :docx-xml))
         (table (find-if (lambda (b) (and (typep b 'doc-extract-protocol:table-block)
                                          (= 3 (doc-extract-protocol:table-block-rows b))))
                         (%table-blocks doc))))
    (ok table "outer table")
    (when table
      (ok (= 3 (doc-extract-protocol:table-block-cols table)))
      (let ((wide (%find-cell table 0 0))
            (tall (%find-cell table 1 0)))
        (ok (equal "wide" (%cell-text table 0 0)))
        (ok (= 2 (doc-extract-protocol:table-cell-col-span wide)) "w:gridSpan")
        (ok (doc-extract-protocol:table-cell-header-p wide) "w:tblHeader")
        (ok (equal "h3" (%cell-text table 0 2)))
        (ok (equal "tall" (%cell-text table 1 0)))
        (ok (= 2 (doc-extract-protocol:table-cell-row-span tall)) "w:vMerge")
        (ng (doc-extract-protocol:table-cell-header-p tall))
        (ok (null (%find-cell table 2 0)) "vMerge continuation is not a cell")
        (ok (equal "x y" (%cell-text table 1 1)))
        (ok (equal "z" (%cell-text table 2 1)))
        (ok (typep (first (doc-extract-protocol:table-cell-content (%find-cell table 1 2)))
                   'doc-extract-protocol:table-block)
            "nested table stays a table")
        (ok (equal '((0 0 1 2) (1 0 2 1))
                   (mapcar (lambda (m)
                             (list (doc-extract-protocol:merged-region-row m)
                                   (doc-extract-protocol:merged-region-col m)
                                   (doc-extract-protocol:merged-region-row-span m)
                                   (doc-extract-protocol:merged-region-col-span m)))
                           (doc-extract-protocol:table-block-merged table))))))
    (let ((text (doc-extract-protocol:extract-text (%b) *docx-structure-xml* :format :docx-xml)))
      (ok (search (format nil "wide~C~Ch3" #\Tab #\Tab) text) text)
      (ok (search (format nil "~Cz" #\Tab) text) text)
      (ok (search (format nil "tall~Cx y~Cinner~%" #\Tab #\Tab) text)
          "a nested table is flattened into its cell, on one line"))))

(deftest office-document-dump-round-trip
  "Office documents (tables, spans, merged regions) survive the JSON data
   model interchange."
  (dolist (doc (list (doc-extract-protocol:extract-document
                      (%b) *docx-structure-xml* :format :docx-xml)
                     (doc-extract-protocol:extract-document
                      (%b) (%office-fixture "excel-report.xlsx") :format :xlsx)))
    (let ((round (doc-extract-protocol:parse-extracted-document
                  (doc-extract-protocol:dump-document doc))))
      (ok (doc-extract-protocol:document-equivalent-p doc round :level :strict)))))

(deftest docx-strict-namespace
  (ok (search "Strict body"
              (doc-extract-protocol:extract-text
               (%b) (%wdoc (%wp "Strict body") :ns "http://purl.oclc.org/ooxml/wordprocessingml/main")
               :format :docx-xml))))

(defun %zip (&rest name-and-data)
  (compression-protocol:write-archive-bytes
   (loop for (name data) on name-and-data by #'cddr collect (list name data))))

(defparameter *rel-ns* "http://schemas.openxmlformats.org/package/2006/relationships")

(defparameter *rel-type* "http://schemas.openxmlformats.org/officeDocument/2006/relationships/")

(defun %rels (&rest id-type-target)
  (format nil "<?xml version=\"1.0\"?><Relationships xmlns=\"~a\">~{<Relationship Id=\"~a\" Type=\"~a~a\" Target=\"~a\"/>~}</Relationships>"
          *rel-ns*
          (loop for (id type target) on id-type-target by #'cdddr
                append (list id *rel-type* type target))))

(deftest docx-package-styles-and-absolute-targets
  "The main part comes from _rels/.rels (absolute target). A localized
   style id maps to its level through word/styles.xml (built-in name
   \"heading 1\")."
  (let ((pkg (%zip "_rels/.rels" (%rels "rId1" "officeDocument" "/word/main.xml")
                   "word/main.xml"
                   (%wdoc (concatenate 'string (%wp "Überblick" :style "berschrift1")
                                       (%wp "Inhalt")))
                   "word/_rels/main.xml.rels" (%rels "rId1" "styles" "styles.xml")
                   "word/styles.xml"
                   (format nil "<w:styles xmlns:w=\"~a\"><w:style w:type=\"paragraph\" w:styleId=\"berschrift1\"><w:name w:val=\"heading 1\"/></w:style></w:styles>"
                           *w-ns*))))
    (let ((secs (doc-extract-protocol:extract-sections (%b) pkg :format :docx)))
      (ok (equal '("Überblick") (mapcar #'doc-extract-protocol:section-title secs)))
      (ok (equal "Inhalt" (doc-extract-protocol:section-text (first secs)))))))

(defparameter *worksheet-edges-xml*
  ;; One string per row, no ~<newline> directives: a Windows (CRLF)
  ;; checkout turns those into ~<return>, which FORMAT rejects.
  (concatenate
   'string
   (format nil "<worksheet xmlns=\"~a\"><sheetData>" *s-ns*)
   "<row r=\"2\"><c r=\"B2\" t=\"inlineStr\"><is><r><t>Na</t></r><r><t>me</t></r><rPh sb=\"0\" eb=\"1\"><t>ナ</t></rPh></is></c><c r=\"D2\" t=\"inlineStr\"><is><t>Flag</t></is></c></row>"
   "<row r=\"3\"><c r=\"B3\" t=\"b\"><v>1</v></c><c r=\"C3\" t=\"e\"><v>#DIV/0!</v></c><c r=\"D3\" t=\"str\"><f>A1&amp;\"x\"</f><v>ax</v></c></row>"
   "<row><c><v>7</v></c><c><v>8.5</v></c></row>"
   "<row r=\"6\"><c r=\"B6\" s=\"1\"/><c r=\"C6\"><f>1/0</f></c></row>"
   "</sheetData><mergeCells count=\"1\"><mergeCell ref=\"B2:C2\"/></mergeCells></worksheet>"))

(deftest xlsx-worksheet-cell-types-and-layout
  "Inline rich text (no phonetic runs), booleans, errors, string formulas,
   rows and cells without r, styled empty cells and formulas with no cached
   value, cropping to the used range, merged cells."
  (let ((text (doc-extract-protocol:extract-text (%b) *worksheet-edges-xml* :format :xlsx-xml)))
    (ok (equal (format nil "~CName~C~CFlag~%~CTRUE~C#DIV/0!~Cax~%7~C8.5"
                       #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
               text)
        (format nil "got ~s" text))
    (ng (search "ナ" text))
    (ng (search "1/0" text)))
  (let* ((doc (doc-extract-protocol:extract-document (%b) *worksheet-edges-xml*
                                                     :format :xlsx-xml))
         (table (first (%table-blocks doc))))
    (ok (= 3 (doc-extract-protocol:table-block-rows table)))
    (ok (= 4 (doc-extract-protocol:table-block-cols table)))
    (ok (equal "Name" (%cell-text table 0 1)))
    (ok (= 2 (doc-extract-protocol:table-cell-col-span (%find-cell table 0 1))) "B2:C2")
    (ok (equal "7" (%cell-text table 2 0)) "row and cell without r")
    (ok (= 1 (length (doc-extract-protocol:table-block-merged table))))))

(deftest xlsx-xml-shared-strings-part
  "An unzipped xl/sharedStrings.xml still reads (0.3 behaviour)."
  (ok (equal (format nil "a~%bc")
             (doc-extract-protocol:extract-text
              (%b) (format nil "<sst xmlns=\"~a\"><si><t>a</t></si><si><r><t>b</t></r><r><t>c</t></r></si><si><t/></si></sst>" *s-ns*)
              :format :xlsx-xml))))

(deftest xlsx-package-relationship-targets
  "Sheets and shared strings are found through xl/_rels/workbook.xml.rels
   (any part name, absolute or relative), sheets in workbook order, not
   part-name order; a sheet with no cells is an empty section."
  (let ((pkg (%zip "_rels/.rels" (%rels "rId1" "officeDocument" "xl/book.xml")
                   "xl/book.xml"
                   (format nil "<workbook xmlns=\"~a\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets><sheet name=\"Zeta\" sheetId=\"2\" r:id=\"rB\"/><sheet name=\"Alpha\" sheetId=\"1\" r:id=\"rA\"/><sheet name=\"Blank\" sheetId=\"3\" r:id=\"rC\"/></sheets></workbook>"
                           *s-ns*)
                   "xl/_rels/book.xml.rels"
                   (%rels "rA" "worksheet" "/xl/worksheets/a.xml"
                          "rB" "worksheet" "worksheets/../worksheets/b.xml"
                          "rC" "worksheet" "worksheets/c.xml"
                          "rS" "sharedStrings" "strings.xml")
                   "xl/strings.xml"
                   (format nil "<sst xmlns=\"~a\"><si><t>from-a</t></si><si><t>from-b</t></si></sst>" *s-ns*)
                   "xl/worksheets/a.xml"
                   (format nil "<worksheet xmlns=\"~a\"><sheetData><row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c></row></sheetData></worksheet>" *s-ns*)
                   "xl/worksheets/b.xml"
                   (format nil "<worksheet xmlns=\"~a\"><sheetData><row r=\"1\"><c r=\"A1\" t=\"s\"><v>1</v></c><c r=\"B1\" t=\"s\"><v>9</v></c></row></sheetData></worksheet>" *s-ns*)
                   "xl/worksheets/c.xml"
                   (format nil "<worksheet xmlns=\"~a\"><sheetData/></worksheet>" *s-ns*))))
    (let ((secs (doc-extract-protocol:extract-sections (%b) pkg :format :xlsx)))
      (ok (equal '("Zeta" "Alpha" "Blank") (mapcar #'doc-extract-protocol:section-title secs)))
      (ok (equal '("from-b" "from-a" "")
                 (mapcar #'doc-extract-protocol:section-text secs))
          "out-of-range shared string index is an empty cell"))
    (ok (equal (format nil "from-b~%from-a")
               (doc-extract-protocol:extract-text (%b) pkg :format :xlsx)))))

(deftest office-errors-are-doc-extract-errors
  (ok (signals (doc-extract-protocol:extract-text (%b) "<w:document" :format :docx-xml)
        'doc-extract-protocol:doc-extract-error)
      "malformed XML")
  (ok (signals (doc-extract-protocol:extract-text (%b) "<foo/>" :format :xlsx-xml)
        'doc-extract-protocol:doc-extract-error)
      "not a SpreadsheetML part")
  (ok (signals (doc-extract-protocol:extract-text
                (%b) (%wdoc "") :format :xlsx-xml)
        'doc-extract-protocol:doc-extract-error)
      "WordprocessingML given as :xlsx-xml")
  (ok (signals (doc-extract-protocol:extract-text
                (%b) (map '(vector (unsigned-byte 8)) #'char-code "not a zip at all")
                :format :docx)
        'doc-extract-protocol:doc-extract-error)
      "octets that are not a zip")
  (ok (signals (doc-extract-protocol:extract-text (%b) (%zip "other.xml" "<a/>") :format :docx)
        'doc-extract-protocol:doc-extract-error)
      "package without word/document.xml")
  (ok (signals (doc-extract-protocol:extract-text (%b) (%zip "other.xml" "<a/>") :format :xlsx)
        'doc-extract-protocol:doc-extract-error)
      "package without a workbook"))

(defun %damaged-docx (how)
  "tests/fixtures/python-docx-report.docx with word/document.xml damaged.
   HOW :deflate overwrites 50 bytes of its deflate data with #xFF; :size
   sets its compressed size in the central directory past the end of the
   file."
  (let* ((octets (with-open-file (in (%office-fixture "python-docx-report.docx")
                                     :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence v in)
                     v)))
         (name (map '(vector (unsigned-byte 8)) #'char-code "word/document.xml"))
         (local (- (search name octets) 30))
         (central (- (search name octets :start2 (+ local 30 (length name))) 46)))
    (ecase how
      (:deflate
       (let ((data (+ local 30 (length name)
                      (aref octets (+ local 28)) (* 256 (aref octets (+ local 29))))))
         (fill octets #xff :start (+ data 10) :end (+ data 60))))
      (:size
       (replace octets #(#xff #xff #xff #x7f) :start1 (+ central 20))))
    octets))

(deftest office-damaged-input-signals-doc-extract-error
  "evidence (PR #8, round 2): the README says malformed XML and a source
   that is not a zip signal DOC-EXTRACT-ERROR, so a caller can handle a bad
   upload with one HANDLER-CASE. These inputs leak other conditions:
     &#x110000; (a character reference past #x10FFFF; lxml / python-docx:
       XMLSyntaxError) => TYPE-ERROR from CODE-CHAR in xml-backend-native;
     a docx whose word/document.xml has corrupt deflate data (python-docx:
       zlib.error) => CHIPZ::CODE-LENGTHS-BOUNDS-ERROR, a chipz internal;
     a central-directory size past the end of the file (python-docx:
       BadZipFile) => a SUBSEQ bounds error."
  (flet ((check (label thunk)
           (let ((e (handler-case (progn (funcall thunk) nil)
                      (error (e) e))))
             (ok (typep e 'doc-extract-protocol:doc-extract-error)
                 (format nil "~a: ~s~@[ ~a~]" label (type-of e) e)))))
    (check "character reference past #x10FFFF"
           (lambda ()
             (doc-extract-protocol:extract-text (%b) (%wdoc (%wp "a&#x110000;b"))
                                                :format :docx-xml)))
    (check "character reference that is a bignum"
           (lambda ()
             (doc-extract-protocol:extract-text (%b) (%wdoc (%wp "a&#99999999999999999999;b"))
                                                :format :docx-xml)))
    (check "corrupt deflate data in word/document.xml"
           (lambda ()
             (doc-extract-protocol:extract-text (%b) (%damaged-docx :deflate) :format :docx)))
    (check "compressed size past the end of the file"
           (lambda ()
             (doc-extract-protocol:extract-text (%b) (%damaged-docx :size) :format :docx)))))

(deftest office-through-registry
  "extract-document with no backend finds the default one for :xlsx / :docx."
  (let ((doc (doc-extract-protocol:extract-document
              nil (%office-fixture "openpyxl-report.xlsx") :format :xlsx)))
    (ok (equal "Sales"
               (doc-extract-protocol:section-title
                (first (doc-extract-protocol:extracted-document-blocks doc)))))
    (ok (equal "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
               (doc-extract-protocol:document-metadata-mimetype
                (doc-extract-protocol:extracted-document-metadata doc))))))

(defparameter *docx-text-box-xml*
  (let ((box "<w:txbxContent><w:p><w:r><w:t>Box one</w:t></w:r></w:p><w:p><w:r><w:t>Box two</w:t></w:r></w:p></w:txbxContent>"))
    (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><w:document xmlns:w=\"~a\" xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:wps=\"http://schemas.microsoft.com/office/word/2010/wordprocessingShape\" xmlns:v=\"urn:schemas-microsoft-com:vml\"><w:body><w:p><w:r><w:t>Before</w:t></w:r><w:r><mc:AlternateContent><mc:Choice Requires=\"wps\"><w:drawing><wp:anchor><a:graphic><a:graphicData><wps:wsp><wps:txbx>~a</wps:txbx></wps:wsp></a:graphicData></a:graphic></wp:anchor></w:drawing></mc:Choice><mc:Fallback><w:pict><v:shape><v:textbox>~a</v:textbox></v:shape></w:pict></mc:Fallback></mc:AlternateContent></w:r><w:r><w:t>After</w:t></w:r></w:p></w:body></w:document>"
            *w-ns* box box))
  "A Word text box as Word writes it: DrawingML wps:txbx with a VML fallback,
   inside a run of the anchoring paragraph; the box has its own w:p.")

(deftest docx-text-box-paragraphs-stay-apart
  "evidence (PR #8): the paragraphs of a text box (w:txbxContent) are read
   as runs of the anchoring paragraph and glued: \"BeforeBox oneBox
   twoAfter\". python-docx 1.2 leaves text boxes out of paragraph.text
   (\"BeforeAfter\"); kept or dropped, words from different paragraphs must
   not run together."
  (let ((text (doc-extract-protocol:extract-text (%b) *docx-text-box-xml* :format :docx-xml)))
    (ok (search "Before" text) text)
    (ok (search "After" text) text)
    (ng (search "BeforeBox" text) text)
    (ng (search "oneBox" text) text)
    (ng (search "twoAfter" text) text)))

(deftest docx-ptab-is-a-tab
  "evidence (PR #8): w:ptab (absolute position tab: right-aligned text in
   headers, TOC leaders) is a Tab in python-docx 1.2's run.text (\"L\\tR\");
   the PR drops it and glues the words (\"LR\")."
  (ok (equal (format nil "L~CR" #\Tab)
             (doc-extract-protocol:extract-text
              (%b)
              (%wdoc "<w:p><w:r><w:t>L</w:t><w:ptab w:relativeTo=\"margin\" w:alignment=\"right\" w:leader=\"none\"/><w:t>R</w:t></w:r></w:p>")
              :format :docx-xml))))

(deftest xlsx-sparse-far-cell-bounded-memory
  "evidence (PR #8): a worksheet part of a few hundred bytes with values in
   A1 and XFD1048576 (Excel's last cell) makes the sheet table 1048576 x
   16384, and extract-text / extract-document allocate that dense grid
   (137 GB requested: HEAP-EXHAUSTED-ERROR). Expected: both values in the
   text, or a DOC-EXTRACT-ERROR refusing the sheet; never a storage
   condition."
  (let ((xml (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><worksheet xmlns=\"~a\"><sheetData><row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>first</t></is></c></row><row r=\"1048576\"><c r=\"XFD1048576\"><v>42</v></c></row></sheetData></worksheet>"
                     *s-ns*)))
    (flet ((outcome (fn)
             (handler-case (funcall fn)
               (doc-extract-protocol:doc-extract-error (e) e)
               (storage-condition (e) (list :storage-condition (type-of e))))))
      (let ((text (outcome (lambda ()
                             (doc-extract-protocol:extract-text (%b) xml :format :xlsx-xml)))))
        (ok (or (typep text 'doc-extract-protocol:doc-extract-error)
                (and (stringp text) (search "first" text) (search "42" text)))
            (format nil "extract-text: ~s" text)))
      (let ((doc (outcome (lambda ()
                            (doc-extract-protocol:extract-document (%b) xml :format :xlsx-xml)))))
        (ok (or (typep doc 'doc-extract-protocol:doc-extract-error)
                (typep doc 'doc-extract-protocol:extracted-document))
            (format nil "extract-document: ~s" doc))))))

(defun %block-texts (doc)
  "Plain text of each text block of DOC, in reading order."
  (let ((texts '()))
    (labels ((walk (blocks)
               (dolist (b blocks)
                 (when (typep b 'doc-extract-protocol:text-block)
                   (push (doc-extract-protocol:block-plain-text b) texts))
                 (walk (doc-extract-protocol:block-children b)))))
      (walk (doc-extract-protocol:extracted-document-blocks doc)))
    (nreverse texts)))

(defun %vml-doc (body)
  "A w:document whose BODY may use the VML (v:) namespace."
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><w:document xmlns:w=\"~a\" xmlns:v=\"urn:schemas-microsoft-com:vml\"><w:body>~a</w:body></w:document>"
          *w-ns* body))

(defun %vml-box (&rest paragraphs)
  "A run holding a VML text box (no mc:AlternateContent, as older Word
   writes it) with one w:p per PARAGRAPHS."
  (format nil "<w:r><w:pict><v:shape><v:textbox><w:txbxContent>~{<w:p><w:r><w:t>~a</w:t></w:r></w:p>~}</w:txbxContent></v:textbox></v:shape></w:pict></w:r>"
          paragraphs))

(deftest docx-text-boxes-are-blocks-after-their-anchor
  (let ((doc (doc-extract-protocol:extract-document (%b) *docx-text-box-xml* :format :docx-xml)))
    (ok (equal '("BeforeAfter" "Box one" "Box two") (%block-texts doc))
        "DrawingML box (its VML fallback not repeated): blocks after the anchor")
    (ok (equal (format nil "BeforeAfter~%Box one~%Box two")
               (doc-extract-protocol:extract-text (%b) *docx-text-box-xml* :format :docx-xml))))
  (flet ((texts (body)
           (%block-texts (doc-extract-protocol:extract-document (%b) (%vml-doc body)
                                                                :format :docx-xml))))
    (ok (equal '("AB" "V1" "V2")
               (texts (format nil "<w:p><w:r><w:t>A</w:t></w:r>~a<w:r><w:t>B</w:t></w:r></w:p>"
                              (%vml-box "V1" "V2"))))
        "VML-only box")
    (ok (equal '("Only") (texts (format nil "<w:p>~a</w:p>" (%vml-box "Only"))))
        "a box anchored in an empty paragraph")
    (ok (equal '("Outer" "Inner")
               (texts (format nil "<w:p>~a</w:p>"
                              (format nil "<w:r><w:pict><v:shape><v:textbox><w:txbxContent><w:p><w:r><w:t>Outer</w:t></w:r>~a</w:p></w:txbxContent></v:textbox></v:shape></w:pict></w:r>"
                                      (%vml-box "Inner")))))
        "a box inside a box"))
  (let* ((doc (doc-extract-protocol:extract-document
               (%b)
               (%vml-doc (format nil "<w:p><w:pPr><w:outlineLvl w:val=\"0\"/></w:pPr><w:r><w:t>Title</w:t></w:r>~a</w:p><w:tbl><w:tr>~a</w:tr></w:tbl>"
                                 (%vml-box "Aside")
                                 (%wtc (format nil "<w:p><w:r><w:t>Cell</w:t></w:r>~a</w:p>"
                                               (%vml-box "Boxed")))))
               :format :docx-xml))
         (section (first (doc-extract-protocol:extracted-document-blocks doc))))
    (ok (and (typep section 'doc-extract-protocol:section)
             (equal "Title" (doc-extract-protocol:section-title section))
             (equal "Aside" (doc-extract-protocol:block-plain-text
                             (first (doc-extract-protocol:section-children section)))))
        "a heading's box is body text in its section, not a heading")
    (ok (equal "Cell Boxed" (%cell-text (first (%table-blocks doc)) 0 0))
        "a box in a table cell stays in the cell")))

(deftest docx-ptab-in-the-block-model
  (let ((doc (doc-extract-protocol:extract-document
              (%b)
              (%wdoc "<w:p><w:r><w:t>Name</w:t><w:ptab w:relativeTo=\"margin\" w:alignment=\"right\" w:leader=\"dot\"/><w:t>7</w:t></w:r></w:p>")
              :format :docx-xml)))
    (ok (equal (list (format nil "Name~C7" #\Tab)) (%block-texts doc)) "a TOC line")))

(defun %sheet-xml (cells &optional (merges '()))
  "A worksheet part with CELLS ((ref text) …) as inline strings and MERGES
   (\"A1:B2\" …)."
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><worksheet xmlns=\"~a\"><sheetData><row>~{~{<c r=\"~a\" t=\"inlineStr\"><is><t>~a</t></is></c>~}~}</row></sheetData>~@[<mergeCells>~{<mergeCell ref=\"~a\"/>~}</mergeCells>~]</worksheet>"
          *s-ns* cells merges))

(deftest office-tables-within-max-table-cells
  (let* ((xml (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?><worksheet xmlns=\"~a\"><sheetData><row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>first</t></is></c></row><row r=\"1048576\"><c r=\"XFD1048576\"><v>42</v></c></row></sheetData></worksheet>"
                      *s-ns*))
         (table (first (%table-blocks (doc-extract-protocol:extract-document (%b) xml :format :xlsx-xml)))))
    (ok (equal (format nil "first~%~C42" #\Tab)
               (doc-extract-protocol:extract-text (%b) xml :format :xlsx-xml))
        "A1 and XFD1048576: empty rows and columns dropped")
    (ok (and table
             (= 2 (doc-extract-protocol:table-block-rows table))
             (= 2 (doc-extract-protocol:table-block-cols table))
             (equal "first" (%cell-text table 0 0))
             (equal "42" (%cell-text table 1 1)))
        "a 2 x 2 table"))
  (let ((doc-extract-protocol:*max-table-cells* 6))
    (let* ((xml (%sheet-xml '(("A1" "a") ("C1" "c") ("A100" "z")) '("C1:E1" "B5:B6")))
           (table (first (%table-blocks
                          (doc-extract-protocol:extract-document (%b) xml :format :xlsx-xml))))
           (merged (doc-extract-protocol:table-block-merged table)))
      (ok (and (= 2 (doc-extract-protocol:table-block-rows table))
               (= 2 (doc-extract-protocol:table-block-cols table))
               (equal "c" (%cell-text table 0 1))
               (equal "z" (%cell-text table 1 0)))
          "100 x 3 compacted to 2 x 2, order kept")
      (ok (and (= 1 (length merged))
               (equal '(0 1 1 1)
                      (let ((m (first merged)))
                        (list (doc-extract-protocol:merged-region-row m)
                              (doc-extract-protocol:merged-region-col m)
                              (doc-extract-protocol:merged-region-row-span m)
                              (doc-extract-protocol:merged-region-col-span m))))
               (= 1 (doc-extract-protocol:table-cell-col-span (%find-cell table 0 1))))
          "spans count kept columns; a merge over dropped ones only is dropped"))
    (let ((dense (%sheet-xml '(("A1" "1") ("B1" "2") ("C1" "3")
                               ("A2" "4") ("B2" "5") ("C2" "6") ("A3" "7")))))
      (ok (signals (doc-extract-protocol:extract-text (%b) dense :format :xlsx-xml)
            'doc-extract-protocol:doc-extract-error)
          "a dense 3 x 3 sheet over the limit")
      (ok (equal ""
                 (handler-bind ((doc-extract-protocol:doc-extract-error
                                 (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
                   (doc-extract-protocol:extract-text (%b) dense :format :xlsx-xml)))
          "CONTINUE leaves the table out")))
  (let ((xml (%wdoc (format nil "<w:tbl><w:tr>~a~a</w:tr><w:tr>~a</w:tr></w:tbl>"
                            (%wtc (%wp "X") "<w:tcPr><w:gridSpan w:val=\"100000000\"/></w:tcPr>")
                            (%wtc (%wp "Y"))
                            (%wtc (%wp "Z"))))))
    (ok (equal (format nil "X~CY~%Z" #\Tab)
               (doc-extract-protocol:extract-text (%b) xml :format :docx-xml))
        "w:gridSpan 100000000")
    (let ((table (first (%table-blocks
                         (doc-extract-protocol:extract-document (%b) xml :format :docx-xml)))))
      (ok (and (= 2 (doc-extract-protocol:table-block-cols table))
               (= 1 (doc-extract-protocol:table-cell-col-span (%find-cell table 0 0))))
          "2 columns, the span covers one"))))
