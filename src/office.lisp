(in-package #:doc-extract-protocol)

;;; DOCX and XLSX (ECMA-376 Office Open XML, transitional and strict).
;;; Package parts come out of the zip through compression-protocol and are
;;; parsed with xml-protocol (doc-extract-protocol#6): namespaces, entities
;;; and xml:space come from the XML parser, not from string search.
;;; Each format is read into an EXTRACTED-DOCUMENT; EXTRACT-TEXT and
;;; EXTRACT-SECTIONS derive from it. :docx-xml / :xlsx-xml take one unzipped
;;; part (word/document.xml, a worksheet, xl/sharedStrings.xml or
;;; xl/workbook.xml) as a string, pathname or octets.

(defparameter +wml-namespaces+
  '("http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    "http://purl.oclc.org/ooxml/wordprocessingml/main")
  "WordprocessingML namespaces: transitional, strict.")

(defparameter +sml-namespaces+
  '("http://schemas.openxmlformats.org/spreadsheetml/2006/main"
    "http://purl.oclc.org/ooxml/spreadsheetml/main")
  "SpreadsheetML namespaces: transitional, strict.")

(defparameter +opc-relationship-namespaces+
  '("http://schemas.openxmlformats.org/package/2006/relationships")
  "Package relationships (_rels/*.rels), shared by both conformance classes.")

(defparameter +markup-compatibility-namespace+
  "http://schemas.openxmlformats.org/markup-compatibility/2006")

(defparameter +xmlns-namespace+ "http://www.w3.org/2000/xmlns/")

(defparameter +office-mimetypes+
  '((:docx . "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    (:xlsx . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")))

(defun %office-format-p (format)
  (and (member format '(:docx :xlsx :docx-xml :xlsx-xml)) t))

(defun %office-base-format (format)
  "FORMAT without the -xml suffix: :docx or :xlsx."
  (ecase format
    ((:docx :docx-xml) :docx)
    ((:xlsx :xlsx-xml) :xlsx)))

;;; XML helpers over xml-protocol nodes

(defun %ns-element-p (node namespaces &optional local-name)
  "NODE is an element in one of NAMESPACES, named LOCAL-NAME when given."
  (and (stack-xml:xml-element-p node)
       (member (stack-xml:xml-element-namespace-uri node) namespaces :test #'equal)
       (or (null local-name)
           (string= local-name (stack-xml:xml-element-local-name node)))
       t))

(defun %ns-children (element namespaces local-name)
  (and element
       (remove-if-not (lambda (c) (%ns-element-p c namespaces local-name))
                      (stack-xml:xml-element-children element))))

(defun %ns-child (element namespaces local-name)
  (and element
       (find-if (lambda (c) (%ns-element-p c namespaces local-name))
                (stack-xml:xml-element-children element))))

(defun %attr (element local-name)
  "Value of ELEMENT's attribute LOCAL-NAME in any namespace (w:val, r:id,
   t), never a namespace declaration; NIL when absent."
  (and element
       (let ((a (find-if (lambda (a)
                           (and (string= local-name (stack-xml:xml-attribute-local-name a))
                                (not (equal +xmlns-namespace+
                                            (stack-xml:xml-attribute-namespace-uri a)))))
                         (stack-xml:xml-element-attributes element))))
         (and a (stack-xml:xml-attribute-value a)))))

(defun %int (string)
  "Integer in STRING (OOXML ST_DecimalNumber), or NIL."
  (and string
       (let ((s (string-trim '(#\Space) string)))
         (and (plusp (length s))
              (every #'digit-char-p s)
              (parse-integer s)))))

(defun %trim-ws (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun %office-xml-root (text part format)
  "Root element of TEXT, the package part PART. Malformed XML signals
   DOC-EXTRACT-ERROR naming PART."
  (let ((root (handler-case (stack-xml:document-root-element (stack-xml:decode text))
                (stack-xml:xml-error (e)
                  (error 'doc-extract-error
                         :format format
                         :message (format nil "~a: ~a" part e)
                         :cause e)))))
    (or root
        (error 'doc-extract-error
               :format format
               :message (format nil "~a: no root element" part)))))

;;; Zip packages (Open Packaging Conventions)

(defun %call-with-office-package (source format fn)
  "Call FN with SOURCE opened as a zip archive. A source that is not a zip
   signals DOC-EXTRACT-ERROR."
  (let ((archive (handler-case (compression-protocol:open-archive source :format :zip)
                   (compression-protocol:compression-error (e)
                     (error 'doc-extract-error
                            :format format
                            :message (format nil "not a ~(~a~) package (zip): ~a"
                                             format e)
                            :cause e)))))
    (unwind-protect (funcall fn archive)
      (compression-protocol:close-archive archive))))

(defun %package-part (archive name &optional format)
  "Decoded text of the part NAME (OPC part names ignore case), or NIL. A part
   the archive cannot read (damaged deflate data, a size or CRC-32 that does
   not match its header, data past the end of the file) signals
   DOC-EXTRACT-ERROR naming the part, as python-docx / openpyxl raise
   zlib.error or BadZipFile."
  (let ((entry (find name (compression-protocol:archive-entries archive)
                     :key #'compression-protocol:archive-entry-name
                     :test #'string-equal)))
    (when (and entry (not (compression-protocol:archive-entry-directory-p entry)))
      (decode-source-octets
       (handler-case (compression-protocol:read-entry archive entry)
         (compression-protocol:compression-error (e)
           (error 'doc-extract-error
                  :format format
                  :message (format nil "damaged package part ~a: ~a" name e)
                  :cause e)))
       :encoding nil))))

(defun %package-part-root (archive name format)
  "Root element of the part NAME, or NIL when the package has no such part."
  (let ((text (%package-part archive name format)))
    (and text (%office-xml-root text name format))))

(defun %part-directory (part)
  "\"xl/workbook.xml\" => \"xl/\"."
  (let ((slash (position #\/ part :from-end t)))
    (if slash (subseq part 0 (1+ slash)) "")))

(defun %rels-part-name (part)
  "Relationships part of PART: \"xl/workbook.xml\" => \"xl/_rels/workbook.xml.rels\"."
  (let ((dir (%part-directory part)))
    (concatenate 'string dir "_rels/" (subseq part (length dir)) ".rels")))

(defun %resolve-part-name (base-directory target)
  "Part name of the relationship TARGET seen from BASE-DIRECTORY: absolute
   targets drop the leading /, relative ones resolve . and .. segments."
  (let* ((absolute (and (plusp (length target)) (char= #\/ (char target 0))))
         (path (if absolute
                   (subseq target 1)
                   (concatenate 'string base-directory target)))
         (out '()))
    (dolist (segment (uiop:split-string path :separator "/"))
      (cond ((string= segment "..") (pop out))
            ((or (string= segment ".") (string= segment "")))
            (t (push segment out))))
    (format nil "~{~a~^/~}" (nreverse out))))

(defun %package-relationships (archive part format)
  "Relationships of PART (\"\" for the package) as plists (:id :type :target),
   targets resolved to part names. External targets are left out."
  (let* ((rels-part (if (string= part "") "_rels/.rels" (%rels-part-name part)))
         (root (%package-part-root archive rels-part format))
         (base (%part-directory part)))
    (loop for rel in (%ns-children root +opc-relationship-namespaces+ "Relationship")
          for target = (%attr rel "Target")
          unless (or (null target) (equal (%attr rel "TargetMode") "External"))
            collect (list :id (%attr rel "Id")
                          :type (or (%attr rel "Type") "")
                          :target (%resolve-part-name base target)))))

(defun %relationship-type-p (relationship kind)
  "RELATIONSHIP's type URI ends in /KIND (transitional and strict URIs differ
   only before the last segment)."
  (let ((type (getf relationship :type))
        (suffix (concatenate 'string "/" kind)))
    (and (>= (length type) (length suffix))
         (string= suffix type :start2 (- (length type) (length suffix))))))

(defun %main-part-name (archive format default)
  "Target of the package's officeDocument relationship, else DEFAULT."
  (let ((rel (find-if (lambda (r) (%relationship-type-p r "officeDocument"))
                      (%package-relationships archive "" format))))
    (if rel (getf rel :target) default)))

;;; Text of blocks, for extract-text and extract-sections

(defun %office-cell-text (cell)
  "One-line text of a table CELL: its blocks joined by a space, a nested
   table's rows and cells too (line breaks and tabs become spaces)."
  (flet ((one-line (text)
           (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return #\Tab))) text)))
    (format nil "~{~a~^ ~}"
            (loop for b in (or (table-cell-content cell) '())
                  for text = (one-line (if (typep b 'table-block)
                                           (%office-table-text b)
                                           (block-plain-text b)))
                  when (plusp (length text))
                    collect text))))

(defun %office-table-text (table)
  "TABLE as lines of Tab-separated cells (see %OFFICE-CELL-TEXT); empty
   rows and trailing empty cells are left out. Only the cells are visited,
   never the whole rows × cols grid; of two cells at one position the last
   wins."
  (let ((rows (or (table-block-rows table) 0))
        (cols (or (table-block-cols table) 0))
        (by-row (make-hash-table)))
    (dolist (cell (or (table-block-cells table) '()))
      (let ((r (table-cell-row cell))
            (c (table-cell-col cell)))
        (when (and (< -1 r rows) (< -1 c cols))
          (push (cons c (%office-cell-text cell)) (gethash r by-row)))))
    (with-output-to-string (s)
      (let ((first t))
        (dolist (r (sort (loop for r being the hash-keys of by-row collect r) #'<))
          ;; Newest first, so after a stable sort the first of each column wins.
          (let ((line (loop with previous = nil
                            for (c . text) in (stable-sort (gethash r by-row) #'< :key #'car)
                            unless (eql c previous)
                              when (plusp (length text))
                                collect (cons c text)
                            do (setf previous c))))
            (when line
              (unless first (terpri s))
              (setf first nil)
              (let ((col 0))
                (loop for (c . text) in line
                      do (loop repeat (- c col) do (write-char #\Tab s))
                         (setf col c)
                         (write-string text s))))))))))

(defun %office-blocks-text (blocks &key (titles t))
  "Reading-order text of BLOCKS, one line per paragraph or table row;
   section titles too when TITLES."
  (with-output-to-string (s)
    (let ((first t))
      (labels ((line (text)
                 (when (plusp (length text))
                   (unless first (terpri s))
                   (setf first nil)
                   (write-string text s)))
               (walk (blocks)
                 (dolist (b blocks)
                   (typecase b
                     (section
                      (when titles (line (section-title b)))
                      (walk (section-children b)))
                     (table-block (line (%office-table-text b)))
                     (t (line (block-plain-text b)))))))
        (walk blocks)))))

(defun %office-document-sections (doc &key (titles t))
  "EXTRACTED-SECTIONs of DOC in reading order: one per section block (text:
   its own non-section children), after an untitled one for any content
   before the first section."
  (let ((out '())
        (blocks (extracted-document-blocks doc)))
    (flet ((own-text (children)
             (%office-blocks-text (remove-if (lambda (b) (typep b 'section)) children)
                                  :titles titles)))
      (let ((lead (remove-if (lambda (b) (typep b 'section)) blocks)))
        (when lead
          (push (make-extracted-section :title "" :text (own-text lead) :level 1) out)))
      (labels ((walk (blocks)
                 (dolist (b blocks)
                   (when (typep b 'section)
                     (push (make-extracted-section
                            :title (section-title b)
                            :text (own-text (section-children b))
                            :level (section-level b))
                           out)
                     (walk (section-children b))))))
        (walk blocks)))
    (nreverse out)))

(defun %nest-under-headings (items)
  "ITEMS: blocks and (:heading LEVEL TITLE). Each heading opens a section
   block holding what follows, up to the next heading of the same or a
   higher level. Returns the top-level blocks."
  (let ((roots '())
        (stack '()))
    (flet ((add (block)
             (if stack
                 (push block (section-children (cdr (first stack))))
                 (push block roots))))
      (dolist (item items)
        (if (and (consp item) (eq :heading (first item)))
            (destructuring-bind (level title) (rest item)
              (let ((section (make-section-block :title title :level level)))
                (setf (section-children section) '())
                (loop while (and stack (>= (car (first stack)) level))
                      do (pop stack))
                (add section)
                (push (cons level section) stack)))
            (add item))))
    (labels ((fix (block)
               (when (typep block 'section)
                 (setf (section-children block) (nreverse (section-children block)))
                 (mapc #'fix (section-children block)))))
      (let ((roots (nreverse roots)))
        (mapc #'fix roots)
        roots))))

(defun %office-document (blocks format)
  (ensure-ids
   (make-extracted-document
    :metadata (make-document-metadata
               :mimetype (cdr (assoc (%office-base-format format) +office-mimetypes+)))
    :blocks blocks)))

(defvar *max-table-cells* 4000000
  "Largest rows × columns of a table the DOCX and XLSX readers build. A
   larger one (a sheet with a stray value far from the rest, a huge
   w:gridSpan) keeps only the rows and the columns in which a cell starts;
   when that is still larger, DOC-EXTRACT-ERROR with a CONTINUE restart that
   leaves the table out. The block model and its renderers hold a table as
   a rows × columns grid, so this bounds their memory.")

(defun %sorted-unique (integers)
  "INTEGERS sorted ascending without duplicates, as a vector."
  (let ((sorted (sort (copy-list integers) #'<))
        (out '()))
    (dolist (n sorted)
      (unless (and out (= n (first out)))
        (push n out)))
    (coerce (nreverse out) 'simple-vector)))

(defun %lower-bound (keys value)
  "Index of the first element of the ascending vector KEYS that is >= VALUE."
  (let ((lo 0)
        (hi (length keys)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (< (svref keys mid) value)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    lo))

(defun %compact-table (cells merged)
  "CELLS and MERGED regions reduced to the rows and columns in which a cell
   starts: each position becomes its index among those, each span the number
   of them it covers. Merged regions that cover none are dropped.
   → (values rows cols cells merged)."
  (let ((row-keys (%sorted-unique (mapcar #'table-cell-row cells)))
        (col-keys (%sorted-unique (mapcar #'table-cell-col cells))))
    (flet ((covered (keys start span)
             (- (%lower-bound keys (+ start span)) (%lower-bound keys start))))
      (dolist (cell cells)
        (let ((r (table-cell-row cell))
              (c (table-cell-col cell)))
          (setf (table-cell-row-span cell) (max 1 (covered row-keys r (table-cell-row-span cell)))
                (table-cell-col-span cell) (max 1 (covered col-keys c (table-cell-col-span cell)))
                (table-cell-row cell) (%lower-bound row-keys r)
                (table-cell-col cell) (%lower-bound col-keys c))))
      (values (length row-keys) (length col-keys) cells
              (loop for m in merged
                    for r = (merged-region-row m)
                    for c = (merged-region-col m)
                    for row-span = (covered row-keys r (merged-region-row-span m))
                    for col-span = (covered col-keys c (merged-region-col-span m))
                    when (and (plusp row-span) (plusp col-span))
                      collect (make-instance 'merged-region
                                             :row (%lower-bound row-keys r)
                                             :col (%lower-bound col-keys c)
                                             :row-span row-span :col-span col-span))))))

(defun %office-table-block (rows cols cells merged format)
  "TABLE-BLOCK of ROWS × COLS holding CELLS and MERGED regions, within
   *MAX-TABLE-CELLS* (see there); NIL when the CONTINUE restart leaves it
   out."
  (when (> (* rows cols) *max-table-cells*)
    (let ((size (list rows cols)))
      (multiple-value-setq (rows cols cells merged) (%compact-table cells merged))
      (when (> (* rows cols) *max-table-cells*)
        (restart-case
            (error 'doc-extract-error
                   :format format
                   :message (format nil "a ~{~d × ~d~} table (~d × ~d without its empty rows and columns) is larger than *max-table-cells* (~d)"
                                    size rows cols *max-table-cells*))
          (continue ()
            :report "Leave the table out."
            (return-from %office-table-block nil))))))
  (make-table-block :rows rows :cols cols :cells cells :merged merged))

;;; DOCX (WordprocessingML)

(defun %wml-p (node &optional local-name)
  (%ns-element-p node +wml-namespaces+ local-name))

(defun %wml-child (element local-name)
  (%ns-child element +wml-namespaces+ local-name))

(defun %wml-val (element child-name)
  "w:val of ELEMENT's CHILD-NAME child (<w:pStyle w:val=…/>), or NIL."
  (%attr (%wml-child element child-name) "val"))

(defun %wml-on-p (element)
  "An ST_OnOff toggle such as <w:tblHeader/>: present and not false."
  (and element
       (not (member (%attr element "val") '("0" "false" "off") :test #'equal))))

(defparameter +wml-skipped-inline+
  '("pPr" "rPr" "del" "moveFrom" "instrText" "delInstrText")
  "Run content that is not document text: properties, deleted or
   moved-away text, field instructions.")

(defun %wml-inline-text (element stream on-text-box)
  "Write the text of ELEMENT's runs to STREAM: w:t as is, w:tab and w:ptab
   as Tab, w:br / w:cr as Newline, w:noBreakHyphen as -. Of an
   mc:AlternateContent only the first choice is read (the fallback repeats
   it). A text box (w:txbxContent, in a DrawingML wps:txbx or a VML
   v:textbox) holds paragraphs of its own, so it is not run text: it goes to
   the function ON-TEXT-BOX instead."
  (dolist (node (stack-xml:xml-element-children element))
    (when (stack-xml:xml-element-p node)
      (let ((name (stack-xml:xml-element-local-name node)))
        (cond
         ((equal +markup-compatibility-namespace+
                 (stack-xml:xml-element-namespace-uri node))
          (when (string= name "AlternateContent")
            (let ((choice (find-if #'stack-xml:xml-element-p
                                   (stack-xml:xml-element-children node))))
              (when choice (%wml-inline-text choice stream on-text-box)))))
         ((not (%wml-p node)) (%wml-inline-text node stream on-text-box))
         ((string= name "t") (write-string (stack-xml:xml-element-text node) stream))
         ((or (string= name "tab") (string= name "ptab")) (write-char #\Tab stream))
         ((or (string= name "br") (string= name "cr")) (write-char #\Newline stream))
         ((string= name "noBreakHyphen") (write-char #\- stream))
         ((string= name "txbxContent") (funcall on-text-box node))
         ((member name +wml-skipped-inline+ :test #'string=))
         (t (%wml-inline-text node stream on-text-box)))))))

(defun %wml-paragraph-text (p)
  "→ (values text text-boxes): the text of the paragraph P, and the
   w:txbxContent elements of its text boxes in document order, which are
   not part of that text."
  (let ((boxes '()))
    (values (%trim-ws (with-output-to-string (s)
                        (%wml-inline-text p s (lambda (box) (push box boxes)))))
            (nreverse boxes))))

(defun %outline-level (value)
  "w:outlineLvl 0..8 => heading level 1..9 (9 is body text)."
  (let ((n (%int value)))
    (and n (<= 0 n 8) (1+ n))))

(defun %heading-style-level (name)
  "\"Heading1\" (style id) or \"heading 1\" (built-in style name) => 1."
  (let ((prefix "heading"))
    (when (and (> (length name) (length prefix))
               (string-equal prefix name :end2 (length prefix)))
      (let ((n (%int (subseq name (length prefix)))))
        (and n (<= 1 n 9) n)))))

(defun %wml-style-levels (styles-root)
  "Paragraph style id => heading level, from word/styles.xml: the style's
   w:outlineLvl, else its built-in name \"heading N\" (stored in English
   whatever the UI language, so localized style ids still map)."
  (let ((levels (make-hash-table :test #'equal)))
    (dolist (style (%ns-children styles-root +wml-namespaces+ "style") levels)
      (let ((id (%attr style "styleId")))
        (when (and id (equal "paragraph" (or (%attr style "type") "paragraph")))
          (let ((level (or (%outline-level (%wml-val (%wml-child style "pPr") "outlineLvl"))
                           (let ((name (%wml-val style "name")))
                             (and name (%heading-style-level name))))))
            (when level
              (setf (gethash id levels) level))))))))

(defun %wml-heading-level (p style-levels)
  "Heading level of the paragraph P: its own w:outlineLvl, its style's
   level from STYLE-LEVELS, else a HeadingN style id; NIL for body text."
  (let* ((ppr (%wml-child p "pPr"))
         (style (%wml-val ppr "pStyle")))
    (or (%outline-level (%wml-val ppr "outlineLvl"))
        (and style style-levels (gethash style style-levels))
        (and style (%heading-style-level style)))))

(defparameter +wml-skipped-blocks+
  '("sectPr" "sdtPr" "sdtEndPr" "tblPr" "tblGrid" "trPr" "tcPr" "del" "moveFrom")
  "Block-level elements with no document text.")

(defun %wml-flow (container style-levels &key (headings t))
  "Blocks of a w:body, w:tc, w:sdtContent, w:txbxContent …, in order: text
   blocks for paragraphs, table blocks for w:tbl, and (:heading LEVEL TITLE)
   for heading paragraphs when HEADINGS. The paragraphs of a text box follow
   the paragraph that anchors it (never as headings). Content controls,
   custom XML and tracked insertions are read through."
  (let ((items '()))
    (labels ((walk (parent)
               (dolist (node (stack-xml:xml-element-children parent))
                 (when (stack-xml:xml-element-p node)
                   (let ((name (stack-xml:xml-element-local-name node)))
                     (cond
                      ((%wml-p node "p")
                       (multiple-value-bind (text boxes) (%wml-paragraph-text node)
                         (let ((level (and headings (%wml-heading-level node style-levels))))
                           (when (plusp (length text))
                             (push (if level
                                       (list :heading level text)
                                       (make-text-block :text text))
                                   items)))
                         (dolist (box boxes)
                           (dolist (item (%wml-flow box style-levels :headings nil))
                             (push item items)))))
                      ((%wml-p node "tbl")
                       (let ((table (%wml-table node style-levels)))
                         (when table (push table items))))
                      ((equal +markup-compatibility-namespace+
                              (stack-xml:xml-element-namespace-uri node))
                       (when (string= name "AlternateContent")
                         (let ((choice (find-if #'stack-xml:xml-element-p
                                                (stack-xml:xml-element-children node))))
                           (when choice (walk choice)))))
                      ((and (%wml-p node)
                            (member name +wml-skipped-blocks+ :test #'string=)))
                      (t (walk node))))))))
      (walk container))
    (nreverse items)))

(defun %wml-row-cells (tr)
  "w:tc elements of the row TR, also inside content controls / custom XML."
  (let ((cells '()))
    (labels ((walk (parent)
               (dolist (node (stack-xml:xml-element-children parent))
                 (cond ((%wml-p node "tc") (push node cells))
                       ((or (%wml-p node "sdt") (%wml-p node "sdtContent")
                            (%wml-p node "customXml"))
                        (walk node))))))
      (walk tr))
    (nreverse cells)))

(defun %wml-table (tbl style-levels)
  "TABLE-BLOCK of the w:tbl TBL, or NIL when *MAX-TABLE-CELLS* leaves it
   out. w:gridSpan sets col-span, w:vMerge restart / continue sets
   row-span, w:gridBefore skips grid columns, rows marked w:tblHeader are
   header cells. Spans are also listed as merged regions."
  (let* ((grid-cols (length (%ns-children (%wml-child tbl "tblGrid")
                                          +wml-namespaces+ "gridCol")))
         (cols grid-cols)
         (row 0)
         (cells '())
         (open-merges (make-hash-table)))
    (dolist (tr (%ns-children tbl +wml-namespaces+ "tr"))
      (let* ((trpr (%wml-child tr "trPr"))
             (header-p (%wml-on-p (%wml-child trpr "tblHeader")))
             (col (or (%int (%wml-val trpr "gridBefore")) 0)))
        (dolist (tc (%wml-row-cells tr))
          (let* ((tcpr (%wml-child tc "tcPr"))
                 (span (max 1 (or (%int (%wml-val tcpr "gridSpan")) 1)))
                 (vmerge (%wml-child tcpr "vMerge"))
                 (restart (and vmerge (equal "restart" (%attr vmerge "val"))))
                 (above (gethash col open-merges)))
            (if (and vmerge (not restart) above)
                (incf (table-cell-row-span above))
                (let ((cell (make-table-cell
                             :row row :col col :col-span span :header-p header-p
                             :content (%wml-flow tc style-levels :headings nil))))
                  (push cell cells)
                  (if restart
                      (setf (gethash col open-merges) cell)
                      (remhash col open-merges))))
            (incf col span)))
        (setf cols (max cols col))
        (incf row)))
    (let ((cells (nreverse cells)))
      (%office-table-block
       row cols cells
       (loop for cell in cells
             when (or (> (table-cell-row-span cell) 1)
                      (> (table-cell-col-span cell) 1))
               collect (make-instance 'merged-region
                                      :row (table-cell-row cell)
                                      :col (table-cell-col cell)
                                      :row-span (table-cell-row-span cell)
                                      :col-span (table-cell-col-span cell)))
       :docx))))

(defun %wml-document-blocks (root style-levels)
  "Top-level blocks of the w:document ROOT, nested under its headings."
  (let ((body (%wml-child root "body")))
    (unless (and (%wml-p root "document") body)
      (error 'doc-extract-error
             :format :docx
             :message "not a WordprocessingML document part (w:document/w:body)"))
    (%nest-under-headings (%wml-flow body style-levels))))

(defun %docx-document (source format)
  (%office-document
   (if (eq format :docx-xml)
       (%wml-document-blocks (%office-xml-root (%source-string source) "document" format)
                             nil)
       (%call-with-office-package
        source format
        (lambda (archive)
          (let* ((main (%main-part-name archive format "word/document.xml"))
                 (root (or (%package-part-root archive main format)
                           (error 'doc-extract-error
                                  :format :docx
                                  :message (format nil "~a missing" main))))
                 (styles-rel (find-if (lambda (r) (%relationship-type-p r "styles"))
                                      (%package-relationships archive main format)))
                 (styles (%package-part-root archive
                                             (if styles-rel
                                                 (getf styles-rel :target)
                                                 "word/styles.xml")
                                             format)))
            (%wml-document-blocks root (and styles (%wml-style-levels styles)))))))
   format))

;;; XLSX (SpreadsheetML)

(defun %sml-p (node &optional local-name)
  (%ns-element-p node +sml-namespaces+ local-name))

(defun %sml-children (element local-name)
  (%ns-children element +sml-namespaces+ local-name))

(defun %sml-child (element local-name)
  (%ns-child element +sml-namespaces+ local-name))

(defun %sml-rich-text (item)
  "Text of a shared-string <si> or inline <is>: its <t>, or the <t> of each
   rich-text run <r>. Phonetic runs (<rPh>) are not cell text."
  (with-output-to-string (s)
    (dolist (c (stack-xml:xml-element-children item))
      (cond ((%sml-p c "t")
             (write-string (stack-xml:xml-element-text c) s))
            ((%sml-p c "r")
             (let ((tx (%sml-child c "t")))
               (when tx (write-string (stack-xml:xml-element-text tx) s))))))))

(defun %sml-shared-strings (sst-root)
  (map 'vector #'%sml-rich-text (%sml-children sst-root "si")))

(defun %cell-reference (reference)
  "A1-style REFERENCE (\"B2\", \"$AB$12\") => (values row col), zero-based;
   NIL when it is not one."
  (when reference
    (let* ((ref (remove #\$ reference))
           (n (length ref))
           (i (or (position-if-not (lambda (c) (char<= #\A (char-upcase c) #\Z)) ref) n))
           (row (and (plusp i) (%int (subseq ref i)))))
      (when (and row (plusp row))
        (values (1- row)
                (1- (loop with col = 0
                          for k from 0 below i
                          do (setf col (+ (* col 26)
                                          (- (char-code (char-upcase (char ref k)))
                                             (char-code #\A))
                                          1))
                          finally (return col))))))))

(defun %range-reference (reference)
  "\"A1:C2\" => (values row col row-span col-span), zero-based; NIL otherwise."
  (let ((colon (and reference (position #\: reference))))
    (when colon
      (multiple-value-bind (r0 c0) (%cell-reference (subseq reference 0 colon))
        (multiple-value-bind (r1 c1) (%cell-reference (subseq reference (1+ colon)))
          (when (and r0 r1 (<= r0 r1) (<= c0 c1))
            (values r0 c0 (1+ (- r1 r0)) (1+ (- c1 c0)))))))))

(defun %sml-cell-value (c shared-strings)
  "Display text of the cell C, or NIL when it holds none. t=\"s\": shared
   string by index; inlineStr: <is>; b: TRUE / FALSE; n (default), str,
   e and d: <v> as written, a formula's cached value. Formulas themselves
   are not cell text."
  (let* ((type (or (%attr c "t") "n"))
         (v (%sml-child c "v"))
         (value (and v (stack-xml:xml-element-text v))))
    (cond
      ((string= type "inlineStr")
       (let ((is (%sml-child c "is")))
         (if is (%sml-rich-text is) value)))
      ((null value) nil)
      ((string= type "s")
       (let ((index (%int value)))
         (and index shared-strings (< index (length shared-strings))
              (aref shared-strings index))))
      ((string= type "b")
       (if (equal "1" (%trim-ws value)) "TRUE" "FALSE"))
      (t value))))

(defun %sml-worksheet-table (root shared-strings)
  "TABLE-BLOCK of a worksheet's <sheetData>, cropped to the cells that hold
   a value (row 0 / col 0 is the first used one), or NIL for an empty sheet
   (or one *MAX-TABLE-CELLS* leaves out). <mergeCells> set the spans of
   their top-left cell."
  (let ((values '())
        (next-row 0))
    (dolist (row (%sml-children (%sml-child root "sheetData") "row"))
      (let ((r (let ((n (%int (%attr row "r")))) (if (and n (plusp n)) (1- n) next-row)))
            (next-col 0))
        (dolist (c (%sml-children row "c"))
          (multiple-value-bind (cell-row cell-col) (%cell-reference (%attr c "r"))
            (let ((col (or cell-col next-col))
                  (text (%sml-cell-value c shared-strings)))
              (when (and text (plusp (length text)))
                (push (list (or cell-row r) col text) values))
              (setf next-col (1+ col)))))
        (setf next-row (1+ r))))
    (when values
      (let* ((values (nreverse values))
             (r0 (reduce #'min values :key #'first))
             (c0 (reduce #'min values :key #'second))
             (rows (1+ (- (reduce #'max values :key #'first) r0)))
             (cols (1+ (- (reduce #'max values :key #'second) c0)))
             (at (make-hash-table :test #'equal))
             (cells (mapcar (lambda (v)
                              (destructuring-bind (r c text) v
                                (setf (gethash (cons (- r r0) (- c c0)) at)
                                      (make-table-cell :row (- r r0) :col (- c c0)
                                                       :content (list (make-text-block :text text))))))
                            values))
             (merged '()))
        (dolist (m (%sml-children (%sml-child root "mergeCells") "mergeCell"))
          (multiple-value-bind (r c row-span col-span) (%range-reference (%attr m "ref"))
            (when (and r (>= r r0) (>= c c0) (< (- r r0) rows) (< (- c c0) cols))
              (let* ((r (- r r0))
                     (c (- c c0))
                     (cell (gethash (cons r c) at)))
                (push (make-instance 'merged-region :row r :col c
                                                    :row-span row-span :col-span col-span)
                      merged)
                (when cell
                  (setf (table-cell-row-span cell) row-span
                        (table-cell-col-span cell) col-span))))))
        (%office-table-block rows cols cells (nreverse merged) :xlsx)))))

(defun %sml-sheet-section (title root shared-strings)
  "Section block TITLE (the sheet name) holding the sheet's table, if any."
  (let ((table (and root (%sml-worksheet-table root shared-strings))))
    (make-section-block :title title :level 1 :children (and table (list table)))))

(defun %sml-workbook-blocks (archive format)
  "One section block per sheet, in workbook order, titled with its name."
  (let* ((main (%main-part-name archive format "xl/workbook.xml"))
         (workbook (or (%package-part-root archive main format)
                       (error 'doc-extract-error
                              :format :xlsx
                              :message (format nil "~a missing" main))))
         (rels (%package-relationships archive main format))
         (sst-rel (find-if (lambda (r) (%relationship-type-p r "sharedStrings")) rels))
         (sst (%package-part-root archive
                                  (if sst-rel (getf sst-rel :target) "xl/sharedStrings.xml")
                                  format))
         (shared (and sst (%sml-shared-strings sst))))
    (unless (%sml-p workbook "workbook")
      (error 'doc-extract-error
             :format :xlsx
             :message (format nil "~a is not a SpreadsheetML workbook" main)))
    (loop for sheet in (%sml-children (%sml-child workbook "sheets") "sheet")
          for index from 1
          for id = (%attr sheet "id")
          for rel = (find id rels :key (lambda (r) (getf r :id)) :test #'equal)
          for part = (if rel
                         (getf rel :target)
                         (format nil "~aworksheets/sheet~d.xml" (%part-directory main) index))
          collect (%sml-sheet-section (or (%attr sheet "name") "")
                                      (%package-part-root archive part format)
                                      shared))))

(defun %sml-part-blocks (root)
  "Blocks of one unzipped SpreadsheetML part (:xlsx-xml): a worksheet (shared
   strings cannot be resolved without the package), the shared-string table,
   or a workbook (sheet names)."
  (cond
    ((%sml-p root "worksheet")
     (list (%sml-sheet-section "" root nil)))
    ((%sml-p root "sst")
     (list (make-section-block
            :title "" :level 1
            :children (loop for text across (%sml-shared-strings root)
                            when (plusp (length text))
                              collect (make-text-block :text text)))))
    ((%sml-p root "workbook")
     (loop for sheet in (%sml-children (%sml-child root "sheets") "sheet")
           collect (%sml-sheet-section (or (%attr sheet "name") "") nil nil)))
    (t
     (error 'doc-extract-error
            :format :xlsx-xml
            :message (format nil "not a SpreadsheetML part: <~a>"
                             (stack-xml:xml-qname root))))))

(defun %xlsx-document (source format)
  (%office-document
   (if (eq format :xlsx-xml)
       (%sml-part-blocks (%office-xml-root (%source-string source) "part" format))
       (%call-with-office-package
        source format
        (lambda (archive) (%sml-workbook-blocks archive format))))
   format))

;;; Entry points (the default backend's GF methods call these)

(defun office-extract-document (source &key format)
  "EXTRACTED-DOCUMENT of a DOCX / XLSX SOURCE (pathname, namestring or
   octets; one unzipped part for :docx-xml / :xlsx-xml). DOCX: paragraphs as
   text blocks, tables as table blocks, nested under heading sections.
   XLSX: one section per sheet (title = sheet name) holding a table block."
  (let ((fmt (or format :docx)))
    (ecase (%office-base-format fmt)
      (:docx (%docx-document source fmt))
      (:xlsx (%xlsx-document source fmt)))))

(defun %office-titles-p (format)
  "Section titles are document text for DOCX (headings), not for XLSX
   (sheet names)."
  (eq :docx (%office-base-format format)))

(defun office-extract-text (source &key format)
  "Text of SOURCE: one line per paragraph or table row, table cells
   separated by Tab."
  (let ((fmt (or format :docx)))
    (%office-blocks-text (extracted-document-blocks (office-extract-document source :format fmt))
                         :titles (%office-titles-p fmt))))

(defun office-extract-sections (source &key format)
  "DOCX: one section per heading (after an untitled one for content before
   the first). XLSX: one section per sheet, titled with its name."
  (let ((fmt (or format :docx)))
    (%office-document-sections (office-extract-document source :format fmt)
                               :titles (%office-titles-p fmt))))

(defun office-extract-metadata (source &key format)
  (declare (ignore source))
  (list :format (or format :docx)))

(defmethod normalize-document ((backend default-doc-extract-backend) raw &key format)
  "Office formats are read into the block tree directly (tables stay
   tables); other formats use the sections shim."
  (let ((fmt (infer-format raw format)))
    (if (%office-format-p fmt)
        (office-extract-document raw :format fmt)
        (call-next-method))))

(register-extractor 'default-doc-extract-backend
                    :formats '(:docx :xlsx :docx-xml :xlsx-xml
                               "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
                    :priority 10)
