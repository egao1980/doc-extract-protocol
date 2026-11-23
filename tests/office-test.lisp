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
