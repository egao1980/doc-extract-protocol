(in-package #:doc-extract-protocol)

;;; CommonMark + attribute comments. Markup is lossy-tolerant; JSON/sexp
;;; are the lossless forms. :llm keeps ids (and type); :archival keeps
;;; provenance, attrs, annotations.

(defun %comment-quote (s)
  (if (or (zerop (length s))
          (find-if (lambda (c)
                     (or (char= c #\Space) (char= c #\")
                         (char= c #\=) (char= c #\>))
                     ) s))
      (with-output-to-string (o)
        (write-char #\" o)
        (loop for ch across s
              do (when (or (char= ch #\\) (char= ch #\"))
                   (write-char #\\ o))
                 (write-char ch o))
        (write-char #\" o))
      s))

(defun %bound (object slot)
  (and (slot-exists-p object slot)
       (slot-boundp object slot)
       (slot-value object slot)))

(defun %format-comment (block &key (profile :llm))
  (let ((parts (list (format nil "type=~a"
                             (string-downcase (symbol-name (block-node-type block)))))))
    (when (typep block 'text-block)
      (push (format nil "kind=~a"
                    (string-downcase (symbol-name (text-block-kind block))))
            parts))
    (when (typep block 'section)
      (push (format nil "level=~d" (or (section-level block) 1)) parts))
    (when (and (typep block 'list-block) (list-block-ordered-p block))
      (push "ordered=true" parts))
    (when (and (typep block 'code-block) (%bound block 'language))
      (push (format nil "lang=~a" (%comment-quote (code-block-language block)))
            parts))
    (when (typep block 'image-block)
      (when (%bound block 'alt)
        (push (format nil "alt=~a" (%comment-quote (image-block-alt block))) parts))
      (when (%bound block 'content-hash)
        (push (format nil "hash=~a" (image-block-content-hash block)) parts)))
    (when (typep block 'table-block)
      (push (format nil "rows=~d" (or (table-block-rows block) 0)) parts)
      (push (format nil "cols=~d" (or (table-block-cols block) 0)) parts)
      (let ((cids (mapcar #'block-id (block-children block))))
        (when (some #'identity cids)
          (push (format nil "cells=~{~a~^,~}"
                        (mapcar (lambda (id) (or id "")) cids))
                parts))))
    (let ((layer (or (block-layer block) :body)))
      (unless (eq layer :body)
        (push (format nil "layer=~a" (string-downcase (symbol-name layer)))
              parts)))
    (when (eq profile :archival)
      (let ((attrs (block-attrs block)))
        (when (and attrs (listp attrs))
          (loop for (k v) on attrs by #'cddr
                do (push (format nil "~a=~a"
                                 (string-downcase (symbol-name k))
                                 (%comment-quote (if (keywordp v)
                                                     (string-downcase (symbol-name v))
                                                     (princ-to-string v))))
                         parts))))
      (when (%bound block 'confidence)
        (push (format nil "conf=~a" (block-confidence block)) parts))
      (dolist (ann (and (typep block 'text-block)
                        (text-block-annotations block)))
        (push (format nil "ann=~d-~d:~a~@[:~a~]"
                      (annotation-span-start ann)
                      (annotation-span-end ann)
                      (string-downcase (symbol-name (annotation-span-kind ann)))
                      (%bound ann 'target))
              parts))
      (dolist (p (block-provenance block))
        (when (%bound p 'page)
          (push (format nil "page=~d" (provenance-entry-page p)) parts))
        (when (%bound p 'bbox)
          (push (format nil "bbox=~{~a~^,~}" (provenance-entry-bbox p)) parts))
        (when (%bound p 'charspan)
          (push (format nil "span=~{~a~^,~}" (provenance-entry-charspan p)) parts))
        (let ((cs (%bound p 'coord-system)))
          (when cs
            (push (format nil "units=~a"
                          (string-downcase
                           (symbol-name (or (%bound cs 'units) :pixels))))
                  parts)
            (push (format nil "origin=~a"
                          (string-downcase
                           (symbol-name (or (%bound cs 'origin) :top-left))))
                  parts)
            (when (%bound cs 'layout-width)
              (push (format nil "lw=~a" (coord-system-layout-width cs)) parts))
            (when (%bound cs 'layout-height)
              (push (format nil "lh=~a" (coord-system-layout-height cs)) parts))))))
    (format nil "<!-- {#~a~{ ~a~}} -->"
            (or (%bound block 'id) "anon")
            (nreverse parts))))

(defun %read-quoted (s start)
  (let ((out (make-string-output-stream))
        (i (1+ start)))
    (loop while (< i (length s))
          for ch = (char s i)
          do (cond
               ((char= ch #\\)
                (when (< (1+ i) (length s))
                  (write-char (char s (1+ i)) out)
                  (incf i 2)))
               ((char= ch #\")
                (return (values (get-output-stream-string out) (1+ i))))
               (t
                (write-char ch out)
                (incf i))))
    (values (get-output-stream-string out) i)))

(defun %read-token (s start)
  (let ((end start))
    (loop while (and (< end (length s))
                     (not (member (char s end)
                                  '(#\Space #\Tab #\Newline #\Return #\}))))
          do (incf end))
    (values (subseq s start end) end)))

(defun parse-attribute-comment (line)
  "Parse <!-- {#id k=v ...} --> → plist of keywords."
  (let ((open (search "<!--" line)))
    (unless open
      (return-from parse-attribute-comment nil))
    (let* ((lb (position #\{ line :start open))
           (rb (and lb (position #\} line :start lb))))
      (unless (and lb rb)
        (return-from parse-attribute-comment nil))
      (let ((body (subseq line (1+ lb) rb))
            (acc '())
            (i 0))
        (loop while (< i (length body))
              do (let ((ch (char body i)))
                   (cond
                     ((member ch '(#\Space #\Tab #\Newline #\Return))
                      (incf i))
                     ((char= ch #\#)
                      (multiple-value-bind (tok j) (%read-token body (1+ i))
                        (setf acc (list* :id tok acc)
                              i j)))
                     (t
                      (let ((eq (position #\= body :start i)))
                        (unless eq
                          (return))
                        (let* ((key (intern (string-upcase (subseq body i eq))
                                            :keyword)))
                          (incf eq)
                          (multiple-value-bind (val j)
                              (if (and (< eq (length body))
                                       (char= (char body eq) #\"))
                                  (%read-quoted body eq)
                                  (%read-token body eq))
                            (when (eq key :ann)
                              (setf acc (list* :ann
                                               (cons val (getf acc :ann))
                                               (progn (remf acc :ann) acc)))
                              (setf i j))
                            (unless (eq key :ann)
                              (setf acc (list* key val acc)
                                    i j)))))))))
        acc))))

(defun %kw (s &optional default)
  (cond
    ((null s) default)
    ((keywordp s) s)
    ((symbolp s) (intern (symbol-name s) :keyword))
    ((and (stringp s) (plusp (length s)))
     (intern (string-upcase s) :keyword))
    (t default)))

(defun %parse-int (s)
  (and s (parse-integer s :junk-allowed t)))

(defun %parse-num-list (s)
  (when (and s (plusp (length s)))
    (loop for start = 0 then (1+ comma)
          for comma = (position #\, s :start start)
          collect (let ((*read-eval* nil))
                    (read-from-string s t nil :start start
                                      :end (or comma (length s))))
          while comma)))

(defun %parse-anns (raw-list)
  (mapcar (lambda (raw)
            (let* ((colon (position #\: raw))
                   (range (subseq raw 0 colon))
                   (rest (and colon (subseq raw (1+ colon))))
                   (dash (position #\- range))
                   (start (parse-integer range :end dash))
                   (end (parse-integer range :start (1+ dash)))
                   (colon2 (and rest (position #\: rest)))
                   (kind (%kw (if colon2 (subseq rest 0 colon2) rest)))
                   (target (and colon2 (subseq rest (1+ colon2)))))
              (make-annotation-span :start start :end end :kind kind
                                    :target (and target (plusp (length target))
                                                 target))))
          (reverse raw-list)))

(defun %attrs-from-comment (plist)
  (let ((reserved '(:id :type :kind :level :ordered :lang :alt :hash
                    :rows :cols :cells :layer :conf :ann :page :bbox :span
                    :units :origin :lw :lh :close))
        (acc '()))
    (loop for (k v) on plist by #'cddr
          unless (member k reserved)
            do (setf acc (list* k (%kw v v) acc)))
    acc))

(defun %provenance-from-comment (plist)
  (let ((page (%parse-int (getf plist :page)))
        (bbox (%parse-num-list (getf plist :bbox)))
        (span (%parse-num-list (getf plist :span)))
        (has-units (getf plist :units))
        (has-origin (getf plist :origin))
        (has-lw (getf plist :lw))
        (has-lh (getf plist :lh)))
    (when (or page bbox span has-units has-origin has-lw has-lh)
      (list (make-provenance-entry
             :page page :bbox bbox :charspan span
             :coord-system (when (or has-units has-origin has-lw has-lh)
                             (make-coord-system
                              :origin (%kw has-origin :top-left)
                              :units (%kw has-units :pixels)
                              :layout-width
                              (and has-lw
                                   (let ((*read-eval* nil))
                                     (read-from-string has-lw)))
                              :layout-height
                              (and has-lh
                                   (let ((*read-eval* nil))
                                     (read-from-string has-lh))))))))))

(defun %apply-comment (block plist)
  (when (getf plist :id)
    (setf (block-id block) (getf plist :id)))
  (when (getf plist :layer)
    (setf (block-layer block) (%kw (getf plist :layer) :body)))
  (when (getf plist :conf)
    (setf (block-confidence block)
          (let ((*read-eval* nil))
            (read-from-string (getf plist :conf)))))
  (let ((attrs (%attrs-from-comment plist)))
    (when attrs
      (setf (block-attrs block) attrs)))
  (let ((prov (%provenance-from-comment plist)))
    (when prov
      (setf (block-provenance block) prov)))
  (when (and (typep block 'text-block) (getf plist :ann))
    (setf (text-block-annotations block) (%parse-anns (getf plist :ann))))
  block)

(defun %write-comment (stream block profile)
  (write-string (%format-comment block :profile profile) stream)
  (terpri stream))

(defun %make-from-comment (plist &key default-type)
  (let* ((type (%kw (or (getf plist :type) default-type) :text))
         (block
           (ecase type
             (:section (make-section-block
                        :title ""
                        :level (or (%parse-int (getf plist :level)) 1)))
             (:text (make-text-block
                     :kind (%kw (getf plist :kind) :para)
                     :text ""))
             (:list (make-list-block
                     :ordered-p (equal (getf plist :ordered) "true")))
             (:code (make-code-block
                     :language (getf plist :lang)
                     :text ""))
             (:table (make-table-block
                      :rows (or (%parse-int (getf plist :rows)) 0)
                      :cols (or (%parse-int (getf plist :cols)) 0)))
             (:image (make-image-block
                      :alt (getf plist :alt)
                      :content-hash (getf plist :hash)))
             (:key-value (make-key-value-block :key "" :value "")))))
    (%apply-comment block plist)
    block))


(defun %text-spans (block)
  (and (typep block 'text-block) (%bound block 'annotations)))

(defun %write-prefixed (stream text first rest)
  "TEXT's lines to STREAM, the first after FIRST, the others after REST."
  (loop for start = 0 then (1+ nl)
        for nl = (position #\Newline text :start start)
        for prefix = first then rest
        do (write-string prefix stream)
           (write-string text stream :start start :end (or nl (length text)))
           (terpri stream)
        while nl))

(defun %dump-table (stream table)
  (multiple-value-bind (grid rows cols) (%table-grid table)
    (unless (or (zerop rows) (zerop cols))
      (let ((cells (make-array (list rows cols) :initial-element nil)))
        (dolist (cell (or (%bound table 'cells) nil))
          (let ((r (table-cell-row cell)) (c (table-cell-col cell)))
            (when (and r c (< -1 r rows) (< -1 c cols))
              (setf (aref cells r c) cell))))
        (flet ((cell-markup (r c)
                 (let* ((cell (aref cells r c))
                        (content (and cell (%bound cell 'content)))
                        (text (substitute #\Space #\Newline (aref grid r c))))
                   (if (and (= 1 (length content)) (typep (first content) 'text-block))
                       (markup-inline text (%text-spans (first content)))
                       (markup-escape text)))))
          (dotimes (r rows)
            (write-string "|" stream)
            (dotimes (c cols)
              (format stream " ~a |" (cell-markup r c)))
            (terpri stream)
            (when (zerop r)
              (write-string "|" stream)
              (dotimes (c cols) (write-string " --- |" stream))
              (terpri stream))))))))

(defun %dump-block (stream block profile heading-depth)
  (%write-comment stream block profile)
  (etypecase block
    (section
     (let ((depth (max 1 (min 6 (or (section-level block) heading-depth 1)))))
       (dotimes (i depth) (write-char #\# stream))
       (write-char #\Space stream)
       (write-string (substitute #\Space #\Newline
                                 (markup-escape (or (section-title block) "")))
                     stream)
       (terpri stream)
       (terpri stream)
       (dolist (c (section-children block))
         (%dump-block stream c profile (1+ depth)))
       (write-string "<!-- {close=section} -->" stream)
       (terpri stream)
       (terpri stream)))
    (text-block
     (let ((text (markup-inline (text-block-text block) (%text-spans block))))
       (ecase (or (text-block-kind block) :para)
         (:quote (%write-prefixed stream text "> " "> "))
         ((:para :heading) (%write-prefixed stream text "" ""))))
     (terpri stream))
    (code-block
     (let* ((body (or (code-block-text block) ""))
            (fence (if (search "```" body) "~~~~" "```")))
       (write-string fence stream)
       (let ((language (%bound block 'language)))
         (when language
           (write-string language stream)))
       (terpri stream)
       (write-string body stream)
       (unless (or (zerop (length body))
                   (char= (char body (1- (length body))) #\Newline))
         (terpri stream))
       (write-string fence stream)
       (terpri stream)
       (terpri stream)))
    (list-block
     (loop for item in (or (%bound block 'items) nil)
           for i from 1
           do (%write-comment stream item profile)
              (let ((marker (if (list-block-ordered-p block) (format nil "~d. " i) "- ")))
                (if (typep item 'list-block)
                    (progn
                      (write-string (string-right-trim " " marker) stream)
                      (terpri stream)
                      (%dump-block stream item profile heading-depth))
                    (%write-prefixed stream
                                     (if (typep item 'text-block)
                                         (markup-inline (text-block-text item)
                                                        (%text-spans item))
                                         (markup-escape (block-plain-text item)))
                                     marker
                                     (make-string (length marker)
                                                  :initial-element #\Space)))))
     (terpri stream))
    (table-block
     (%dump-table stream block)
     (terpri stream))
    (image-block
     (format stream "![~a](~a)~%"
             (markup-escape (or (%bound block 'alt) ""))
             (let ((hash (%bound block 'content-hash)))
               (if hash (format nil "hash:~a" hash) "")))
     (terpri stream))
    (key-value-block
     (format stream "**~a**: ~a~%"
             (markup-escape (or (key-value-block-key block) ""))
             (markup-escape (or (key-value-block-value block) "")))
     (terpri stream))))

(defun dump-markup (doc &key (profile :llm) stream)
  "Serialize DOC as CommonMark + attribute comments.
   PROFILE :llm omits provenance; :archival keeps everything. Text is
   escaped so PARSE-MARKUP reads it back; emphasis, strong, strike, code
   and link annotation spans are written as markdown when they nest."
  (check-type doc extracted-document)
  (ensure-ids doc)
  (flet ((emit (s)
           (dolist (b (extracted-document-blocks doc))
             (%dump-block s b profile 1))))
    (if stream
        (progn (emit stream) stream)
        (with-output-to-string (s)
          (emit s)))))

;;; ------------------------------------------------------------------
;;; Reading. The text is split at attribute-comment lines (outside fenced
;;; code); each run of other lines is parsed by markdown-protocol and its
;;; blocks are mapped onto the model, taking the pending comments in order
;;; as before. A list split by item comments is one list again: a list run
;;; that ends on a non-blank line, followed only by comments that are not
;;; type=list, continues into the next run's leading list.

(defun %markup-text (input)
  (etypecase input
    (string input)
    (pathname
     (with-open-file (s input :direction :input)
       (let ((buf (make-string (file-length s))))
         (subseq buf 0 (read-sequence buf s)))))
    ((vector (unsigned-byte 8)) (utf8-decode input))))

(defun %markup-lines (text)
  (let ((acc '()) (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) acc)
               (setf start (1+ i)))
    (when (< start (length text))
      (push (subseq text start) acc))
    (nreverse acc)))

(defun %blank-line-p (line)
  (every (lambda (c) (member c '(#\Space #\Tab #\Return))) line))

(defun %fence-run (line)
  "→ (values char length) when LINE opens or closes a fence, else NIL."
  (let* ((s (string-right-trim '(#\Return) line))
         (ind (or (position #\Space s :test-not #'char=) (length s))))
    (when (and (<= ind 3) (< ind (length s)) (find (char s ind) "`~"))
      (let* ((ch (char s ind))
             (end (or (position ch s :start ind :test-not #'char=) (length s)))
             (len (- end ind)))
        (when (>= len 3)
          (values ch len (%blank-line-p (subseq s end))))))))

(defun %comment-line (line)
  "The attribute plist of an attribute-comment LINE, else NIL."
  (let ((s (string-trim '(#\Space #\Tab #\Return) line)))
    (and (>= (length s) 4)
         (string= "<!--" s :end2 4)
         (parse-attribute-comment s))))

(defun %markup-runs (lines)
  "LINES → a list of (:comment plist) and (:text string ends-blank-p)."
  (let ((runs '()) (current '()) (fence nil))
    (flet ((flush ()
             (when current
               (push (list :text (format nil "~{~a~%~}" (reverse current))
                           (%blank-line-p (first current)))
                     runs)
               (setf current '()))))
      (dolist (line lines)
        (let ((comment (and (not fence) (%comment-line line))))
          (if comment
              (progn (flush) (push (list :comment comment) runs))
              (progn
                (multiple-value-bind (ch len only-ws) (%fence-run line)
                  (cond ((null ch))
                        ((null fence) (setf fence (cons ch len)))
                        ((and (char= ch (car fence)) (>= len (cdr fence)) only-ws)
                         (setf fence nil))))
                (push line current)))))
      (flush))
    (nreverse runs)))

(defparameter +markup-block-types+
  '(:section :text :list :code :table :image :key-value))

(defun %block-from (plist type class)
  "A block of CLASS for pending comment PLIST (or none). TYPE is the
default type keyword. A comment whose type does not fit the content still
gives its id, attributes and provenance."
  (let ((ptype (and plist (%kw (getf plist :type)))))
    (if (and plist (or (null ptype) (member ptype +markup-block-types+)))
        (let ((b (%make-from-comment plist :default-type type)))
          (if (typep b class)
              b
              (%apply-comment (%make-from-comment nil :default-type type) plist)))
        (let ((b (%make-from-comment nil :default-type type)))
          (if plist (%apply-comment b plist) b)))))

(defun %set-text (block text spans)
  "Set a text BLOCK's TEXT; SPANS unless its comment gave annotations."
  (setf (text-block-text block) text)
  (when (and spans (null (%bound block 'annotations)))
    (setf (text-block-annotations block) spans))
  block)

(defun %image-paragraph (inlines)
  "→ (values alt url) for a paragraph that is exactly ![alt](url)."
  (when (and (= 2 (length inlines))
             (equal "!" (first inlines))
             (consp (second inlines))
             (eq :link (first (second inlines))))
    (values (inlines-text-spans (third (second inlines)))
            (second (second inlines)))))

(defun %kv-paragraph (inlines)
  "→ (values key value) for a one-line paragraph **key**: value."
  (let ((k (first inlines)) (rest (rest inlines)))
    (when (and (consp k) (eq :strong (first k))
               (stringp (first rest))
               (>= (length (first rest)) 2)
               (string= ": " (first rest) :end2 2)
               (notany (lambda (x) (and (consp x) (member (first x) '(:soft-break :hard-break))))
                       inlines))
      (values (inlines-text-spans (second k))
              (inlines-text-spans (cons (subseq (first rest) 2) (rest rest)))))))

(defun parse-markup (input)
  "Parse CommonMark + attribute comments into an EXTRACTED-DOCUMENT.
   Markdown is read by markdown-protocol. Paragraphs, quotes, list items
   and table cells keep their inline emphasis, strong, strike, code and
   links as ANNOTATION-SPANs over the plain text (an ann= comment, when
   present, gives the annotations instead)."
  (let ((roots '())
        (stack '())
        (pending '())
        (open-list nil))
    (labels ((attach (b)
               (if stack
                   (push b (section-children (cdr (first stack))))
                   (push b roots)))
             (take-pending () (pop pending))
             (take-pending-of (type)
               (let ((hit (find-if (lambda (p) (eq (%kw (getf p :type)) type)) pending)))
                 (when hit
                   (setf pending (remove hit pending :count 1 :test #'eq)))
                 hit))
             (text-block-of (text spans &optional kind)
               (let ((b (%block-from (take-pending) :text 'text-block)))
                 (when kind (setf (text-block-kind b) kind))
                 (%set-text b text spans)))
             (list-items (items)
               (loop for item in items
                     for texts = (remove-if (lambda (b) (member (first b) '(:bullet-list :ordered-list)))
                                            item)
                     when (or texts (null item))
                       collect (multiple-value-bind (text spans) (%blocks-text-spans texts)
                                 (text-block-of text spans))
                     nconc (loop for b in item
                                 when (member (first b) '(:bullet-list :ordered-list))
                                   collect (new-list b nil))))
             (new-list (b pending-p)
               (let* ((ordered (eq (first b) :ordered-list))
                      (list (%block-from (and pending-p (take-pending-of :list)) :list 'list-block)))
                 (setf (list-block-ordered-p list) ordered
                       (list-block-items list) (list-items (if ordered (fifth b) (third b))))
                 list))
             (heading (level inlines)
               (let ((b (%block-from (or (take-pending-of :section) (take-pending))
                                     :section 'section)))
                 (setf (section-title b) (substitute #\Space #\Newline (inlines-text-spans inlines))
                       (section-level b) level)
                 (loop while (and stack (>= (car (first stack)) level))
                       do (pop stack))
                 (attach b)
                 (push (cons level b) stack)))
             (paragraph (inlines)
               (multiple-value-bind (alt url) (%image-paragraph inlines)
                 (when alt
                   (let ((b (%block-from (take-pending) :image 'image-block)))
                     (setf (image-block-alt b) alt)
                     (when (and (>= (length url) 5) (string= "hash:" url :end2 5))
                       (setf (image-block-content-hash b) (subseq url 5)))
                     (return-from paragraph (attach b)))))
               (multiple-value-bind (k v) (%kv-paragraph inlines)
                 (when k
                   (let ((b (%block-from (take-pending) :key-value 'key-value-block)))
                     (setf (key-value-block-key b) k
                           (key-value-block-value b) v)
                     (return-from paragraph (attach b)))))
               (multiple-value-bind (text spans) (inlines-text-spans inlines)
                 (attach (text-block-of text spans))))
             (table (header rows)
               (let* ((plist (take-pending))
                      (b (%block-from plist :table 'table-block))
                      (all (cons header rows))
                      (cells (loop for row in all
                                   for r from 0
                                   nconc (loop for cell in row
                                               for c from 0
                                               collect (make-table-cell
                                                        :row r :col c :header-p (zerop r)
                                                        :content
                                                        (list (multiple-value-bind (text spans)
                                                                  (inlines-text-spans cell)
                                                                (make-text-block
                                                                 :text text :annotations spans))))))))
                 (setf (table-block-rows b) (length all)
                       (table-block-cols b) (length header)
                       (table-block-cells b) cells)
                 (let ((raw (getf plist :cells)))
                   (when raw
                     (mapc (lambda (child id)
                             (when (and id (plusp (length id)))
                               (setf (block-id child) id)))
                           (block-children b)
                           (loop for start = 0 then (1+ comma)
                                 for comma = (position #\, raw :start start)
                                 collect (subseq raw start (or comma (length raw)))
                                 while comma))))
                 (attach b)))
             (md-block (b)
               (ecase (first b)
                 (:heading (heading (second b) (third b)))
                 (:paragraph (paragraph (second b)))
                 (:thematic-break (attach (text-block-of "---" nil)))
                 (:block-quote
                  (multiple-value-bind (text spans) (%blocks-text-spans (second b))
                    (attach (text-block-of text spans :quote))))
                 (:code-block
                  (let ((c (%block-from (take-pending) :code 'code-block)))
                    (setf (code-block-text c)
                          (string-right-trim '(#\Newline) (format nil "~{~a~^~%~}" (third b))))
                    (when (and (second b) (not (%bound c 'language)))
                      (setf (code-block-language c) (second b)))
                    (attach c)))
                 ((:bullet-list :ordered-list) (attach (new-list b t)))
                 (:table (table (third b) (fourth b)))))
             (continues-list-p (b)
               (and open-list
                    (member (first b) '(:bullet-list :ordered-list))
                    (eq (list-block-ordered-p open-list) (eq (first b) :ordered-list))
                    (notany (lambda (p) (eq (%kw (getf p :type)) :list)) pending))))
      (dolist (run (%markup-runs (%markup-lines (%markup-text input))))
        (ecase (first run)
          (:comment
           (let ((plist (second run)))
             (if (getf plist :close)
                 (progn (when stack (pop stack))
                        (setf open-list nil))
                 (setf pending (nconc pending (list plist))))))
          (:text
           (let ((blocks (markdown-protocol:parse-markdown (second run))))
             (when blocks
               (when (continues-list-p (first blocks))
                 (let ((b (pop blocks)))
                   (setf (list-block-items open-list)
                         (append (%bound open-list 'items)
                                 (list-items (if (eq (first b) :ordered-list)
                                                 (fifth b)
                                                 (third b))))))))
             (mapc #'md-block blocks)
             (setf open-list
                   (let ((last (if stack
                                   (first (section-children (cdr (first stack))))
                                   (first roots))))
                     (and (not (third run))
                          (typep last 'list-block)
                          last))))))))
    (labels ((fix (b)
               (when (typep b 'section)
                 (setf (section-children b)
                       (nreverse (or (%bound b 'children) nil)))
                 (mapc #'fix (section-children b)))))
      (mapc #'fix roots))
    (make-extracted-document :blocks (nreverse roots))))
