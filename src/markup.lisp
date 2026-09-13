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

(defun %dump-block (stream block profile heading-depth)
  (%write-comment stream block profile)
  (etypecase block
    (section
     (let ((depth (max 1 (min 6 (or (section-level block) heading-depth 1)))))
       (dotimes (i depth) (write-char #\# stream))
       (write-char #\Space stream)
       (write-string (or (section-title block) "") stream)
       (terpri stream)
       (terpri stream)
       (dolist (c (section-children block))
         (%dump-block stream c profile (1+ depth)))
       (write-string "<!-- {close=section} -->" stream)
       (terpri stream)
       (terpri stream)))
    (text-block
     (ecase (or (text-block-kind block) :para)
       (:quote
        (write-string "> " stream)
        (write-string (or (text-block-text block) "") stream)
        (terpri stream))
       ((:para :heading)
        (write-string (or (text-block-text block) "") stream)
        (terpri stream)))
     (terpri stream))
    (code-block
     (let* ((body (or (code-block-text block) ""))
            (fence (if (search "```" body) "~~~~" "```")))
       (write-string fence stream)
       (when (code-block-language block)
         (write-string (code-block-language block) stream))
       (terpri stream)
       (write-string body stream)
       (unless (or (zerop (length body))
                   (char= (char body (1- (length body))) #\Newline))
         (terpri stream))
       (write-string fence stream)
       (terpri stream)
       (terpri stream)))
    (list-block
     (loop for item in (list-block-items block)
           for i from 1
           do (%write-comment stream item profile)
              (if (list-block-ordered-p block)
                  (format stream "~d. " i)
                  (write-string "- " stream))
              (if (typep item 'list-block)
                  (progn
                    (terpri stream)
                    (%dump-block stream item profile heading-depth))
                  (progn
                    (write-string (block-plain-text item) stream)
                    (terpri stream))))
     (terpri stream))
    (table-block
     (write-string (render-table block :format :markdown) stream)
     (terpri stream))
    (image-block
     (format stream "![~a](~a)~%"
             (or (image-block-alt block) "")
             (if (image-block-content-hash block)
                 (format nil "hash:~a" (image-block-content-hash block))
                 ""))
     (terpri stream))
    (key-value-block
     (format stream "**~a**: ~a~%"
             (or (key-value-block-key block) "")
             (or (key-value-block-value block) ""))
     (terpri stream))))

(defun dump-markup (doc &key (profile :llm) stream)
  "Serialize DOC as CommonMark + attribute comments.
   PROFILE :llm omits provenance; :archival keeps everything."
  (check-type doc extracted-document)
  (ensure-ids doc)
  (flet ((emit (s)
           (dolist (b (extracted-document-blocks doc))
             (%dump-block s b profile 1))))
    (if stream
        (progn (emit stream) stream)
        (with-output-to-string (s)
          (emit s)))))

(defun %blank-line-p (line)
  (every (lambda (c) (member c '(#\Space #\Tab #\Return))) line))

(defun %heading-line (line)
  (let ((n 0))
    (loop while (and (< n (length line)) (char= (char line n) #\#)
                     (< n 6))
          do (incf n))
    (when (and (plusp n)
               (< n (length line))
               (char= (char line n) #\Space))
      (values n (string-trim '(#\Space #\Tab) (subseq line (1+ n)))))))

(defun %list-line (line)
  (let ((s (string-left-trim '(#\Space #\Tab) line)))
    (cond
      ((and (>= (length s) 2)
            (char= (char s 0) #\-)
            (char= (char s 1) #\Space))
       (values :ul (subseq s 2)))
      ((and (>= (length s) 2)
            (digit-char-p (char s 0)))
       (let ((dot (position #\. s)))
         (when (and dot
                    (< (1+ dot) (length s))
                    (char= (char s (1+ dot)) #\Space)
                    (every #'digit-char-p (subseq s 0 dot)))
           (values :ol (subseq s (+ dot 2))))))
      (t (values nil nil)))))

(defun %fence-line (line)
  (let ((s (string-right-trim '(#\Space #\Tab #\Return) line)))
    (cond
      ((and (>= (length s) 3) (string= (subseq s 0 3) "```"))
       (values t (subseq s 3)))
      ((and (>= (length s) 4) (string= (subseq s 0 4) "~~~~"))
       (values t (subseq s 4)))
      (t (values nil nil)))))

(defun %table-line-p (line)
  (let ((s (string-trim '(#\Space #\Tab) line)))
    (and (plusp (length s)) (char= (char s 0) #\|))))

(defun %table-sep-p (line)
  (let ((s (string-trim '(#\Space #\Tab) line)))
    (and (plusp (length s))
         (char= (char s 0) #\|)
         (every (lambda (c) (member c '(#\| #\- #\: #\Space))) s))))

(defun %split-row (line)
  (let* ((s (string-trim '(#\Space #\Tab) line))
         (s (string-trim '(#\|) s)))
    (loop for start = 0 then (1+ bar)
          for bar = (position #\| s :start start)
          collect (string-trim '(#\Space #\Tab) (subseq s start (or bar (length s))))
          while bar)))

(defun %image-line (line)
  (let ((s (string-trim '(#\Space #\Tab) line)))
    (when (and (>= (length s) 5)
               (char= (char s 0) #\!)
               (char= (char s 1) #\[))
      (let ((rb (position #\] s :start 2)))
        (when (and rb (< (1+ rb) (length s)) (char= (char s (1+ rb)) #\())
          (let ((rp (position #\) s :start (+ rb 2))))
            (when rp
              (values (subseq s 2 rb) (subseq s (+ rb 2) rp)))))))))

(defun %kv-line (line)
  (let ((s (string-trim '(#\Space #\Tab) line)))
    (when (and (>= (length s) 5)
               (char= (char s 0) #\*)
               (char= (char s 1) #\*))
      (let ((end (search "**: " s :start2 2)))
        (when end
          (values (subseq s 2 end) (subseq s (+ end 4))))))))

(defun %quote-line (line)
  (let ((s (string-left-trim '(#\Space #\Tab) line)))
    (when (and (>= (length s) 2)
               (char= (char s 0) #\>)
               (char= (char s 1) #\Space))
      (subseq s 2))))

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

(defun parse-markup (input)
  "Parse CommonMark + attribute comments into an EXTRACTED-DOCUMENT."
  (let* ((text (etypecase input
                 (string input)
                 (pathname
                  (with-open-file (s input :direction :input)
                    (let ((buf (make-string (file-length s))))
                      (read-sequence buf s)
                      buf)))
                 ((vector (unsigned-byte 8)) (utf8-decode input))))
         (lines (let ((acc '())
                      (start 0))
                  (loop for i from 0 below (length text)
                        when (char= (char text i) #\Newline)
                          do (push (subseq text start i) acc)
                             (setf start (1+ i)))
                  (push (subseq text start) acc)
                  (nreverse acc)))
         (roots '())
         (stack '())
         (pending '())
         (i 0)
         (n (length lines)))
    (labels ((add-root (b)
               (if stack
                   (push b (section-children (cdr (first stack))))
                   (push b roots)))
             (enqueue-pending (plist)
               (setf pending (nconc pending (list plist))))
             (take-pending ()
               (pop pending))
             (take-pending-of (type)
               (let ((hit (find-if (lambda (p)
                                     (eq (%kw (getf p :type)) type))
                                   pending)))
                 (when hit
                   (setf pending (remove hit pending :count 1 :test #'eq)))
                 hit))
             (attach (block)
               (add-root block)))
      (tagbody
       :loop
         (when (>= i n) (go :done))
         (let* ((line (nth i lines))
                (comment (parse-attribute-comment line)))
           (cond
             (comment
              (if (getf comment :close)
                  (when stack (pop stack))
                  (enqueue-pending comment))
              (incf i)
              (go :loop))
             ((%blank-line-p line)
              (incf i)
              (go :loop)))
           (multiple-value-bind (depth title) (%heading-line line)
             (when depth
               (let ((block (%make-from-comment
                             (or (take-pending-of :section) (take-pending)
                                 (list :type "section" :level
                                       (princ-to-string depth)))
                             :default-type :section)))
                 (setf (section-title block) title
                       (section-level block) depth)
                 (loop while (and stack (>= (car (first stack)) depth))
                       do (pop stack))
                 (if stack
                     (push block (section-children (cdr (first stack))))
                     (push block roots))
                 (push (cons depth block) stack)
                 (incf i)
                 (go :loop))))
           (multiple-value-bind (fenced lang) (%fence-line line)
             (when fenced
               (incf i)
               (let ((body (make-string-output-stream)))
                 (loop while (< i n)
                       for ln = (nth i lines)
                       do (multiple-value-bind (end) (%fence-line ln)
                            (when end
                              (incf i)
                              (return))
                            (write-string ln body)
                            (terpri body)
                            (incf i)))
                 (let ((block (%make-from-comment
                               (or (take-pending) (list :type "code"))
                               :default-type :code)))
                   (setf (code-block-text block)
                         (string-right-trim '(#\Newline)
                                            (get-output-stream-string body)))
                   (when (and lang (plusp (length lang))
                              (not (code-block-language block)))
                     (setf (code-block-language block) lang))
                   (attach block)))
               (go :loop)))
           (when (%table-line-p line)
             (let ((rows '())
                   (header nil)
                   (plist (or (take-pending) (list :type "table"))))
               (loop while (and (< i n) (%table-line-p (nth i lines)))
                     do (if (%table-sep-p (nth i lines))
                            (setf header t)
                            (push (%split-row (nth i lines)) rows))
                        (incf i))
               (setf rows (nreverse rows))
               (let* ((ncols (if rows (length (first rows)) 0))
                      (nrows (length rows))
                      (cells '())
                      (block (%make-from-comment plist :default-type :table)))
                 (loop for row in rows
                       for r from 0
                       do (loop for text in row
                                for c from 0
                                do (push (make-table-cell
                                          :row r :col c
                                          :header-p (and header (zerop r))
                                          :content
                                          (list (make-text-block :text text)))
                                         cells)))
                 (setf (table-block-rows block) nrows
                       (table-block-cols block) ncols
                       (table-block-cells block) (nreverse cells))
                 (let ((raw (getf plist :cells)))
                   (when raw
                     (mapc (lambda (child id)
                             (when (and id (plusp (length id)))
                               (setf (block-id child) id)))
                           (block-children block)
                           (loop for start = 0 then (1+ comma)
                                 for comma = (position #\, raw :start start)
                                 collect (subseq raw start (or comma (length raw)))
                                 while comma))))
                 (attach block)))
             (go :loop))
           (multiple-value-bind (alt href) (%image-line line)
             (when alt
               (let* ((hash (when (and href
                                       (>= (length href) 5)
                                       (string= (subseq href 0 5) "hash:"))
                              (subseq href 5)))
                      (block (%make-from-comment
                              (or (take-pending) (list :type "image"))
                              :default-type :image)))
                 (setf (image-block-alt block) alt)
                 (when hash
                   (setf (image-block-content-hash block) hash))
                 (attach block)
                 (incf i)
                 (go :loop))))
           (multiple-value-bind (k v) (%kv-line line)
             (when k
               (let ((block (%make-from-comment
                             (or (take-pending) (list :type "key-value"))
                             :default-type :key-value)))
                 (setf (key-value-block-key block) k
                       (key-value-block-value block) v)
                 (attach block)
                 (incf i)
                 (go :loop))))
           (let ((q (%quote-line line)))
             (when q
               (let ((block (%make-from-comment
                             (or (take-pending)
                                 (list :type "text" :kind "quote"))
                             :default-type :text)))
                 (setf (text-block-kind block) :quote
                       (text-block-text block) q)
                 (attach block)
                 (incf i)
                 (go :loop))))
           (multiple-value-bind (style item-text) (%list-line line)
             (declare (ignore item-text))
             (when style
               (let ((items '())
                     (list (%make-from-comment
                            (or (take-pending-of :list)
                                (list :type "list"
                                      :ordered (if (eq style :ol)
                                                   "true" "false")))
                            :default-type :list)))
                 (when (typep list 'list-block)
                   (setf (list-block-ordered-p list) (eq style :ol)))
                 (loop while (< i n)
                       for ln = (nth i lines)
                       do (cond
                            ((parse-attribute-comment ln)
                             (enqueue-pending (parse-attribute-comment ln))
                             (incf i))
                            ((%blank-line-p ln)
                             (incf i)
                             (when (and (< i n)
                                        (%blank-line-p (nth i lines)))
                               (return)))
                            ((nth-value 0 (%list-line ln))
                             (multiple-value-bind (st txt)
                                 (%list-line ln)
                               (declare (ignore st))
                               (let ((item (%make-from-comment
                                            (or (take-pending)
                                                (list :type "text"))
                                            :default-type :text)))
                                 (setf (text-block-text item) txt)
                                 (push item items)))
                             (incf i))
                            (t (return))))
                 (setf (list-block-items list) (nreverse items))
                 (attach list)
                 (go :loop))))
           (let ((block (%make-from-comment
                         (or (take-pending) (list :type "text"))
                         :default-type :text)))
             (setf (text-block-text block)
                   (string-trim '(#\Space #\Tab #\Return) line))
             (attach block)
             (incf i)
             (go :loop)))
       :done))
    (labels ((fix (b)
               (when (typep b 'section)
                 (setf (section-children b)
                       (nreverse (section-children b)))
                 (mapc #'fix (section-children b)))
               (when (typep b 'list-block)
                 (mapc #'fix (list-block-items b)))))
      (mapc #'fix roots))
    (make-extracted-document :blocks (nreverse roots))))
