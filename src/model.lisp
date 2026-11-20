(in-package #:doc-extract-protocol)

;;; Canonical extracted-document model (demiurge-doc/1).
;;; Interchange = schema-protocol dump/parse (JSON data model as hash-tables).
;;; Text lives on blocks; (document-text) is a derived view only.
;;; Offsets are Unicode code-point indices into the owning block's text.

(defparameter +document-schema-version+ "demiurge-doc/1")

(defun %omit-nils (class &rest keys)
  "MAKE-INSTANCE dropping NIL so DUMP omits optional empties."
  (apply #'make-instance class
         (loop for (k v) on keys by #'cddr
               when v
                 append (list k v))))

(schema-protocol:defschema annotation-span ()
  "Offset range over the owning block's plain text (code-point indices)."
  (start integer :accessor annotation-span-start)
  (end integer :accessor annotation-span-end)
  (kind keyword :accessor annotation-span-kind)
  (target string :optional t :accessor annotation-span-target))

(schema-protocol:defschema coord-system ()
  (origin keyword :default :top-left :accessor coord-system-origin)
  (units keyword :default :pixels :accessor coord-system-units)
  (layout-width real :optional t :accessor coord-system-layout-width)
  (layout-height real :optional t :accessor coord-system-layout-height))

(schema-protocol:defschema provenance-entry ()
  (page integer :optional t :accessor provenance-entry-page)
  (bbox t :optional t :accessor provenance-entry-bbox)
  (coord-system coord-system :optional t
                :accessor provenance-entry-coord-system)
  (charspan t :optional t :accessor provenance-entry-charspan))

(schema-protocol:defschema page-info ()
  (number integer :accessor page-info-number)
  (width real :optional t :accessor page-info-width)
  (height real :optional t :accessor page-info-height)
  (dpi real :optional t :accessor page-info-dpi)
  (raster-hash string :optional t :accessor page-info-raster-hash))

(schema-protocol:defschema document-metadata ()
  (title string :optional t :accessor document-metadata-title)
  (authors t :optional t :accessor document-metadata-authors)
  (dates t :optional t :accessor document-metadata-dates)
  (language string :optional t :accessor document-metadata-language)
  (mimetype string :optional t :accessor document-metadata-mimetype)
  (filename string :optional t :accessor document-metadata-filename)
  (content-hash string :optional t :accessor document-metadata-content-hash))

(schema-protocol:defschema block-relation ()
  (kind keyword :accessor block-relation-kind)
  (from-id string :accessor block-relation-from-id)
  (to-id string :accessor block-relation-to-id))

(schema-protocol:defschema merged-region ()
  (row integer :accessor merged-region-row)
  (col integer :accessor merged-region-col)
  (row-span integer :default 1 :accessor merged-region-row-span)
  (col-span integer :default 1 :accessor merged-region-col-span))

(schema-protocol:defschema block-node ()
  "Uniform envelope: every block has id, provenance, attrs, layer, confidence."
  (node-type keyword :accessor block-node-type)
  (id string :optional t :accessor block-id)
  (provenance (list provenance-entry) :optional t :default nil
              :accessor block-provenance)
  (attrs t :optional t :default nil :accessor block-attrs)
  (layer keyword :default :body :accessor block-layer)
  (confidence real :optional t :accessor block-confidence)
  (:tag node-type
        section text-block list-block code-block
        table-block image-block key-value-block)
  (:extra :ignore))

(schema-protocol:defschema table-cell ()
  (row integer :accessor table-cell-row)
  (col integer :accessor table-cell-col)
  (row-span integer :default 1 :accessor table-cell-row-span)
  (col-span integer :default 1 :accessor table-cell-col-span)
  (header-p boolean :default nil :accessor table-cell-header-p)
  (content (list block-node) :optional t :default nil
           :accessor table-cell-content))

(schema-protocol:defschema section (block-node)
  (node-type (eql :section) :default :section)
  (title string :default "" :accessor section-title)
  (level integer :default 1 :accessor section-level)
  (children (list block-node) :optional t :default nil
            :accessor section-children))

(schema-protocol:defschema text-block (block-node)
  (node-type (eql :text) :default :text)
  (kind keyword :default :para :accessor text-block-kind)
  (text string :default "" :accessor text-block-text)
  (annotations (list annotation-span) :optional t :default nil
               :accessor text-block-annotations))

(schema-protocol:defschema list-block (block-node)
  (node-type (eql :list) :default :list)
  (ordered-p boolean :default nil :accessor list-block-ordered-p)
  (items (list block-node) :optional t :default nil
         :accessor list-block-items))

(schema-protocol:defschema code-block (block-node)
  (node-type (eql :code) :default :code)
  (language string :optional t :accessor code-block-language)
  (text string :default "" :accessor code-block-text))

(schema-protocol:defschema table-block (block-node)
  (node-type (eql :table) :default :table)
  (rows integer :default 0 :accessor table-block-rows)
  (cols integer :default 0 :accessor table-block-cols)
  (cells (list table-cell) :optional t :default nil
         :accessor table-block-cells)
  (merged (list merged-region) :optional t :default nil
          :accessor table-block-merged))

(schema-protocol:defschema image-block (block-node)
  (node-type (eql :image) :default :image)
  (content-hash string :optional t :accessor image-block-content-hash)
  (octets t :optional t :accessor image-block-octets)
  (alt string :optional t :accessor image-block-alt)
  (ocr-text string :optional t :accessor image-block-ocr-text))

(schema-protocol:defschema key-value-block (block-node)
  (node-type (eql :key-value) :default :key-value)
  (key-text string :default "" :accessor key-value-block-key)
  (value-text string :default "" :accessor key-value-block-value))

(schema-protocol:defschema extracted-document ()
  (schema-version string :default "demiurge-doc/1"
                  :accessor extracted-document-schema-version)
  (metadata document-metadata :optional t
            :accessor extracted-document-metadata)
  (blocks (list block-node) :optional t :default nil
          :accessor extracted-document-blocks)
  (pages (list page-info) :optional t :default nil
         :accessor extracted-document-pages)
  (relations (list block-relation) :optional t :default nil
             :accessor extracted-document-relations)
  (id-index t :optional t :wire nil :dump nil
            :accessor extracted-document-id-index)
  (text-cache t :optional t :wire nil :dump nil
              :accessor extracted-document-text-cache)
  (:extra :ignore))

(defun block-node-p (x)
  (typep x 'block-node))

(defun extracted-document-p (x)
  (typep x 'extracted-document))

(defun make-annotation-span (&key start end kind target)
  (%omit-nils 'annotation-span :start start :end end :kind kind :target target))

(defun make-coord-system (&key (origin :top-left) (units :pixels)
                            layout-width layout-height)
  (apply #'make-instance 'coord-system
         :origin origin :units units
         (append (when layout-width (list :layout-width layout-width))
                 (when layout-height (list :layout-height layout-height)))))

(defun make-provenance-entry (&key page bbox coord-system charspan)
  (%omit-nils 'provenance-entry
              :page page :bbox bbox
              :coord-system coord-system :charspan charspan))

(defun make-page-info (&key number width height dpi raster-hash)
  (%omit-nils 'page-info :number number :width width :height height
              :dpi dpi :raster-hash raster-hash))

(defun make-document-metadata (&key title authors dates language
                                 mimetype filename content-hash)
  (%omit-nils 'document-metadata
              :title title :authors authors :dates dates
              :language language :mimetype mimetype
              :filename filename :content-hash content-hash))

(defun make-block-relation (&key kind from-id to-id)
  (make-instance 'block-relation :kind kind :from-id from-id :to-id to-id))

(defun %block-keys (&key id provenance attrs layer confidence)
  (append (when id (list :id id))
          (when provenance (list :provenance provenance))
          (when attrs (list :attrs attrs))
          (when layer (list :layer layer))
          (when confidence (list :confidence confidence))))

(defun make-text-block (&key id text (kind :para) layer attrs provenance
                          annotations confidence)
  (apply #'make-instance 'text-block
         :kind kind :text (or text "")
         (append (when annotations (list :annotations annotations))
                 (%block-keys :id id :provenance provenance :attrs attrs
                              :layer layer :confidence confidence))))

(defun make-section-block (&key id title (level 1) children layer attrs
                             provenance confidence)
  (apply #'make-instance 'section
         :title (or title "") :level level
         (append (when children (list :children children))
                 (%block-keys :id id :provenance provenance :attrs attrs
                              :layer layer :confidence confidence))))

(defun make-list-block (&key id items ordered-p layer attrs provenance
                          confidence)
  (apply #'make-instance 'list-block
         :ordered-p ordered-p
         (append (when items (list :items items))
                 (%block-keys :id id :provenance provenance :attrs attrs
                              :layer layer :confidence confidence))))

(defun make-code-block (&key id text language layer attrs provenance confidence)
  (apply #'make-instance 'code-block
         :text (or text "")
         (append (when language (list :language language))
                 (%block-keys :id id :provenance provenance :attrs attrs
                              :layer layer :confidence confidence))))

(defun make-table-cell (&key row col (row-span 1) (col-span 1) header-p content)
  (make-instance 'table-cell :row row :col col
                 :row-span row-span :col-span col-span
                 :header-p header-p :content content))

(defun make-table-block (&key id rows cols cells merged layer attrs
                           provenance confidence)
  (apply #'make-instance 'table-block
         :rows (or rows 0) :cols (or cols 0) :cells cells :merged merged
         (%block-keys :id id :provenance provenance :attrs attrs
                      :layer layer :confidence confidence)))

(defun make-image-block (&key id content-hash octets alt ocr-text
                           layer attrs provenance confidence)
  (apply #'make-instance 'image-block
         (append (when content-hash (list :content-hash content-hash))
                 (when octets (list :octets octets))
                 (when alt (list :alt alt))
                 (when ocr-text (list :ocr-text ocr-text))
                 (%block-keys :id id :provenance provenance :attrs attrs
                              :layer layer :confidence confidence))))

(defun make-key-value-block (&key id key value layer attrs provenance confidence)
  (apply #'make-instance 'key-value-block
         :key-text (or key "") :value-text (or value "")
         (%block-keys :id id :provenance provenance :attrs attrs
                      :layer layer :confidence confidence)))

(defun make-extracted-document (&key (schema-version +document-schema-version+)
                                  metadata blocks pages relations)
  (make-instance 'extracted-document
                 :schema-version schema-version
                 :metadata (or metadata (make-document-metadata))
                 :blocks blocks :pages pages :relations relations))

(defun block-children (block)
  "Immediate child blocks in reading order (tree order = reading order)."
  (etypecase block
    (section (or (section-children block) nil))
    (list-block (or (list-block-items block) nil))
    (table-block
     (mapcan (lambda (cell) (copy-list (or (table-cell-content cell) nil)))
             (or (table-block-cells block) nil)))
    (block-node nil)))

(defun (setf block-children) (new block)
  (etypecase block
    (section (setf (section-children block) new))
    (list-block (setf (list-block-items block) new))
    (block-node
     (error 'doc-extract-error
            :message (format nil "block ~s has no child list" (type-of block))))))

(defun block-plain-text (block)
  "Stored text of BLOCK (never a derived concatenation)."
  (etypecase block
    (text-block (or (text-block-text block) ""))
    (code-block (or (code-block-text block) ""))
    (section (or (section-title block) ""))
    (key-value-block
     (let ((k (or (key-value-block-key block) ""))
           (v (or (key-value-block-value block) "")))
       (if (and (plusp (length k)) (plusp (length v)))
           (format nil "~a: ~a" k v)
           (concatenate 'string k v))))
    (image-block
     (or (and (slot-boundp block 'ocr-text) (image-block-ocr-text block))
         (and (slot-boundp block 'alt) (image-block-alt block))
         ""))
    (table-block (render-table block :format :markdown))
    (list-block "")
    (block-node "")))

(defun map-blocks (blocks fn)
  (dolist (b blocks)
    (funcall fn b)
    (map-blocks (block-children b) fn)))

(defun map-document-blocks (doc fn)
  (map-blocks (extracted-document-blocks doc) fn))

(defun ensure-id-index (doc)
  (or (and (slot-boundp doc 'id-index) (extracted-document-id-index doc))
      (let ((ht (make-hash-table :test #'equal)))
        (map-document-blocks
         doc
         (lambda (b)
           (let ((id (block-id b)))
             (when id
               (setf (gethash id ht) b)))))
        (setf (extracted-document-id-index doc) ht))))

(defun find-block (doc id)
  (gethash id (ensure-id-index doc)))

(defun %layer-matches-p (block layer)
  (or (null layer)
      (eq layer :all)
      (eq (or (block-layer block) :body) layer)))

(defun document-text (doc &key (layer :body))
  "Concatenated reading-order text of LAYER blocks plus a block-id → (start end)
   map. Offsets are Unicode code-point indices. Derived only — never stored."
  (check-type doc extracted-document)
  (let* ((cache (and (slot-boundp doc 'text-cache)
                     (extracted-document-text-cache doc)))
         (hit (and cache (assoc layer cache :test #'eql))))
    (when hit
      (return-from document-text (values (second hit) (third hit))))
    (let ((out (make-string-output-stream))
          (spans (make-hash-table :test #'equal))
          (pos 0)
          (pending-nl nil))
      (labels ((emit (block text)
                 (when (and text (plusp (length text)))
                   (when pending-nl
                     (write-char #\Newline out)
                     (incf pos))
                   (setf pending-nl t)
                   (let ((start pos))
                     (write-string text out)
                     (incf pos (length text))
                     (when (and (slot-boundp block 'id) (block-id block))
                       (setf (gethash (block-id block) spans)
                             (list start pos)))))))
        (map-document-blocks
         doc
         (lambda (b)
           (when (and (%layer-matches-p b layer)
                      (not (typep b 'section))
                      (not (typep b 'list-block)))
             (emit b (block-plain-text b))))))
      (let ((text (get-output-stream-string out)))
        (setf (extracted-document-text-cache doc)
              (cons (list layer text spans) (remove layer cache :key #'car)))
        (values text spans)))))

(defun document-text-spans (doc &key (layer :body))
  (nth-value 1 (document-text doc :layer layer)))

(defun dump-document (doc &rest args &key (as :hash-table) format)
  (declare (ignore as format))
  (apply #'schema-protocol:dump doc args))

(defun parse-extracted-document (source &rest args &key coerce extra format)
  (declare (ignore coerce extra format))
  (apply #'schema-protocol:parse 'extracted-document source args))

(defun %alist<-hash (ht)
  (let ((acc '()))
    (maphash (lambda (k v) (push (cons k (%sexp-encode v)) acc)) ht)
    (sort acc #'string< :key (lambda (p) (princ-to-string (car p))))))

(defun %sexp-encode (value)
  (cond
    ((and (typep value 'standard-object)
          (schema-protocol:schema-class-p (class-of value)))
     (cons (class-name (class-of value))
           (%alist<-hash (schema-protocol:dump value :include-computed nil))))
    ((hash-table-p value)
     (cons :ht (%alist<-hash value)))
    ((and (vectorp value) (not (stringp value)))
     (cons :vec (map 'list #'%sexp-encode value)))
    ((listp value)
     (mapcar #'%sexp-encode value))
    (t value)))

(defun %sexp-decode (value)
  (cond
    ((and (consp value) (eq (first value) :ht))
     (let ((ht (make-hash-table :test #'equal)))
       (dolist (pair (rest value) ht)
         (setf (gethash (car pair) ht) (%sexp-decode (cdr pair))))))
    ((and (consp value) (eq (first value) :vec))
     (map 'vector #'%sexp-decode (rest value)))
    ((and (consp value)
          (symbolp (first value))
          (schema-protocol:schema-class-p
           (find-class (first value) nil)))
     (schema-protocol:parse (first value) (%sexp-decode (cons :ht (rest value)))))
    ((listp value)
     (mapcar #'%sexp-decode value))
    (t value)))

(defun document->sexp (doc)
  (%sexp-encode doc))

(defun sexp->document (sexp)
  (let ((v (%sexp-decode sexp)))
    (if (typep v 'extracted-document)
        v
        (parse-extracted-document v))))

(defun %cell-text (cell)
  (format nil "~{~a~^ ~}"
          (mapcar #'block-plain-text (or (table-cell-content cell) nil))))

(defun %table-grid (table)
  (let* ((rows (or (table-block-rows table) 0))
         (cols (or (table-block-cols table) 0))
         (grid (make-array (list (max 1 rows) (max 1 cols))
                           :initial-element "")))
    (when (or (zerop rows) (zerop cols))
      (return-from %table-grid (values #2A() 0 0)))
    (dolist (cell (or (table-block-cells table) nil))
      (let ((r (table-cell-row cell))
            (c (table-cell-col cell)))
        (when (and r c (< r rows) (< c cols))
          (setf (aref grid r c) (%cell-text cell)))))
    (values grid rows cols)))

(defun %escape-md-cell (s)
  (let ((out (make-string-output-stream)))
    (loop for ch across (or s "")
          do (when (char= ch #\|)
               (write-char #\\ out))
             (write-char ch out))
    (get-output-stream-string out)))

(defun render-table (table &key (format :markdown))
  (check-type table table-block)
  (multiple-value-bind (grid rows cols) (%table-grid table)
    (ecase format
      (:markdown
       (if (or (zerop rows) (zerop cols))
           ""
           (with-output-to-string (o)
             (dotimes (r rows)
               (write-string "|" o)
               (dotimes (c cols)
                 (format o " ~a |" (%escape-md-cell (aref grid r c))))
               (terpri o)
               (when (zerop r)
                 (write-string "|" o)
                 (dotimes (c cols)
                   (write-string " --- |" o))
                 (terpri o))))))
      (:csv
       (with-output-to-string (o)
         (dotimes (r rows)
           (dotimes (c cols)
             (when (plusp c) (write-char #\, o))
             (let ((s (aref grid r c)))
               (if (or (find #\, s) (find #\" s) (find #\Newline s))
                   (progn
                     (write-char #\" o)
                     (loop for ch across s
                           do (when (char= ch #\")
                                (write-char #\" o))
                              (write-char ch o))
                     (write-char #\" o))
                   (write-string s o))))
           (terpri o)))))))

(defun %section-path-string (path)
  (format nil "~{~a~^/~}" (reverse path)))

(defun split-block-tree (doc &key (layer :body))
  "Walk LAYER blocks. A table is one markdown chunk; furniture is skipped
   when LAYER is :body. Each chunk is a plist (:text :block-id :section-path :kind)."
  (check-type doc extracted-document)
  (let ((chunks '()))
    (labels ((visit (block path)
               (unless (%layer-matches-p block layer)
                 (return-from visit))
               (cond
                 ((typep block 'section)
                  (let ((path (cons (or (section-title block) "") path)))
                    (dolist (c (block-children block))
                      (visit c path))))
                 ((typep block 'list-block)
                  (dolist (c (block-children block))
                    (visit c path)))
                 ((typep block 'table-block)
                  (push (list :text (render-table block :format :markdown)
                              :block-id (block-id block)
                              :section-path (%section-path-string path)
                              :kind :table)
                        chunks))
                 (t
                  (let ((text (block-plain-text block)))
                    (when (plusp (length text))
                      (push (list :text text
                                  :block-id (block-id block)
                                  :section-path (%section-path-string path)
                                  :kind (block-node-type block))
                            chunks)))))))
      (dolist (b (extracted-document-blocks doc))
        (visit b '())))
    (nreverse chunks)))
