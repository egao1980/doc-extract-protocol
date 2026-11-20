(in-package #:doc-extract-protocol)

;;; Inline spans between markdown-protocol inlines and the model.
;;;
;;; A text block keeps its plain text; emphasis, strong, strike, code spans
;;; and links are ANNOTATION-SPANs over it (code-point offsets, END
;;; exclusive), the representation the model already has:
;;;
;;;   (:emph …)       → :emphasis        (:strong …) → :strong
;;;   (:strike …)     → :strike          (:code s)   → :code
;;;   (:link url …)   → :link, TARGET url (NIL when url is "")
;;;
;;; Soft and hard breaks become #\Newline. Spans are listed outer first,
;;; then by start. DUMP-MARKUP writes these kinds back as markdown when they
;;; nest; other kinds (and spans that would not read back) stay in the
;;; archival ann= comment only.

(defparameter +markup-span-kinds+ '(:emphasis :strong :strike :code :link)
  "Annotation kinds that markup represents inline.")

(defun %inline-node-kind (node)
  (case (first node)
    (:emph :emphasis) (:strong :strong) (:strike :strike)
    (:code :code) (:link :link)))

(defun %write-inlines (inlines out pos spans)
  "Write INLINES' text to OUT starting at offset POS. → (values pos spans),
SPANS with this run's spans pushed (outer pushed first)."
  (dolist (x inlines (values pos spans))
    (cond
      ((stringp x)
       (write-string x out)
       (incf pos (length x)))
      ((member (first x) '(:soft-break :hard-break))
       (write-char #\Newline out)
       (incf pos))
      ((eq (first x) :code)
       (let ((s (second x)))
         (push (make-annotation-span :start pos :end (+ pos (length s)) :kind :code)
               spans)
         (write-string s out)
         (incf pos (length s))))
      (t
       (let* ((kind (%inline-node-kind x))
              (url (and (eq kind :link) (second x)))
              (children (if (eq kind :link) (third x) (second x)))
              (span (make-annotation-span :start pos :end pos :kind kind
                                          :target (and url (plusp (length url)) url))))
         (push span spans)
         (multiple-value-setq (pos spans) (%write-inlines children out pos spans))
         (setf (annotation-span-end span) pos))))))

(defun %sort-spans (spans)
  "SPANS ordered by start, an enclosing span before the spans it holds."
  (stable-sort (copy-list spans)
               (lambda (a b)
                 (or (< (annotation-span-start a) (annotation-span-start b))
                     (and (= (annotation-span-start a) (annotation-span-start b))
                          (> (annotation-span-end a) (annotation-span-end b)))))))

(defun inlines-text-spans (inlines)
  "markdown-protocol INLINES → (values text spans): the plain text and its
ANNOTATION-SPANs (kinds in +MARKUP-SPAN-KINDS+), outer first, by start."
  (let ((out (make-string-output-stream)))
    (multiple-value-bind (pos spans) (%write-inlines inlines out 0 '())
      (declare (ignore pos))
      (values (get-output-stream-string out)
              (%sort-spans (nreverse spans))))))

(defun %blocks-text-spans (blocks)
  "markdown-protocol BLOCKS flattened to (values text spans): blocks joined
by #\\Newline, lists item by item, table rows with cells joined by \" | \"."
  (let ((out (make-string-output-stream))
        (pos 0)
        (spans '())
        (first t))
    (labels ((sep ()
               (unless first
                 (write-char #\Newline out)
                 (incf pos))
               (setf first nil))
             (inl (inlines)
               (multiple-value-setq (pos spans) (%write-inlines inlines out pos spans)))
             (text (s)
               (write-string s out)
               (incf pos (length s)))
             (walk (b)
               (case (first b)
                 ((:paragraph) (sep) (inl (second b)))
                 ((:heading) (sep) (inl (third b)))
                 ((:code-block) (sep) (text (format nil "~{~a~^~%~}" (third b))))
                 ((:thematic-break) (sep) (text "---"))
                 ((:block-quote) (mapc #'walk (second b)))
                 ((:bullet-list) (dolist (item (third b)) (mapc #'walk item)))
                 ((:ordered-list) (dolist (item (fifth b)) (mapc #'walk item)))
                 ((:table)
                  (dolist (row (cons (third b) (fourth b)))
                    (sep)
                    (loop for (cell . more) on row
                          do (inl cell)
                             (when more (text " | "))))))))
      (mapc #'walk blocks))
    (values (get-output-stream-string out) (%sort-spans (nreverse spans)))))

;;; ------------------------------------------------------------------
;;; Writing: plain text with markdown punctuation escaped, and markdown
;;; spans written inline when they read back exactly.

(defun %escape-inline-char-p (ch)
  (find ch "\\`*_[]<~|!"))

(defun %escape-line-start (line out)
  "Write LINE to OUT escaping what would start a block, then the rest as
inline text."
  (let* ((n (length line))
         (ind (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line) n))
         (digits (or (position-if-not #'digit-char-p line :start ind) n)))
    (write-string line out :end ind)
    (cond
      ((and (< ind n) (find (char line ind) "#>-+=:"))
       (write-char #\\ out))
      ((and (> digits ind) (< digits n) (find (char line digits) ".)"))
       (write-string line out :start ind :end digits)
       (write-char #\\ out)
       (setf ind digits)))
    ind))

(defun markup-escape (text)
  "TEXT with markdown punctuation backslash-escaped, so PARSE-MARKUP reads
back TEXT as plain text: inline punctuation everywhere, block markers at
the start of each line."
  (with-output-to-string (out)
    (let ((line-start t))
      (loop with i = 0
            while (< i (length text))
            do (when line-start
                 (let* ((end (or (position #\Newline text :start i) (length text)))
                        (skip (%escape-line-start (subseq text i end) out)))
                   (incf i skip)
                   (setf line-start nil)))
               (when (< i (length text))
                 (let ((ch (char text i)))
                   (when (%escape-inline-char-p ch)
                     (write-char #\\ out))
                   (write-char ch out)
                   (when (char= ch #\Newline)
                     (setf line-start t))
                   (incf i)))))))

(defun %span-key (span)
  (list (annotation-span-start span) (annotation-span-end span)
        (annotation-span-kind span)
        (and (slot-boundp span 'target) (annotation-span-target span))))

(defun %markup-spans (spans length)
  "The spans of SPANS that markup writes: known kinds, non-empty, in range."
  (remove-if-not (lambda (s)
                   (and (member (annotation-span-kind s) +markup-span-kinds+)
                        (<= 0 (annotation-span-start s))
                        (< (annotation-span-start s) (annotation-span-end s))
                        (<= (annotation-span-end s) length)))
                 spans))

(defun %code-span (s)
  (let* ((longest (loop with best = 0 and run = 0
                        for ch across s
                        do (if (char= ch #\`)
                               (setf best (max best (incf run)))
                               (setf run 0))
                        finally (return best)))
         (fence (make-string (1+ longest) :initial-element #\`))
         (pad (or (and (plusp (length s))
                       (or (char= (char s 0) #\`) (char= (char s (1- (length s))) #\`)))
                  (and (> (length s) 1)
                       (char= (char s 0) #\Space)
                       (char= (char s (1- (length s))) #\Space)
                       (find #\Space s :test-not #'char=)))))
    (format nil "~a~:[~; ~]~a~:[~; ~]~a" fence pad s pad fence)))

(defun %link-destination (url)
  (if (or (zerop (length url))
          (notany (lambda (c) (or (member c '(#\Space #\Tab #\Newline #\< #\> #\( #\) #\\))))
                  url))
      url
      (format nil "<~a>" url)))

(defun %write-span-tree (text spans start end out)
  "Write TEXT[START, END) with SPANS (sorted, nested, all inside) as markdown."
  (let ((i start))
    (loop while spans
          do (let* ((s (pop spans))
                    (s-end (annotation-span-end s))
                    (inner (loop while (and spans (<= (annotation-span-end (first spans)) s-end))
                                 collect (pop spans))))
               (write-string (markup-escape (subseq text i (annotation-span-start s))) out)
               (let ((body (with-output-to-string (o)
                             (%write-span-tree text inner (annotation-span-start s) s-end o))))
                 (ecase (annotation-span-kind s)
                   (:emphasis (format out "*~a*" body))
                   (:strong (format out "**~a**" body))
                   (:strike (format out "~~~~~a~~~~" body))
                   (:code (write-string (%code-span (subseq text (annotation-span-start s) s-end))
                                        out))
                   (:link (format out "[~a](~a)" body
                                  (%link-destination
                                   (or (and (slot-boundp s 'target) (annotation-span-target s))
                                       ""))))))
               (setf i s-end)))
    (write-string (markup-escape (subseq text i end)) out)))

(defun %nested-p (spans)
  "True when sorted SPANS form a tree: each is inside or after the ones
before it, with nothing inside a code span or inside a link's link."
  (let ((open '()))
    (dolist (s spans t)
      (loop while (and open (<= (annotation-span-end (first open)) (annotation-span-start s)))
            do (pop open))
      (when (and open
                 (or (> (annotation-span-end s) (annotation-span-end (first open)))
                     (eq (annotation-span-kind (first open)) :code)
                     (and (eq (annotation-span-kind s) :link)
                          (find :link open :key #'annotation-span-kind))))
        (return nil))
      (push s open))))

(defun markup-inline (text &optional spans)
  "TEXT as markdown inline text with the markup kinds among SPANS written as
markdown (* ** ~~ ` [](…)). Falls back to MARKUP-ESCAPE of TEXT when the
spans do not nest or would not read back as exactly TEXT and those spans."
  (let* ((text (or text ""))
         (mine (%sort-spans (%markup-spans spans (length text))))
         (plain (markup-escape text)))
    (if (or (null mine) (not (%nested-p mine)))
        plain
        (let* ((candidate (with-output-to-string (o)
                            (%write-span-tree text mine 0 (length text) o)))
               (blocks (markdown-protocol:parse-markdown candidate)))
          (if (and (= 1 (length blocks))
                   (eq :paragraph (first (first blocks)))
                   (multiple-value-bind (back back-spans)
                       (inlines-text-spans (second (first blocks)))
                     (and (string= back text)
                          (equal (mapcar #'%span-key back-spans)
                                 (mapcar #'%span-key mine)))))
              candidate
              plain)))))
