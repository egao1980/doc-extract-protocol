(in-package #:doc-extract-protocol)

(defun %decode-entities (s)
  (let ((out (make-array (length s) :element-type 'character :fill-pointer 0)))
    (loop with i = 0
          while (< i (length s))
          do (if (char= (char s i) #\&)
                 (let ((semi (position #\; s :start (1+ i))))
                   (if (and semi (< (- semi i) 12))
                       (let ((ent (subseq s (1+ i) semi)))
                         (vector-push-extend
                          (cond
                            ((string-equal ent "amp") #\&)
                            ((string-equal ent "lt") #\<)
                            ((string-equal ent "gt") #\>)
                            ((string-equal ent "quot") #\")
                            ((string-equal ent "nbsp") #\Space)
                            ((and (plusp (length ent)) (char= (char ent 0) #\#))
                             (code-char
                              (if (and (> (length ent) 1)
                                       (char-equal (char ent 1) #\x))
                                  (parse-integer ent :start 2 :radix 16 :junk-allowed t)
                                  (parse-integer ent :start 1 :junk-allowed t))))
                            (t #\&))
                          out)
                         (setf i (1+ semi)))
                       (progn (vector-push-extend #\& out) (incf i))))
                 (progn (vector-push-extend (char s i) out) (incf i))))
    (coerce out 'string)))

(defun %strip-scripts (html)
  (let ((s html))
    (dolist (tag '("script" "style"))
      (loop
        (let ((open (search (format nil "<~a" tag) s :test #'char-equal)))
          (unless open (return))
          (let ((close (search (format nil "</~a>" tag) s :start2 open :test #'char-equal)))
            (setf s (concatenate 'string
                                 (subseq s 0 open)
                                 (if close
                                     (subseq s (+ close (length tag) 3))
                                     "")))))))
    s))

(defun %heading-level (tag)
  (when (and (= (length tag) 2)
             (char-equal (char tag 0) #\h)
             (digit-char-p (char tag 1)))
    (digit-char-p (char tag 1))))

(defun html-extract-via-protocol (source)
  (let* ((pkg (find-package :html-protocol))
         (parse (and pkg (find-symbol "PARSE" pkg)))
         (element-text (and pkg (find-symbol "ELEMENT-TEXT" pkg)))
         (element-p (and pkg (find-symbol "HTML-ELEMENT-P" pkg)))
         (element-name (and pkg (find-symbol "HTML-ELEMENT-NAME" pkg)))
         (element-children (and pkg (find-symbol "HTML-ELEMENT-CHILDREN" pkg)))
         (doc-p (and pkg (find-symbol "HTML-DOCUMENT-P" pkg)))
         (doc-children (and pkg (find-symbol "HTML-DOCUMENT-CHILDREN" pkg))))
    (unless (and parse (fboundp parse) (html-backend-bound-p))
      (return-from html-extract-via-protocol nil))
    (let ((doc (funcall parse source))
          (text-parts '())
          (sections '())
          (current nil))
      (labels ((walk (node)
                 (cond
                   ((and doc-p (funcall doc-p node))
                    (dolist (c (funcall doc-children node)) (walk c)))
                   ((and element-p (funcall element-p node))
                    (let* ((name (funcall element-name node))
                           (lvl (%heading-level name)))
                      (when lvl
                        (let ((title (string-trim '(#\Space #\Tab #\Newline)
                                                  (funcall element-text node))))
                          (push (make-extracted-section :title title :text "" :level lvl)
                                sections)
                          (setf current (first sections))))
                      (unless (member name '("script" "style") :test #'string-equal)
                        (dolist (c (funcall element-children node))
                          (walk c)))))
                   (t
                    (when (and element-text (fboundp element-text))
                      (let ((txt (ignore-errors (funcall element-text node))))
                        (when (and txt (plusp (length txt)))
                          (push txt text-parts)
                          (when current
                            (setf (section-text current)
                                  (concatenate 'string (section-text current)
                                               " " txt))))))))))
        (walk doc))
      (list (string-trim '(#\Space #\Tab #\Newline)
                         (format nil "~{~a~^ ~}" (nreverse text-parts)))
            (nreverse sections)))))

(defun html-extract-via-strip (source)
  (let* ((html (%strip-scripts (%source-string source)))
         (sections '())
         (current nil)
         (text (make-array 256 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop with i = 0
          with in-heading = nil
          while (< i (length html))
          do (if (char= (char html i) #\<)
                 (let ((gt (position #\> html :start (1+ i))))
                   (unless gt (return))
                   (let* ((raw (subseq html (1+ i) gt))
                          (closing (and (plusp (length raw)) (char= (char raw 0) #\/)))
                          (name (string-downcase
                                 (string-left-trim
                                  "/"
                                  (subseq raw 0 (or (position-if
                                                     (lambda (c)
                                                       (or (char= c #\Space)
                                                           (char= c #\/)))
                                                     raw)
                                                    (length raw))))))
                          (lvl (%heading-level name)))
                     (when (member name '("p" "div" "br" "li" "tr") :test #'string=)
                       (vector-push-extend #\Space text))
                     (cond
                       ((and lvl (not closing))
                        (setf current (make-extracted-section :title "" :text "" :level lvl)
                              in-heading t)
                        (push current sections))
                       ((and lvl closing)
                        (setf in-heading nil)))
                     (setf i (1+ gt))))
                 (progn
                   (vector-push-extend (char html i) text)
                   (when current
                     (if in-heading
                         (setf (section-title current)
                               (concatenate 'string (section-title current)
                                            (string (char html i))))
                         (setf (section-text current)
                               (concatenate 'string (section-text current)
                                            (string (char html i))))))
                   (incf i))))
    (dolist (sec sections)
      (setf (section-title sec)
            (string-trim '(#\Space #\Tab #\Newline) (section-title sec))))
    (list (string-trim '(#\Space #\Tab #\Newline)
                       (%decode-entities (coerce text 'string)))
          (nreverse sections))))

(defun html-extract-text (source)
  (or (car (html-extract-via-protocol source))
      (car (html-extract-via-strip source))))

(defun html-extract-sections (source)
  (or (cadr (html-extract-via-protocol source))
      (cadr (html-extract-via-strip source))))

(defun html-extract-metadata (source)
  (let* ((s (%source-string source))
         (title nil))
    (let ((open (search "<title" s :test #'char-equal)))
      (when open
        (let ((gt (position #\> s :start open)))
          (when gt
            (let ((close (search "</title>" s :start2 gt :test #'char-equal)))
              (when close
                (setf title (string-trim '(#\Space #\Tab #\Newline)
                                         (%decode-entities (subseq s (1+ gt) close))))))))))
    (list :title title :format :html)))

(register-extractor 'default-doc-extract-backend
                    :formats '(:html :htm "text/html" "application/xhtml+xml")
                    :priority 10)
