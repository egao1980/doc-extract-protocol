(in-package #:doc-extract-protocol)

(defun %collect-xml-tag-text (xml local-name)
  "Collect concatenated text of every <LOCAL-NAME>…</…> element (no xml-protocol)."
  (let ((needle (concatenate 'string "<" local-name))
        (parts '())
        (i 0))
    (loop
      (let ((open (search needle xml :start2 i :test #'char-equal)))
        (unless open (return))
        (let ((gt (position #\> xml :start open)))
          (unless gt (return))
          (cond
            ((and (> gt 0) (char= (char xml (1- gt)) #\/))
             (setf i (1+ gt)))
            (t
             (let* ((qname-end (or (position-if (lambda (c)
                                                  (or (char= c #\Space)
                                                      (char= c #\>)))
                                                xml :start (1+ open))
                                   gt))
                    (qname (subseq xml (1+ open) qname-end))
                    (close-tag (format nil "</~a>" qname))
                    (close (search close-tag xml :start2 gt :test #'char-equal)))
               (if close
                   (progn
                     (push (string-trim '(#\Space #\Tab #\Newline)
                                        (%strip-inner-tags (subseq xml (1+ gt) close)))
                           parts)
                     (setf i (+ close (length close-tag))))
                   (setf i (1+ gt)))))))))
    (nreverse (remove-if (lambda (s) (zerop (length s))) parts))))

(defun %strip-inner-tags (s)
  (with-output-to-string (o)
    (loop with i = 0
          while (< i (length s))
          do (if (char= (char s i) #\<)
                 (let ((gt (position #\> s :start (1+ i))))
                   (setf i (if gt (1+ gt) (length s))))
                 (progn (write-char (char s i) o) (incf i))))))

(defun %docx-heading-level (xml)
  "Scan for w:pStyle val=HeadingN near the start of a paragraph blob."
  (let ((pos (search "Heading" xml :test #'char-equal)))
    (when pos
      (let ((n (and (< (+ pos 7) (length xml))
                    (digit-char-p (char xml (+ pos 7))))))
        n))))

(defun office-text-from-xml (xml format)
  (cond
    ((member format '(:docx :docx-xml) :test #'eq)
     (format nil "~{~a~^ ~}" (%collect-xml-tag-text xml "w:t")))
    ((member format '(:xlsx :xlsx-xml) :test #'eq)
     (format nil "~{~a~^ ~}"
             (append (%collect-xml-tag-text xml "t")
                     (%collect-xml-tag-text xml "v"))))
    (t "")))

(defun office-sections-from-xml (xml format)
  (cond
    ((member format '(:docx :docx-xml) :test #'eq)
     (let ((paras (%collect-xml-tag-text xml "w:p"))
           (sections '())
           (current nil))
       (declare (ignore paras))
       ;; Split on paragraphs by walking w:p blocks.
       (let ((i 0) (parts '()))
         (loop
           (let ((open (search "<w:p" xml :start2 i :test #'char-equal)))
             (unless open (return))
             (let ((close (search "</w:p>" xml :start2 open :test #'char-equal)))
               (unless close (return))
               (push (subseq xml open (+ close 6)) parts)
               (setf i (+ close 6)))))
         (dolist (p (nreverse parts))
           (let ((lvl (%docx-heading-level p))
                 (text (office-text-from-xml p :docx-xml)))
             (cond
               (lvl
                (setf current (make-extracted-section :title text :text "" :level lvl))
                (push current sections))
               (current
                (setf (section-text current)
                      (string-trim
                       '(#\Space)
                       (concatenate 'string (section-text current) " " text))))))))
       (nreverse sections)))
    (t
     (list (make-extracted-section
            :title ""
            :text (office-text-from-xml xml format)
            :level 1)))))

(defun %zip-entry-string (archive name)
  (let* ((pkg (find-package :compression-protocol))
         (read (and pkg (find-symbol "READ-ENTRY" pkg)))
         (entries (and pkg (find-symbol "ARCHIVE-ENTRIES" pkg)))
         (ename (and pkg (find-symbol "ARCHIVE-ENTRY-NAME" pkg))))
    (unless (and read entries)
      (return-from %zip-entry-string nil))
    (let* ((list (funcall entries archive))
           (ent (find name list :key ename :test #'string=)))
      (when ent
        (let ((octets (funcall read archive ent)))
          (map 'string #'code-char octets))))))

(defun %open-zip (source)
  (let* ((pkg (find-package :compression-protocol))
         (open (and pkg (find-symbol "OPEN-ARCHIVE" pkg))))
    (when (and open (fboundp open))
      (funcall open source :format :zip))))

(defun office-extract-from-zip (source format)
  (unless (zip-available-p)
    (error 'doc-extract-unsupported :format format
           :message "compression-protocol zip not loaded — pass unzipped XML as :docx-xml / :xlsx-xml"))
  (let ((archive (%open-zip source)))
    (unless archive
      (error 'doc-extract-error :format format :message "could not open zip"))
    (ecase format
      (:docx
       (or (%zip-entry-string archive "word/document.xml")
           (error 'doc-extract-error :format :docx
                  :message "word/document.xml missing")))
      (:xlsx
       (or (%zip-entry-string archive "xl/sharedStrings.xml")
           (%zip-entry-string archive "xl/workbook.xml")
           (error 'doc-extract-error :format :xlsx
                  :message "xlsx workbook strings missing"))))))

(defun office-extract-text (source &key format)
  (let* ((fmt (or format :docx))
         (xml (if (member fmt '(:docx-xml :xlsx-xml) :test #'eq)
                  (%source-string source)
                  (office-extract-from-zip source fmt))))
    (office-text-from-xml xml fmt)))

(defun office-extract-sections (source &key format)
  (let* ((fmt (or format :docx))
         (xml (if (member fmt '(:docx-xml :xlsx-xml) :test #'eq)
                  (%source-string source)
                  (office-extract-from-zip source fmt))))
    (office-sections-from-xml xml fmt)))

(defun office-extract-metadata (source &key format)
  (declare (ignore source))
  (list :format (or format :docx)))

(register-extractor 'default-doc-extract-backend
                    :formats '(:docx :xlsx :docx-xml :xlsx-xml
                               "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
                    :priority 10)
