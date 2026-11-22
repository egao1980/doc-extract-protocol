(in-package #:doc-extract-protocol)

;;; Octets → text for extraction sources (encoding-protocol).
;;; Order: *SOURCE-ENCODING* (caller), byte-order mark, declared charset
;;; (<?xml encoding=…?>, <meta charset=…>, http-equiv content=…charset=…),
;;; then UTF-8. Undeclared text that is not valid UTF-8 falls back to
;;; windows-1252's ISO-8859-1 subset (the HTML legacy default) with a warning.

(defvar *source-encoding* nil
  "NIL (detect) or an encoding designator (:utf-8, \"iso-8859-1\", …) used to
   decode octet and pathname sources, e.g. from an HTTP Content-Type charset.")

(defun %bom-encoding (octets)
  "(values encoding bom-length) for a UTF-8/UTF-16 byte-order mark, or NIL."
  (let ((n (length octets)))
    (flet ((at (i) (aref octets i)))
      (cond ((and (>= n 3) (= (at 0) #xEF) (= (at 1) #xBB) (= (at 2) #xBF))
             (values :utf-8 3))
            ((and (>= n 2) (= (at 0) #xFE) (= (at 1) #xFF)) (values :utf-16be 2))
            ((and (>= n 2) (= (at 0) #xFF) (= (at 1) #xFE)) (values :utf-16le 2))
            (t nil)))))

(defun %ascii-prefix (octets &optional (limit 1024))
  "The first LIMIT octets as a string, non-ASCII bytes mapped to #\\?."
  (let* ((n (min limit (length octets)))
         (s (make-string n)))
    (dotimes (i n s)
      (let ((b (aref octets i)))
        (setf (char s i) (if (< b #x80) (code-char b) #\?))))))

(defun %value-after (head key)
  "Token after KEY (case-insensitive) in HEAD, unquoted, or NIL."
  (let ((pos (search key head :test #'char-equal)))
    (when pos
      (let* ((start (+ pos (length key)))
             (start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\" #\' #\=)))
                                         head :start start)
                        (length head)))
             (end (or (position-if (lambda (c) (member c '(#\" #\' #\Space #\Tab #\; #\> #\/ #\?)))
                                   head :start start)
                      (length head))))
        (and (< start end) (subseq head start end))))))

(defun declared-charset (octets)
  "Charset declared in the first KB of OCTETS (XML declaration or HTML meta), or NIL."
  (let ((head (%ascii-prefix octets)))
    (or (and (search "<?xml" head :test #'char-equal :end2 (min 64 (length head)))
             (%value-after head "encoding"))
        (%value-after head "charset"))))

(defun decode-source-octets (octets &key (encoding *source-encoding*))
  "Decode OCTETS (an extraction source) to a string through encoding-protocol,
   honouring ENCODING, then a BOM, then a declared charset, then UTF-8."
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (multiple-value-bind (bom-encoding bom-length) (%bom-encoding octets)
      (let ((body (if bom-length (subseq octets bom-length) octets))
            (explicit (or encoding bom-encoding (declared-charset octets))))
        (flet ((decode-as (enc)
                 (handler-case
                     (if (eq (encoding-protocol:normalize-encoding enc) :utf-8)
                         (encoding-protocol:decode body :encoding :utf-8 :strict t)
                         (encoding-protocol:decode body :encoding enc))
                   (encoding-protocol:encoding-error (e)
                     (error 'doc-extract-error
                            :message (format nil "cannot decode source as ~A: ~A" enc e))))))
          (if explicit
              (decode-as explicit)
              (handler-case (encoding-protocol:decode body :encoding :utf-8 :strict t)
                (encoding-protocol:encoding-error ()
                  (warn "doc-extract: source is not valid UTF-8 and declares no charset; decoding as ISO-8859-1")
                  (encoding-protocol:decode body :encoding :iso-8859-1)))))))))

(defun read-source-octets (pathname)
  "Contents of PATHNAME as an octet vector."
  (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (subseq buf 0 (read-sequence buf s)))))
