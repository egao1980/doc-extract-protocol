(in-package #:doc-extract-protocol)

;;; Content-addressed block ids. *BLOCK-DIGEST-FN* is the hook
;;; (octets → hex string). Default = SHA-256 through crypto-protocol.

(defvar *block-digest-fn* nil
  "When bound, a function of (octets) → lowercase hex digest string.")

(defun utf8-encode (string)
  "STRING → UTF-8 octet vector (encoding-protocol)."
  (coerce (encoding-protocol:encode string :encoding :utf-8)
          '(simple-array (unsigned-byte 8) (*))))

(defun utf8-decode (octets)
  "UTF-8 OCTETS → string, strict (encoding-protocol)."
  (encoding-protocol:decode octets :encoding :utf-8 :strict t))

(defun portable-digest (octets)
  "SHA-256 → lowercase hex (crypto-protocol). OCTETS is a byte vector or a string."
  (string-downcase
   (encoding-protocol:encode
    (crypto-protocol:digest (if (stringp octets)
                                (utf8-encode octets)
                                (coerce octets '(simple-array (unsigned-byte 8) (*))))
                            :algorithm :sha256)
    :encoding :base16)))

(defun digest-octets (octets)
  (if *block-digest-fn*
      (funcall *block-digest-fn* octets)
      (portable-digest octets)))

(defun %normalize-id-text (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or s "")))

(defun %id-payload (block child-ids)
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-readably* nil)
          (*print-circle* nil)
          (*package* (find-package :keyword)))
      (prin1-to-string
       (list (block-node-type block)
             (when (typep block 'text-block) (text-block-kind block))
             (%normalize-id-text (block-plain-text block))
             child-ids)))))

(defun compute-block-id (block)
  "SHA-256 over (kind, normalized-text, child-ids). Deterministic."
  (let ((child-ids (mapcar (lambda (c)
                             (or (block-id c) (compute-block-id c)))
                           (block-children block))))
    (digest-octets (utf8-encode (%id-payload block child-ids)))))

(defun %ensure-block-ids (block)
  (dolist (c (block-children block))
    (%ensure-block-ids c))
  (setf (block-id block) (compute-block-id block))
  (block-id block))

(defun ensure-ids (document)
  "Walk DOCUMENT bottom-up and assign deterministic content-hash ids."
  (check-type document extracted-document)
  (dolist (b (extracted-document-blocks document))
    (%ensure-block-ids b))
  (when (slot-boundp document 'id-index)
    (setf (extracted-document-id-index document) nil))
  (when (slot-boundp document 'text-cache)
    (setf (extracted-document-text-cache document) nil))
  document)
