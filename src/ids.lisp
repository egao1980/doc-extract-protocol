(in-package #:doc-extract-protocol)

;;; Content-addressed block ids. Core stays dep-free: *BLOCK-DIGEST-FN* is
;;; the hook (octets → hex string). Default = portable SHA-256.

(defvar *block-digest-fn* nil
  "When bound, a function of (octets) → lowercase hex digest string.")

(defun utf8-encode (string)
  "STRING → UTF-8 octet vector. Characters are Unicode code points."
  (let ((out (make-array (length string)
                         :element-type '(unsigned-byte 8)
                         :adjustable t
                         :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for cp = (char-code (char string i))
          do (cond
               ((< cp #x80)
                (vector-push-extend cp out))
               ((< cp #x800)
                (vector-push-extend (logior #xC0 (ash cp -6)) out)
                (vector-push-extend (logior #x80 (logand cp #x3F)) out))
               ((< cp #x10000)
                (vector-push-extend (logior #xE0 (ash cp -12)) out)
                (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
                (vector-push-extend (logior #x80 (logand cp #x3F)) out))
               (t
                (vector-push-extend (logior #xF0 (ash cp -18)) out)
                (vector-push-extend (logior #x80 (logand (ash cp -12) #x3F)) out)
                (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
                (vector-push-extend (logior #x80 (logand cp #x3F)) out))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun utf8-decode (octets)
  (let ((out (make-array (length octets)
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0))
        (n (length octets))
        (i 0))
    (loop while (< i n)
          do (let ((b (aref octets i)))
               (cond
                 ((< b #x80)
                  (vector-push-extend (code-char b) out)
                  (incf i))
                 ((< b #xE0)
                  (vector-push-extend
                   (code-char (logior (ash (logand b #x1F) 6)
                                      (logand (aref octets (1+ i)) #x3F)))
                   out)
                  (incf i 2))
                 ((< b #xF0)
                  (vector-push-extend
                   (code-char (logior (ash (logand b #x0F) 12)
                                      (ash (logand (aref octets (1+ i)) #x3F) 6)
                                      (logand (aref octets (+ i 2)) #x3F)))
                   out)
                  (incf i 3))
                 (t
                  (vector-push-extend
                   (code-char (logior (ash (logand b #x07) 18)
                                      (ash (logand (aref octets (1+ i)) #x3F) 12)
                                      (ash (logand (aref octets (+ i 2)) #x3F) 6)
                                      (logand (aref octets (+ i 3)) #x3F)))
                   out)
                  (incf i 4)))))
    (coerce out 'string)))

(defun %u32 (n)
  (logand n #xFFFFFFFF))

(defun %rotr32 (n k)
  (let ((n (%u32 n)))
    (%u32 (logior (ash n (- k)) (ash n (- 32 k))))))

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
    #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3
    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
    #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
    #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
    #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
    #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
    #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %sha256-process (h chunk)
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i 16)
      (setf (aref w i)
            (logior (ash (aref chunk (* i 4)) 24)
                    (ash (aref chunk (+ (* i 4) 1)) 16)
                    (ash (aref chunk (+ (* i 4) 2)) 8)
                    (aref chunk (+ (* i 4) 3)))))
    (loop for i from 16 below 64
          for s0 = (logxor (%rotr32 (aref w (- i 15)) 7)
                           (%rotr32 (aref w (- i 15)) 18)
                           (ash (aref w (- i 15)) -3))
          for s1 = (logxor (%rotr32 (aref w (- i 2)) 17)
                           (%rotr32 (aref w (- i 2)) 19)
                           (ash (aref w (- i 2)) -10))
          do (setf (aref w i)
                   (%u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
    (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
      (dotimes (i 64)
        (let* ((s1 (logxor (%rotr32 e 6) (%rotr32 e 11) (%rotr32 e 25)))
               (ch (logxor (logand e f) (logand (logand (lognot e) #xFFFFFFFF) g)))
               (temp1 (%u32 (+ hh s1 ch (aref +sha256-k+ i) (aref w i))))
               (s0 (logxor (%rotr32 a 2) (%rotr32 a 13) (%rotr32 a 22)))
               (maj (logxor (logand a b) (logand a c) (logand b c)))
               (temp2 (%u32 (+ s0 maj))))
          (setf hh g g f f e e (%u32 (+ d temp1))
                d c c b b a a (%u32 (+ temp1 temp2)))))
      (setf (aref h 0) (%u32 (+ (aref h 0) a))
            (aref h 1) (%u32 (+ (aref h 1) b))
            (aref h 2) (%u32 (+ (aref h 2) c))
            (aref h 3) (%u32 (+ (aref h 3) d))
            (aref h 4) (%u32 (+ (aref h 4) e))
            (aref h 5) (%u32 (+ (aref h 5) f))
            (aref h 6) (%u32 (+ (aref h 6) g))
            (aref h 7) (%u32 (+ (aref h 7) hh))))
    h))

(defun sha256-octets (octets)
  (let* ((len (length octets))
         (bitlen (* len 8))
         (padlen (mod (- 56 (mod (+ len 1) 64)) 64))
         (total (+ len 1 padlen 8))
         (buf (make-array total :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (h (make-array 8 :element-type '(unsigned-byte 32)
                        :initial-contents
                        '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                          #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))))
    (replace buf octets)
    (setf (aref buf len) #x80)
    (loop for i from 0 below 8
          do (setf (aref buf (- total 1 i))
                   (ldb (byte 8 (* i 8)) bitlen)))
    (loop for off from 0 below total by 64
          do (%sha256-process h (subseq buf off (+ off 64))))
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 8)
        (let ((w (aref h i)))
          (setf (aref out (* i 4)) (ldb (byte 8 24) w)
                (aref out (+ (* i 4) 1)) (ldb (byte 8 16) w)
                (aref out (+ (* i 4) 2)) (ldb (byte 8 8) w)
                (aref out (+ (* i 4) 3)) (ldb (byte 8 0) w))))
      out)))

(defun %bytes-hex (octets)
  (with-output-to-string (s)
    (loop for b across octets
          do (format s "~(~2,'0x~)" b))))

(defun portable-digest (octets)
  "Portable SHA-256 → lowercase hex. OCTETS is a byte vector or a string."
  (%bytes-hex
   (sha256-octets (if (stringp octets)
                      (utf8-encode octets)
                      octets))))

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
