(in-package #:doc-extract-protocol/tests)

;;; parse-markup over markdown-protocol: inline spans as annotation-spans,
;;; plain text unchanged, and dump-markup writing them back.

(defun %blocks (md)
  (doc-extract-protocol:extracted-document-blocks
   (doc-extract-protocol:parse-markup md)))

(defun %only-text (md)
  "The single text block MD parses to."
  (let ((blocks (%blocks md)))
    (assert (and (= 1 (length blocks))
                 (typep (first blocks) 'doc-extract-protocol:text-block)))
    (first blocks)))

(defun %spans (block)
  "BLOCK's annotations as (start end kind target)."
  (mapcar (lambda (s)
            (list (doc-extract-protocol:annotation-span-start s)
                  (doc-extract-protocol:annotation-span-end s)
                  (doc-extract-protocol:annotation-span-kind s)
                  (and (slot-boundp s 'doc-extract-protocol::target)
                       (doc-extract-protocol:annotation-span-target s))))
          (doc-extract-protocol:text-block-annotations block)))

(defun %text (block) (doc-extract-protocol:block-plain-text block))

(deftest markup-span-kinds
  (let ((b (%only-text "an *emphasis* word")))
    (ok (equal "an emphasis word" (%text b)))
    (ok (equal "an emphasis word" (doc-extract-protocol:text-block-text b)))
    (ok (equal '((3 11 :emphasis nil)) (%spans b)) "emphasis"))
  (let ((b (%only-text "a **strong** one")))
    (ok (equal "a strong one" (%text b)))
    (ok (equal '((2 8 :strong nil)) (%spans b)) "strong"))
  (let ((b (%only-text "call `(f x)` now")))
    (ok (equal "call (f x) now" (%text b)))
    (ok (equal '((5 10 :code nil)) (%spans b)) "code"))
  (let ((b (%only-text "see [the docs](https://example.com/d) here")))
    (ok (equal "see the docs here" (%text b)))
    (ok (equal '((4 12 :link "https://example.com/d")) (%spans b)) "link, url as target"))
  (let ((b (%only-text "<https://x.y>")))
    (ok (equal "https://x.y" (%text b)))
    (ok (equal '((0 11 :link "https://x.y")) (%spans b)) "autolink"))
  (let ((b (%only-text "was ~~gone~~")))
    (ok (equal "was gone" (%text b)))
    (ok (equal '((4 8 :strike nil)) (%spans b)) "strike")))

(deftest markup-span-nesting-and-offsets
  (let ((b (%only-text "**bold *both* x** and [*em* link](u)")))
    (ok (equal "bold both x and em link" (%text b)))
    (ok (equal '((0 11 :strong nil) (5 9 :emphasis nil)
                 (16 23 :link "u") (16 18 :emphasis nil))
               (%spans b))
        "outer span first, then by start"))
  (let ((b (%only-text "café *日本* naïve")))
    (ok (equal "café 日本 naïve" (%text b)))
    (ok (equal '((5 7 :emphasis nil)) (%spans b)) "code-point offsets"))
  (let ((b (%only-text (format nil "one *two~%three*"))))
    (ok (equal (format nil "one two~%three") (%text b)) "a soft break is a newline")
    (ok (equal '((4 13 :emphasis nil)) (%spans b))))
  (let ((b (%only-text "plain \\*not\\* emphasis")))
    (ok (equal "plain *not* emphasis" (%text b)))
    (ok (null (%spans b)) "escaped punctuation is text")))

(deftest markup-spans-in-containers
  (let ((q (%only-text "> a *quoted* line")))
    (ok (eq :quote (doc-extract-protocol:text-block-kind q)))
    (ok (equal "a quoted line" (%text q)))
    (ok (equal '((2 8 :emphasis nil)) (%spans q))))
  (let* ((l (first (%blocks (format nil "- one `x`~%- [two](t)~%"))))
         (items (doc-extract-protocol:list-block-items l)))
    (ok (typep l 'doc-extract-protocol:list-block))
    (ok (equal '("one x" "two") (mapcar #'%text items)))
    (ok (equal '(((4 5 :code nil)) ((0 3 :link "t"))) (mapcar #'%spans items))))
  (let* ((tb (first (%blocks (format nil "| **h** | k |~%|---|---|~%| `v` | w |~%"))))
         (cells (doc-extract-protocol:table-block-cells tb))
         (texts (mapcar (lambda (c) (first (doc-extract-protocol:table-cell-content c))) cells)))
    (ok (equal '("h" "k" "v" "w") (mapcar #'%text texts)))
    (ok (equal '(((0 1 :strong nil)) nil ((0 1 :code nil)) nil) (mapcar #'%spans texts)))
    (ok (equal '(t t nil nil) (mapcar #'doc-extract-protocol:table-cell-header-p cells))))
  (let ((s (first (%blocks "# A *styled* title"))))
    (ok (equal "A styled title" (doc-extract-protocol:section-title s))
        "a section title is plain text")))

(deftest markup-plain-text-accessors
  (let* ((doc (doc-extract-protocol:parse-markup
               (format nil "# T~%~%Some *x* and `y`.~%~%- [z](u)~%")))
         (text (doc-extract-protocol:document-text doc)))
    (ok (search "Some x and y." text))
    (ok (search "z" text))
    (ng (search "*" text))
    (ng (search "`" text))))

(deftest markup-keeps-model-blocks
  (let ((blocks (%blocks (format nil "![fig](hash:abc)~%~%**Name**: Ada~%~%```lisp~%(+ 1 2)~%```~%"))))
    (ok (typep (first blocks) 'doc-extract-protocol:image-block))
    (ok (equal "fig" (doc-extract-protocol:image-block-alt (first blocks))))
    (ok (equal "abc" (doc-extract-protocol:image-block-content-hash (first blocks))))
    (ok (typep (second blocks) 'doc-extract-protocol:key-value-block))
    (ok (equal '("Name" "Ada") (list (doc-extract-protocol:key-value-block-key (second blocks))
                                     (doc-extract-protocol:key-value-block-value (second blocks)))))
    (ok (equal "lisp" (doc-extract-protocol:code-block-language (third blocks))))
    (ok (equal "(+ 1 2)" (doc-extract-protocol:code-block-text (third blocks))))))

(deftest markup-comments-around-markdown
  (let* ((md (format nil "<!-- {#L type=list} -->~%<!-- {#a type=text} -->~%- *a*~%<!-- {#b type=text} -->~%- b~%~%"))
         (l (first (%blocks md))))
    (ok (typep l 'doc-extract-protocol:list-block) "item comments do not split the list")
    (ok (equal "L" (doc-extract-protocol:block-id l)))
    (ok (equal '("a" "b") (mapcar #'doc-extract-protocol:block-id
                                  (doc-extract-protocol:list-block-items l))))
    (ok (equal '((0 1 :emphasis nil)) (%spans (first (doc-extract-protocol:list-block-items l))))))
  (let ((b (%only-text (format nil "<!-- {#p type=text ann=0-2:entity} -->~%*ab* c~%"))))
    (ok (equal "ab c" (%text b)))
    (ok (equal '((0 2 :entity nil)) (%spans b)) "an ann= comment gives the annotations"))
  (let ((c (first (%blocks (format nil "```~%<!-- {#x type=text} -->~%```~%")))))
    (ok (equal "<!-- {#x type=text} -->" (doc-extract-protocol:code-block-text c))
        "comment-like lines inside fenced code are code")))

(defun %span-doc (text spans)
  (doc-extract-protocol:ensure-ids
   (doc-extract-protocol:make-extracted-document
    :blocks (list (doc-extract-protocol:make-text-block
                   :text text
                   :annotations (loop for (s e k tg) in spans
                                      collect (doc-extract-protocol:make-annotation-span
                                               :start s :end e :kind k :target tg)))))))

(deftest markup-dump-writes-spans
  (dolist (case '(("an emphasis word" ((3 11 :emphasis nil)) "an *emphasis* word")
                  ("a strong one" ((2 8 :strong nil)) "a **strong** one")
                  ("call (f x) now" ((5 10 :code nil)) "call `(f x)` now")
                  ("see the docs" ((4 12 :link "https://e.x/d")) "see [the docs](https://e.x/d)")
                  ("was gone" ((4 8 :strike nil)) "was ~~gone~~")))
    (destructuring-bind (text spans expected) case
      (let* ((md (doc-extract-protocol:dump-markup (%span-doc text spans)))
             (back (%only-text md)))
        (ok (search expected md) (format nil "dump writes ~s" expected))
        (ok (equal text (%text back)))
        (ok (equal spans (%spans back)) (format nil "~s reads back" expected))))))

(deftest markup-dump-escapes-text
  (dolist (text '("2 * 3 * 4 and a_b_c" "[not a link](x)" "`tick` <b> ~~no~~ | pipe"
                  "# not a heading" "- not a list" "1. not ordered" "> not a quote"
                  "!bang \\ backslash"))
    (let ((back (%only-text (doc-extract-protocol:dump-markup (%span-doc text nil)))))
      (ok (equal text (%text back)) (format nil "~s reads back as text" text))
      (ok (null (%spans back)))))
  (let* ((doc (%span-doc " spaced" '((0 3 :emphasis nil))))
         (md (doc-extract-protocol:dump-markup doc)))
    (ng (search "*" md) "a span that would not read back is not written as markup")))
