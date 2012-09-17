(require 'string-utils)

(expectations

  (desc "string-utils-stringify-anything")

  (expect "123"
    (string-utils-stringify-anything 123))

  (expect "1.2"
    (string-utils-stringify-anything 1.2))

  (expect ""
    (string-utils-stringify-anything nil))

  (expect "symbol"
    (string-utils-stringify-anything 'symbol))

  (expect "1 2"
    (string-utils-stringify-anything (list 1 2)))

  (expect "1 2"
    (string-utils-stringify-anything (cons 1 2)))

  (expect "123"
    (string-utils-stringify-anything (list 1 2 3) ""))

  (expect "1 2 3 4"
    (string-utils-stringify-anything '(1 2 (3 4))))

  (expect "1 2 3 4"
    (string-utils-stringify-anything '[1 2 [3 4]]))

  (expect "1 2 3 string"
    (string-utils-stringify-anything '[1 2 [3 string]]))

  (expect 0
    (string-match-p "\\`[0-9.-]+\\'" (string-utils-stringify-anything (make-random-state) "")))

  (expect "t t t t t t t t t t"
    (string-utils-stringify-anything (make-bool-vector 10 1)))

  (expect ""
    (string-utils-stringify-anything (make-bool-vector 10 nil) ""))

  ;; not really sure if order is guaranteed here
  (expect "one 1 two 2"
    (let ((tester (make-hash-table)))
      (puthash "one" 1 tester)
      (puthash "two" 2 tester)
      (string-utils-stringify-anything tester)))

  (expect ""
    (string-utils-stringify-anything (make-byte-code nil nil nil nil) ""))

  (expect "args...0"
    (string-utils-stringify-anything (make-byte-code '(args) nil nil 0) "."))

  (expect "1 *scratch*"
    (with-current-buffer "*scratch*"
      (let ((tester (make-marker)))
        (move-marker tester 1)
        (string-utils-stringify-anything tester))))

  (expect ""
    (string-utils-stringify-anything (make-marker) ""))

  (expect "1 1 *scratch*"
    (with-current-buffer "*scratch*"
      (let ((tester (make-overlay 1 1)))
        (string-utils-stringify-anything tester))))

  (expect ""
    (with-current-buffer "*scratch*"
      (let ((tester (make-overlay 1 1)))
        (delete-overlay tester)
        (string-utils-stringify-anything tester ""))))

  (expect ""
    (string-utils-stringify-anything (make-sparse-keymap)))

  (expect "97 ignore"
    (let ((tester (make-sparse-keymap)))
       (define-key tester (kbd "a") 'ignore)
       (string-utils-stringify-anything tester)))

  (expect "99 98 ignore 97 ignore"
    (let ((tester  (make-sparse-keymap))
          (tester2 (make-sparse-keymap)))
       (define-key tester  (kbd "a") 'ignore)
       (define-key tester2 (kbd "b") 'ignore)
       (define-key tester  (kbd "c")  tester2)
       (string-utils-stringify-anything tester)))

  (expect "c b ignore a ignore"
    (let ((tester  (make-sparse-keymap))
          (tester2 (make-sparse-keymap)))
       (define-key tester  (kbd "a") 'ignore)
       (define-key tester2 (kbd "b") 'ignore)
       (define-key tester  (kbd "c")  tester2)
       (string-utils-stringify-anything tester nil 'ints-are-chars)))

  (expect "sleep 10"
     (string-utils-stringify-anything (start-process "sleeper" "*sleeper*" "sleep" "10")))

  (expect "a b 3"
     (let ((tester (make-char-table 'testing)))
        (set-char-table-range tester '(?a . ?b) 3)
        (string-utils-stringify-anything tester))))

(expectations

  (desc "string-utils-has-darkspace-p")

  (expect 0
    (string-utils-has-darkspace-p "text"))

  (expect nil
    (string-utils-has-darkspace-p ""))

  (expect nil
    (string-utils-has-darkspace-p " "))

  (expect nil
    ;; Narrow No-Break Space
    (string-utils-has-darkspace-p (string ?\s (decode-char 'ucs #x0202f))))

  (expect 1
    ;; Narrow No-Break Space
    (string-utils-has-darkspace-p (string ?\s (decode-char 'ucs #x0202f)) 'ascii-only))

  (expect nil
    (string-utils-has-darkspace-p nil))

  (expect 0
    (string-utils-has-darkspace-p '(1 2 3)))

  (expect nil
    (string-utils-has-darkspace-p '(" " " " " "))))


(expectations

  (desc "string-utils-has-whitespace-p")

  (expect nil
    (string-utils-has-whitespace-p "text"))

  (expect nil
    (string-utils-has-whitespace-p ""))

  (expect 0
    (string-utils-has-whitespace-p " "))

  (expect 4
    (string-utils-has-whitespace-p "text "))

  (expect 4
    ;; Narrow No-Break Space
    (string-utils-has-whitespace-p (concat "text" (string (decode-char 'ucs #x0202f)))))

  (expect nil
    ;; Narrow No-Break Space
    (string-utils-has-whitespace-p (concat "text" (string (decode-char 'ucs #x0202f))) 'ascii-only))

  (expect nil
    (string-utils-has-whitespace-p nil))

  (expect nil
    (string-utils-has-whitespace-p '(1 2 3)))

  (expect 1
    (string-utils-has-whitespace-p '(1 " " 3))))


(expectations

  (desc "string-utils-trim-whitespace")

  (expect "text"
    (string-utils-trim-whitespace " text "))

  (expect "text \n words"
    (string-utils-trim-whitespace "\n text \n words\t\r"))

  (expect "text \n words"
    ;; Narrow No-Break Space
    (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f)))))

  (expect (concat "text \n words\t\r" (string (decode-char 'ucs #x0202f)))
    ;; Narrow No-Break Space
    (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii-only))

  (expect "text\nwords"
    ;; Narrow No-Break Space
    (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) nil 'multi-line))

  (expect (concat "text\nwords\t\r" (string (decode-char 'ucs #x0202f)))
    ;; Narrow No-Break Space
    (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii-only 'multi-line)))


(expectations

  (desc "string-utils-compress-whitespace")

  (expect " text "
    (string-utils-compress-whitespace " text "))

  (expect " text words "
    (string-utils-compress-whitespace "\n text \n words\t\r"))

  (expect " text words "
    ;; Narrow No-Break Space
    (string-utils-compress-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f)))))

  (expect (concat " text words " (string (decode-char 'ucs #x0202f)))
    ;; Narrow No-Break Space
    (string-utils-compress-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii-only)))


(expectations

  (desc "string-utils-string-repeat")

  (expect "bbbbb"
    (string-utils-string-repeat "b" 5))

  (expect "abcabcabcabcabc"
    (string-utils-string-repeat "abc" 5)))


(expectations

  (desc "string-utils-escape-double-quotes")

  (expect "text"
    (string-utils-escape-double-quotes "text"))

  (expect "\\\"text\\\""
    (string-utils-escape-double-quotes "\"text\"")))


(expectations

  (desc "string-utils-quotemeta")

  (expect "text"
    (string-utils-quotemeta "text"))

  (expect "this\\ is\\ a\\ shell\\ command\\:\\ git\\ add\\ newfile\\.txt\\ \\&\\&\\ git\\ commit\\ \\-m\\ \\'initial\\ commit\\'"
    (string-utils-quotemeta "this is a shell command: git add newfile.txt && git commit -m 'initial commit'")))


(expectations

  (desc "string-utils-pad")

  (expect "text    "
    (string-utils-pad "text" 8))

  (expect "    text"
    (string-utils-pad "text" 8 'left))

  (expect "  text  "
    (string-utils-pad "text" 8 'center))

  (expect " text   "
    (string-utils-pad "text" 8 1))

  (expect "   text "
    (string-utils-pad "text" 8 -1))

  (expect "..text.."
    (string-utils-pad "text" 8 'center ?.))

  (expect "text"
    (string-utils-pad "text" 2))

  (expect (error)
    (string-utils-pad "text" 2 nil nil 'throw-error))

  (expect "..text        words.."
    (let ((tab-width 8))
      (string-utils-pad "text\twords" 21 'center ?.))))


(expectations

  (desc "string-utils-pad-list")

  (expect '("this   " "is     " "for    " "testing")
    (string-utils-pad-list '("this" "is" "for" "testing")))

  (expect '("this     " "is       " "for      " "testing  ")
    (string-utils-pad-list '("this" "is" "for" "testing") 2))

  (expect '("this " "is   " "for  " "testing")
    (string-utils-pad-list '("this" "is" "for" "testing") nil 5))

  (expect '("this      " "is        " "for       " "testing   ")
    (string-utils-pad-list '("this" "is" "for" "testing") nil 10))

  (expect ' ("   this" "     is" "    for" "testing")
    (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'left))

  (expect '(" this  " "  is   " "  for  " "testing")
    (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'center))

  (expect '(" this     " " is       " " for      " " testing  ")
    (string-utils-pad-list '("this" "is" "for" "testing") nil 10 1))

  (expect '("     this " "       is " "      for " "  testing ")
    (string-utils-pad-list '("this" "is" "for" "testing") nil 10 -1))

  (expect '(".this.." "..is..." "..for.." "testing")
    (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'center ?.))

  (expect '("this" "is  " "for " "testing")
    (string-utils-pad-list '("this" "is" "for" "testing") nil 4))

  (expect (error)
    (string-utils-pad-list '("this" "is" "for" "testing") nil 4 nil nil 'throw-error)))


(expectations

  (desc "string-utils-propertize-fillin")

  (expect 'bold
    (let* ((start "abc")
           (end   "def")
           (start-with-prop (propertize start :face 'bold))
           (end-with-prop   (propertize start :face 'italic))
           (half-prop (concat start end-with-prop)))
      (setq half-prop (string-utils-propertize-fillin half-prop :face 'bold))
      (get-text-property 0 :face half-prop)))

  (expect 'italic
    (let* ((start "abc")
           (end   "def")
           (start-with-prop (propertize start :face 'bold))
           (end-with-prop   (propertize start :face 'italic))
           (half-prop (concat start end-with-prop)))
      (setq half-prop (string-utils-propertize-fillin half-prop :face 'bold))
      (get-text-property 5 :face half-prop))))


(expectations

  (desc "string-utils-squeeze-filename")

  (expect "~/.emacs.d/lisp/string-utils.el"
    (string-utils-squeeze-filename (concat (expand-file-name "~/") ".emacs.d/lisp/string-utils.el") 100))

  (expect "…/projects/repos/lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 nil "…"))

  (expect "…/lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 nil "…"))

  (expect "…/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 nil "…"))

  (expect "…/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 nil "…"))

  (expect "…/string….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 nil "…"))

  (expect "…/strin….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 nil "…"))

  (expect "…/stri….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 nil "…"))

  (expect "…/str….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 nil "…"))

  (expect "…/st….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 nil "…"))

  (expect "…/s….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 nil "…"))

  (expect "…/str…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 nil "…"))

  (expect "…/st…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 nil "…"))

  (expect "…/s…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 nil "…"))

  (expect "…/…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 nil "…"))

  (expect "……"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 nil "…"))

  (expect "…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 nil "…"))

  (expect ""
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 nil "…"))

  (expect "string-u….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 t "…"))

  (expect "string-….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 t "…"))

  (expect "string….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 t "…"))

  (expect "strin….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 t "…"))

  (expect "stri….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 t "…"))

  (expect "str….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 t "…"))

  (expect "st….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 t "…"))

  (expect "s….el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 t "…"))

  (expect "str…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 t "…"))

  (expect "st…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 t "…"))

  (expect "s…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 t "…"))

  (expect "…"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 t "…"))

  (expect ""
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "…"))

  (expect ".../repos/lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 nil "..."))

  (expect ".../lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 nil "..."))

  (expect ".../lisp/.../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 nil "..."))

  (expect ".../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 nil "..."))

  (expect ".../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 19 nil "..."))

  (expect ".../string-u....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 18 nil "..."))

  (expect ".../string-....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 17 nil "..."))

  (expect ".../string....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 16 nil "..."))

  (expect ".../strin....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 15 nil "..."))

  (expect ".../stri....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 14 nil "..."))

  (expect ".../str....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 13 nil "..."))

  (expect ".../st....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 nil "..."))

  (expect ".../s....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 nil "..."))

  (expect ".../str..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 nil "..."))

  (expect ".../st..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 nil "..."))

  (expect ".../s..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 nil "..."))

  (expect ".../s.."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 nil "..."))

  (expect ".../.."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 nil "..."))

  (expect "....."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 nil "..."))

  (expect "...."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 nil "..."))

  (expect "..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 nil "..."))

  (expect ".."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 nil "..."))

  (expect "."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 nil "..."))

  (expect ".../repos/lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 t "..."))

  (expect ".../lisp/string-utils/string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 t "..."))

  (expect ".../lisp/.../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 t "..."))

  (expect ".../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 t "..."))

  (expect ".../string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 19 t "..."))

  (expect "string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 18 t "..."))

  (expect "string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 17 t "..."))

  (expect "string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 16 t "..."))

  (expect "string-utils.el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 15 t "..."))

  (expect "string-u....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 14 t "..."))

  (expect "string-....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 13 t "..."))

  (expect "string....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 t "..."))

  (expect "strin....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 t "..."))

  (expect "stri....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 t "..."))

  (expect "str....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 t "..."))

  (expect "st....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 t "..."))

  (expect "s....el"
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 t "..."))

  (expect "str..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 t "..."))

  (expect "st..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 t "..."))

  (expect "s..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 t "..."))

  (expect "..."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 t "..."))

  (expect ".."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 t "..."))

  (expect "."
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 t "..."))

  (expect ""
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "..."))

  (expect ""
    (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "...")))

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;

;;; string-utils-test.el ends here
