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
    (string-utils-stringify-anything '[1 2 [3 string]])))


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
