
(require 'string-utils)
(require 'eieio)

;;; string-utils-stringify-anything

(ert-deftest string-utils-stringify-anything-01 nil
  (should (equal "123"
                 (string-utils-stringify-anything 123))))

(ert-deftest string-utils-stringify-anything-02 nil
  (should (equal "1.2"
                 (string-utils-stringify-anything 1.2))))

(ert-deftest string-utils-stringify-anything-03 nil
  (should (equal ""
                 (string-utils-stringify-anything nil))))

(ert-deftest string-utils-stringify-anything-04 nil
  (should (equal "symbol"
                 (string-utils-stringify-anything 'symbol))))

(ert-deftest string-utils-stringify-anything-05 nil
  (should (equal "1 2"
                 (string-utils-stringify-anything (list 1 2)))))

(ert-deftest string-utils-stringify-anything-06 nil
  (should (equal "1 2"
                 (string-utils-stringify-anything (cons 1 2)))))

(ert-deftest string-utils-stringify-anything-07 nil
  (should (equal "123"
                 (string-utils-stringify-anything (list 1 2 3) ""))))

(ert-deftest string-utils-stringify-anything-08 nil
  (should (equal "1 2 3 4"
                 (string-utils-stringify-anything '(1 2 (3 4))))))

(ert-deftest string-utils-stringify-anything-09 nil
  (should (equal "1 2 3 4"
                 (string-utils-stringify-anything '[1 2 [3 4]]))))

(ert-deftest string-utils-stringify-anything-10 nil
  (should (equal "1 2 3 string"
                 (string-utils-stringify-anything '[1 2 [3 string]]))))

(ert-deftest string-utils-stringify-anything-11 nil
  (should
   (string-match-p "\\`[0-9.-]+\\'" (string-utils-stringify-anything (make-random-state) ""))))

(ert-deftest string-utils-stringify-anything-12 nil
  (should (equal "t t t t t t t t t t"
                 (string-utils-stringify-anything (make-bool-vector 10 1)))))

(ert-deftest string-utils-stringify-anything-13 nil
  (should (equal ""
                 (string-utils-stringify-anything (make-bool-vector 10 nil) ""))))

(ert-deftest string-utils-stringify-anything-14 nil
  ;; not really sure if order is guaranteed here
  (should (equal "one 1 two 2"
                 (let ((tester (make-hash-table)))
                   (puthash "one" 1 tester)
                   (puthash "two" 2 tester)
                   (string-utils-stringify-anything tester)))))

(ert-deftest string-utils-stringify-anything-15 nil
  (should (equal ""
                 (string-utils-stringify-anything (make-byte-code nil nil nil nil) ""))))

(ert-deftest string-utils-stringify-anything-16 nil
  (should (equal "args...0"
                 (string-utils-stringify-anything (make-byte-code '(args) nil nil 0) "."))))

(ert-deftest string-utils-stringify-anything-17 nil
  (should (equal "1 *scratch*"
                 (with-current-buffer "*scratch*"
                   (let ((tester (make-marker)))
                     (move-marker tester 1)
                     (string-utils-stringify-anything tester))))))

(ert-deftest string-utils-stringify-anything-18 nil
  (should (equal ""
                 (string-utils-stringify-anything (make-marker) ""))))

(ert-deftest string-utils-stringify-anything-19 nil
  (should (equal "1 1 *scratch*"
                 (with-current-buffer "*scratch*"
                   (let ((tester (make-overlay 1 1)))
                     (string-utils-stringify-anything tester))))))

(ert-deftest string-utils-stringify-anything-20 nil
  (should (equal ""
                 (with-current-buffer "*scratch*"
                   (let ((tester (make-overlay 1 1)))
                     (delete-overlay tester)
                     (string-utils-stringify-anything tester ""))))))

(ert-deftest string-utils-stringify-anything-21 nil
  (should (equal ""
                 (string-utils-stringify-anything (make-sparse-keymap)))))

(ert-deftest string-utils-stringify-anything-22 nil
  (should (equal "97 ignore"
                 (let ((tester (make-sparse-keymap)))
                   (define-key tester (kbd "a") 'ignore)
                   (string-utils-stringify-anything tester)))))

(ert-deftest string-utils-stringify-anything-23 nil
  (should (equal "99 98 ignore 97 ignore"
                 (let ((tester  (make-sparse-keymap))
                       (tester2 (make-sparse-keymap)))
                   (define-key tester  (kbd "a") 'ignore)
                   (define-key tester2 (kbd "b") 'ignore)
                   (define-key tester  (kbd "c")  tester2)
                   (string-utils-stringify-anything tester)))))

(ert-deftest string-utils-stringify-anything-24 nil
  (should (equal "c b ignore a ignore"
                 (let ((tester  (make-sparse-keymap))
                       (tester2 (make-sparse-keymap)))
                   (define-key tester  (kbd "a") 'ignore)
                   (define-key tester2 (kbd "b") 'ignore)
                   (define-key tester  (kbd "c")  tester2)
                   (string-utils-stringify-anything tester nil 'ints-are-chars)))))

(ert-deftest string-utils-stringify-anything-25 nil
  (should (equal "sleep 10"
                 (string-utils-stringify-anything (start-process "sleeper" "*sleeper*" "sleep" "10")))))

(ert-deftest string-utils-stringify-anything-26 nil
  (should (equal "a b 3"
                 (let ((tester (make-char-table 'testing)))
                   (set-char-table-range tester '(?a . ?b) 3)
                   (string-utils-stringify-anything tester)))))

(ert-deftest string-utils-stringify-anything-27 nil
  (should (equal "Monaco"
                 (string-utils-stringify-anything (font-spec :family "Monaco")))))

(ert-deftest string-utils-stringify-anything-28 nil
  (should (equal "Monaco"
                 (string-utils-stringify-anything '["-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1" "Monaco" 12 15 0 0 0]))))

(ert-deftest string-utils-stringify-anything-29 nil
  (should (equal "message"
                 (string-utils-stringify-anything (symbol-function 'message)))))

(ert-deftest string-utils-stringify-anything-30 nil
  (should (equal "my_id"
                 (progn
                   (defclass tester nil
                     ((uid)))
                   (string-utils-stringify-anything (tester "my_id"))))))

(ert-deftest string-utils-stringify-anything-31 nil
  "Stringify window title"
  (cond
    (noninteractive
     (should (equal "*scratch*"
                    (string-utils-stringify-anything (selected-window)))))
    (t
     (should (equal "*ert*"
                    (string-utils-stringify-anything (selected-window)))))))

(ert-deftest string-utils-stringify-anything-32 nil
  "Stringify frame title"
  (cond
    (noninteractive
     (should (equal "F1"
                    (string-utils-stringify-anything (selected-frame)))))
    (t
     (should (equal "frame_title"
                    (let ((frame-title-format "frame_title"))
                      (force-mode-line-update)
                      (redisplay)
                      (string-utils-stringify-anything (selected-frame))))))))

(ert-deftest string-utils-stringify-anything-33 nil
  "Stringify frame configuration"
  (cond
    (noninteractive
     (should (string-match-p "\\`F1 .*foreground-color"
                    (string-utils-stringify-anything (current-frame-configuration)))))
    (t
     (should (string-match-p "\\`frame_title .*foreground-color"
                    (let ((frame-title-format "frame_title"))
                      (force-mode-line-update)
                      (redisplay)
                      (string-utils-stringify-anything (current-frame-configuration))))))))


;;; string-utils-has-darkspace-p

(ert-deftest string-utils-has-darkspace-p-01 nil
  (should (= 0
             (string-utils-has-darkspace-p "text"))))

(ert-deftest string-utils-has-darkspace-p-02 nil
  (should (= 0
             (string-utils-has-darkspace-p "text" 'ascii))))

(ert-deftest string-utils-has-darkspace-p-03 nil
  (should (= 0
             (with-current-buffer "*scratch*"
               (emacs-lisp-mode)
               (string-utils-has-darkspace-p "text" 'syntax)))))

(ert-deftest string-utils-has-darkspace-p-04 nil
  (should-not
   (string-utils-has-darkspace-p "")))

(ert-deftest string-utils-has-darkspace-p-05 nil
  (should-not
   (string-utils-has-darkspace-p "" 'ascii)))

(ert-deftest string-utils-has-darkspace-p-06 nil
  (should-not
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode)
     (string-utils-has-darkspace-p "" 'syntax))))

(ert-deftest string-utils-has-darkspace-p-07 nil
  (should-not
   (string-utils-has-darkspace-p " ")))

(ert-deftest string-utils-has-darkspace-p-08 nil
  (should-not
   (string-utils-has-darkspace-p " " 'ascii)))

(ert-deftest string-utils-has-darkspace-p-09 nil
  (should-not
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode)
     (string-utils-has-darkspace-p " " 'syntax))))

(ert-deftest string-utils-has-darkspace-p-10 nil
  (should-not
   ;; Narrow No-Break Space
   (string-utils-has-darkspace-p (string ?\s (decode-char 'ucs #x0202f)))))

(ert-deftest string-utils-has-darkspace-p-11 nil
  (should (= 1
             ;; Narrow No-Break Space
             (string-utils-has-darkspace-p (string ?\s (decode-char 'ucs #x0202f)) 'ascii))))

(ert-deftest string-utils-has-darkspace-p-12 nil
  (should (= 1
             (with-current-buffer "*scratch*"
               (emacs-lisp-mode)
               ;; Narrow No-Break Space
               (string-utils-has-darkspace-p (string ?\s (decode-char 'ucs #x0202f)) 'syntax)))))

(ert-deftest string-utils-has-darkspace-p-13 nil
  (should-not
   (string-utils-has-darkspace-p nil)))

(ert-deftest string-utils-has-darkspace-p-14 nil
  (should (= 0
             (string-utils-has-darkspace-p '(1 2 3)))))

(ert-deftest string-utils-has-darkspace-p-15 nil
  (should-not
   (string-utils-has-darkspace-p '(" " " " " "))))


;;; string-utils-has-whitespace-p

(ert-deftest string-utils-has-whitespace-p-01 nil
  (should-not
   (string-utils-has-whitespace-p "text")))

(ert-deftest string-utils-has-whitespace-p-02 nil
  (should-not
   (string-utils-has-whitespace-p "text" 'ascii)))

(ert-deftest string-utils-has-whitespace-p-03 nil
  (should-not
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode)
     (string-utils-has-whitespace-p "text" 'syntax))))

(ert-deftest string-utils-has-whitespace-p-04 nil
  (should-not
   (string-utils-has-whitespace-p "")))

(ert-deftest string-utils-has-whitespace-p-05 nil
  (should-not
   (string-utils-has-whitespace-p "" 'ascii)))

(ert-deftest string-utils-has-whitespace-p-06 nil
  (should-not
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode)
     (string-utils-has-whitespace-p "" 'syntax))))

(ert-deftest string-utils-has-whitespace-p-07 nil
  (should (= 0
             (string-utils-has-whitespace-p " "))))

(ert-deftest string-utils-has-whitespace-p-08 nil
  (should (= 0
             (string-utils-has-whitespace-p " " 'ascii))))

(ert-deftest string-utils-has-whitespace-p-09 nil
  (should (= 0
             (with-current-buffer "*scratch*"
               (emacs-lisp-mode)
               (string-utils-has-whitespace-p " " 'syntax)))))

(ert-deftest string-utils-has-whitespace-p-10 nil
  (should (= 4
             (string-utils-has-whitespace-p "text "))))

(ert-deftest string-utils-has-whitespace-p-11 nil
  (should (= 4
             (string-utils-has-whitespace-p "text " 'ascii))))

(ert-deftest string-utils-has-whitespace-p-12 nil
  (should (= 4
             (with-current-buffer "*scratch*"
               (emacs-lisp-mode)
               (string-utils-has-whitespace-p "text " 'syntax)))))

(ert-deftest string-utils-has-whitespace-p-13 nil
  (should (= 4
             ;; Narrow No-Break Space
             (string-utils-has-whitespace-p (concat "text" (string (decode-char 'ucs #x0202f)))))))

(ert-deftest string-utils-has-whitespace-p-14 nil
  (should-not
   ;; Narrow No-Break Space
   (string-utils-has-whitespace-p (concat "text" (string (decode-char 'ucs #x0202f))) 'ascii)))

(ert-deftest string-utils-has-whitespace-p-15 nil
  (should-not
   (with-current-buffer "*scratch*"
     (emacs-lisp-mode)
     ;; Narrow No-Break Space
     (string-utils-has-whitespace-p (concat "text" (string (decode-char 'ucs #x0202f))) 'syntax))))

(ert-deftest string-utils-has-whitespace-p-16 nil
  (should-not
   (string-utils-has-whitespace-p nil)))

(ert-deftest string-utils-has-whitespace-p-17 nil
  (should-not
   (string-utils-has-whitespace-p '(1 2 3))))

(ert-deftest string-utils-has-whitespace-p-18 nil
  (should (= 1
             (string-utils-has-whitespace-p '(1 " " 3)))))


;;; string-utils-trim-whitespace

(ert-deftest string-utils-trim-whitespace-01 nil
  (should (equal "text"
                 (string-utils-trim-whitespace " text "))))

(ert-deftest string-utils-trim-whitespace-02 nil
  (should (equal "text"
                 (string-utils-trim-whitespace " text " 'ascii))))

(ert-deftest string-utils-trim-whitespace-03 nil
  (should (equal "text"
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   (string-utils-trim-whitespace " text " 'syntax)))))

(ert-deftest string-utils-trim-whitespace-04 nil
  (should (equal "text \n words"
                 (string-utils-trim-whitespace "\n text \n words\t\r"))))

(ert-deftest string-utils-trim-whitespace-05 nil
  (should (equal "text \n words"
                 ;; Narrow No-Break Space
                 (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f)))))))

(ert-deftest string-utils-trim-whitespace-06 nil
  (should (equal (concat "text \n words\t\r" (string (decode-char 'ucs #x0202f)))
                 ;; Narrow No-Break Space
                 (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii))))

(ert-deftest string-utils-trim-whitespace-07 nil
  (should (equal (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f)))
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   ;; Narrow No-Break Space
                   (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'syntax)))))

(ert-deftest string-utils-trim-whitespace-08 nil
  (should (equal "text\nwords"
                 ;; Narrow No-Break Space
                 (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) nil 'multi-line))))

(ert-deftest string-utils-trim-whitespace-09 nil
  (should (equal "\ntext\nwords\t\r"
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   (string-utils-trim-whitespace "\n text \n words\t\r" 'syntax 'multi-line)))))

(ert-deftest string-utils-trim-whitespace-10 nil
  (should (equal (concat "\ntext\nwords\t\r" (string (decode-char 'ucs #x0202f)))
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   ;; Narrow No-Break Space
                   (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'syntax 'multi-line)))))

(ert-deftest string-utils-trim-whitespace-11 nil
  (should (equal (concat "text\nwords\t\r" (string (decode-char 'ucs #x0202f)))
                 ;; Narrow No-Break Space
                 (string-utils-trim-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii 'multi-line))))


;;; string-utils-compress-whitespace

(ert-deftest string-utils-compress-whitespace-01 nil
  (should (equal " text "
                 (string-utils-compress-whitespace " text "))))

(ert-deftest string-utils-compress-whitespace-02 nil
  (should (equal " text words "
                 (string-utils-compress-whitespace "\n text \n words\t\r"))))

(ert-deftest string-utils-compress-whitespace-03 nil
  (should (equal " text words "
                 ;; Narrow No-Break Space
                 (string-utils-compress-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f)))))))

(ert-deftest string-utils-compress-whitespace-04 nil
  (should (equal (concat " text words " (string (decode-char 'ucs #x0202f)))
                 ;; Narrow No-Break Space
                 (string-utils-compress-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'ascii))))

(ert-deftest string-utils-compress-whitespace-05 nil
  (should (equal "\n text \n words \r"
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   (string-utils-compress-whitespace "\n text \n words\t\r" 'syntax)))))

(ert-deftest string-utils-compress-whitespace-06 nil
  (should (equal (concat "\n text \n words \r" (string (decode-char 'ucs #x0202f)))
                 (with-current-buffer "*scratch*"
                   (emacs-lisp-mode)
                   ;; Narrow No-Break Space
                   (string-utils-compress-whitespace (concat "\n text \n words\t\r" (string (decode-char 'ucs #x0202f))) 'syntax)))))


;;; string-utils-string-repeat

(ert-deftest string-utils-string-repeat-01 nil
  (should (equal "bbbbb"
                 (string-utils-string-repeat "b" 5))))

(ert-deftest string-utils-string-repeat-02 nil
  (should (equal "abcabcabcabcabc"
                 (string-utils-string-repeat "abc" 5))))


;;; string-utils-escape-double-quotes

(ert-deftest string-utils-escape-double-quotes-01 nil
  (should (equal "text"
                 (string-utils-escape-double-quotes "text"))))

(ert-deftest string-utils-escape-double-quotes-02 nil
  (should (equal "\\\"text\\\""
                 (string-utils-escape-double-quotes "\"text\""))))


;;; string-utils-quotemeta

(ert-deftest string-utils-quotemeta-01 nil
  (should (equal "text"
                 (string-utils-quotemeta "text"))))

(ert-deftest string-utils-quotemeta-02 nil
  (should (equal "this\\ is\\ a\\ shell\\ command\\:\\ git\\ add\\ newfile\\.txt\\ \\&\\&\\ git\\ commit\\ \\-m\\ \\'initial\\ commit\\'"
                 (string-utils-quotemeta "this is a shell command: git add newfile.txt && git commit -m 'initial commit'"))))


;;; string-utils-pad

(ert-deftest string-utils-pad-01 nil
  (should (equal "text    "
                 (string-utils-pad "text" 8))))

(ert-deftest string-utils-pad-02 nil
  (should (equal "    text"
                 (string-utils-pad "text" 8 'left))))

(ert-deftest string-utils-pad-03 nil
  (should (equal "  text  "
                 (string-utils-pad "text" 8 'center))))

(ert-deftest string-utils-pad-04 nil
  (should (equal " text   "
                 (string-utils-pad "text" 8 1))))

(ert-deftest string-utils-pad-05 nil
  (should (equal "   text "
                 (string-utils-pad "text" 8 -1))))

(ert-deftest string-utils-pad-06 nil
  (should (equal "..text.."
                 (string-utils-pad "text" 8 'center ?.))))

(ert-deftest string-utils-pad-07 nil
  (should (equal "text"
                 (string-utils-pad "text" 2))))

(ert-deftest string-utils-pad-08 nil
  (should-error
   (string-utils-pad "text" 2 nil nil 'throw-error)))

(ert-deftest string-utils-pad-09 nil
  (should (equal "..text        words.."
                 (let ((tab-width 8))
                   (string-utils-pad "text\twords" 21 'center ?.)))))


;;; string-utils-pad-list

(ert-deftest string-utils-pad-list-01 nil
  (should (equal '("this   " "is     " "for    " "testing")
                 (string-utils-pad-list '("this" "is" "for" "testing")))))

(ert-deftest string-utils-pad-list-02 nil
  (should (equal '("this     " "is       " "for      " "testing  ")
                 (string-utils-pad-list '("this" "is" "for" "testing") 2))))

(ert-deftest string-utils-pad-list-03 nil
  (should (equal '("this " "is   " "for  " "testing")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil 5))))

(ert-deftest string-utils-pad-list-04 nil
  (should (equal '("this      " "is        " "for       " "testing   ")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil 10))))

(ert-deftest string-utils-pad-list-05 nil
  (should (equal ' ("   this" "     is" "    for" "testing")
                   (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'left))))

(ert-deftest string-utils-pad-list-06 nil
  (should (equal '(" this  " "  is   " "  for  " "testing")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'center))))

(ert-deftest string-utils-pad-list-07 nil
  (should (equal '(" this     " " is       " " for      " " testing  ")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil 10 1))))

(ert-deftest string-utils-pad-list-08 nil
  (should (equal '("     this " "       is " "      for " "  testing ")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil 10 -1))))

(ert-deftest string-utils-pad-list-09 nil
  (should (equal '(".this.." "..is..." "..for.." "testing")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil nil 'center ?.))))

(ert-deftest string-utils-pad-list-10 nil
  (should (equal '("this" "is  " "for " "testing")
                 (string-utils-pad-list '("this" "is" "for" "testing") nil 4))))

(ert-deftest string-utils-pad-list-11 nil
  (should-error
   (string-utils-pad-list '("this" "is" "for" "testing") nil 4 nil nil 'throw-error)))


;;; string-utils-propertize-fillin

(ert-deftest string-utils-propertize-fillin-01 nil
  (should (eq 'bold
              (let* ((start "abc")
                     (end   "def")
                     (start-with-prop (propertize start :face 'bold))
                     (end-with-prop   (propertize end   :face 'italic))
                     (half-prop (concat start end-with-prop)))
                (setq half-prop (string-utils-propertize-fillin half-prop :face 'bold))
                (get-text-property 0 :face half-prop)))))

(ert-deftest string-utils-propertize-fillin-02 nil
  (should (eq 'italic
              (let* ((start "abc")
                     (end   "def")
                     (start-with-prop (propertize start :face 'bold))
                     (end-with-prop   (propertize end   :face 'italic))
                     (half-prop (concat start end-with-prop)))
                (setq half-prop (string-utils-propertize-fillin half-prop :face 'bold))
                (get-text-property 5 :face half-prop)))))

(ert-deftest string-utils-propertize-fillin-03 nil
  (should (equal-including-properties
           #("abcdef" 0 3 (:face bold) 3 6 (:face italic))
           (let* ((start "abc")
                  (end   "def")
                  (start-with-prop (propertize start :face 'bold))
                  (end-with-prop   (propertize end   :face 'italic))
                  (half-prop (concat start end-with-prop)))
             (string-utils-propertize-fillin half-prop :face 'bold)))))


;;; string-utils-squeeze-filename

(ert-deftest string-utils-squeeze-filename-01 nil
  (should (equal "~/.emacs.d/lisp/string-utils.el"
                 (string-utils-squeeze-filename (concat (expand-file-name "~/") ".emacs.d/lisp/string-utils.el") 100))))

(ert-deftest string-utils-squeeze-filename-02 nil
  (should (equal "…/projects/repos/lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 nil "…"))))

(ert-deftest string-utils-squeeze-filename-03 nil
  (should (equal "…/lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 nil "…"))))

(ert-deftest string-utils-squeeze-filename-04 nil
  (should (equal "…/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 nil "…"))))

(ert-deftest string-utils-squeeze-filename-05 nil
  (should (equal "…/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 nil "…"))))

(ert-deftest string-utils-squeeze-filename-06 nil
  (should (equal "…/string….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 nil "…"))))

(ert-deftest string-utils-squeeze-filename-07 nil
  (should (equal "…/strin….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 nil "…"))))

(ert-deftest string-utils-squeeze-filename-08 nil
  (should (equal "…/stri….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 nil "…"))))

(ert-deftest string-utils-squeeze-filename-09 nil
  (should (equal "…/str….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 nil "…"))))

(ert-deftest string-utils-squeeze-filename-10 nil
  (should (equal "…/st….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 nil "…"))))

(ert-deftest string-utils-squeeze-filename-11 nil
  (should (equal "…/s….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 nil "…"))))

(ert-deftest string-utils-squeeze-filename-12 nil
  (should (equal "…/str…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 nil "…"))))

(ert-deftest string-utils-squeeze-filename-13 nil
  (should (equal "…/st…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 nil "…"))))

(ert-deftest string-utils-squeeze-filename-14 nil
  (should (equal "…/s…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 nil "…"))))

(ert-deftest string-utils-squeeze-filename-15 nil
  (should (equal "…/…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 nil "…"))))

(ert-deftest string-utils-squeeze-filename-16 nil
  (should (equal "……"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 nil "…"))))

(ert-deftest string-utils-squeeze-filename-17 nil
  (should (equal "…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 nil "…"))))

(ert-deftest string-utils-squeeze-filename-18 nil
  (should (equal ""
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 nil "…"))))

(ert-deftest string-utils-squeeze-filename-19 nil
  (should (equal "string-u….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 t "…"))))

(ert-deftest string-utils-squeeze-filename-20 nil
  (should (equal "string-….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 t "…"))))

(ert-deftest string-utils-squeeze-filename-21 nil
  (should (equal "string….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 t "…"))))

(ert-deftest string-utils-squeeze-filename-22 nil
  (should (equal "strin….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 t "…"))))

(ert-deftest string-utils-squeeze-filename-23 nil
  (should (equal "stri….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 t "…"))))

(ert-deftest string-utils-squeeze-filename-24 nil
  (should (equal "str….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 t "…"))))

(ert-deftest string-utils-squeeze-filename-25 nil
  (should (equal "st….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 t "…"))))

(ert-deftest string-utils-squeeze-filename-26 nil
  (should (equal "s….el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 t "…"))))

(ert-deftest string-utils-squeeze-filename-27 nil
  (should (equal "str…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 t "…"))))

(ert-deftest string-utils-squeeze-filename-28 nil
  (should (equal "st…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 t "…"))))

(ert-deftest string-utils-squeeze-filename-29 nil
  (should (equal "s…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 t "…"))))

(ert-deftest string-utils-squeeze-filename-30 nil
  (should (equal "…"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 t "…"))))

(ert-deftest string-utils-squeeze-filename-31 nil
  (should (equal ""
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "…"))))

(ert-deftest string-utils-squeeze-filename-32 nil
  (should (equal ".../repos/lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 nil "..."))))

(ert-deftest string-utils-squeeze-filename-33 nil
  (should (equal ".../lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 nil "..."))))

(ert-deftest string-utils-squeeze-filename-34 nil
  (should (equal ".../lisp/.../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 nil "..."))))

(ert-deftest string-utils-squeeze-filename-35 nil
  (should (equal ".../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 nil "..."))))

(ert-deftest string-utils-squeeze-filename-36 nil
  (should (equal ".../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 19 nil "..."))))

(ert-deftest string-utils-squeeze-filename-37 nil
  (should (equal ".../string-u....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 18 nil "..."))))

(ert-deftest string-utils-squeeze-filename-38 nil
  (should (equal ".../string-....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 17 nil "..."))))

(ert-deftest string-utils-squeeze-filename-39 nil
  (should (equal ".../string....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 16 nil "..."))))

(ert-deftest string-utils-squeeze-filename-40 nil
  (should (equal ".../strin....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 15 nil "..."))))

(ert-deftest string-utils-squeeze-filename-41 nil
  (should (equal ".../stri....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 14 nil "..."))))

(ert-deftest string-utils-squeeze-filename-42 nil
  (should (equal ".../str....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 13 nil "..."))))

(ert-deftest string-utils-squeeze-filename-43 nil
  (should (equal ".../st....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 nil "..."))))

(ert-deftest string-utils-squeeze-filename-44 nil
  (should (equal ".../s....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 nil "..."))))

(ert-deftest string-utils-squeeze-filename-45 nil
  (should (equal ".../str..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 nil "..."))))

(ert-deftest string-utils-squeeze-filename-46 nil
  (should (equal ".../st..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 nil "..."))))

(ert-deftest string-utils-squeeze-filename-47 nil
  (should (equal ".../s..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 nil "..."))))

(ert-deftest string-utils-squeeze-filename-48 nil
  (should (equal ".../s.."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 nil "..."))))

(ert-deftest string-utils-squeeze-filename-49 nil
  (should (equal ".../.."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 nil "..."))))

(ert-deftest string-utils-squeeze-filename-50 nil
  (should (equal "....."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 nil "..."))))

(ert-deftest string-utils-squeeze-filename-51 nil
  (should (equal "...."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 nil "..."))))

(ert-deftest string-utils-squeeze-filename-52 nil
  (should (equal "..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 nil "..."))))

(ert-deftest string-utils-squeeze-filename-53 nil
  (should (equal ".."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 nil "..."))))

(ert-deftest string-utils-squeeze-filename-54 nil
  (should (equal "."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 nil "..."))))

(ert-deftest string-utils-squeeze-filename-55 nil
  (should (equal ".../repos/lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 50 t "..."))))

(ert-deftest string-utils-squeeze-filename-56 nil
  (should (equal ".../lisp/string-utils/string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 40 t "..."))))

(ert-deftest string-utils-squeeze-filename-57 nil
  (should (equal ".../lisp/.../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 30 t "..."))))

(ert-deftest string-utils-squeeze-filename-58 nil
  (should (equal ".../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 20 t "..."))))

(ert-deftest string-utils-squeeze-filename-59 nil
  (should (equal ".../string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 19 t "..."))))

(ert-deftest string-utils-squeeze-filename-60 nil
  (should (equal "string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 18 t "..."))))

(ert-deftest string-utils-squeeze-filename-61 nil
  (should (equal "string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 17 t "..."))))

(ert-deftest string-utils-squeeze-filename-62 nil
  (should (equal "string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 16 t "..."))))

(ert-deftest string-utils-squeeze-filename-63 nil
  (should (equal "string-utils.el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 15 t "..."))))

(ert-deftest string-utils-squeeze-filename-64 nil
  (should (equal "string-u....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 14 t "..."))))

(ert-deftest string-utils-squeeze-filename-65 nil
  (should (equal "string-....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 13 t "..."))))

(ert-deftest string-utils-squeeze-filename-66 nil
  (should (equal "string....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 12 t "..."))))

(ert-deftest string-utils-squeeze-filename-67 nil
  (should (equal "strin....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 11 t "..."))))

(ert-deftest string-utils-squeeze-filename-68 nil
  (should (equal "stri....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 10 t "..."))))

(ert-deftest string-utils-squeeze-filename-69 nil
  (should (equal "str....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 9 t "..."))))

(ert-deftest string-utils-squeeze-filename-70 nil
  (should (equal "st....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 8 t "..."))))

(ert-deftest string-utils-squeeze-filename-71 nil
  (should (equal "s....el"
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 7 t "..."))))

(ert-deftest string-utils-squeeze-filename-72 nil
  (should (equal "str..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 6 t "..."))))

(ert-deftest string-utils-squeeze-filename-73 nil
  (should (equal "st..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 5 t "..."))))

(ert-deftest string-utils-squeeze-filename-74 nil
  (should (equal "s..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 4 t "..."))))

(ert-deftest string-utils-squeeze-filename-75 nil
  (should (equal "..."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 3 t "..."))))

(ert-deftest string-utils-squeeze-filename-76 nil
  (should (equal ".."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 2 t "..."))))

(ert-deftest string-utils-squeeze-filename-77 nil
  (should (equal "."
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 1 t "..."))))

(ert-deftest string-utils-squeeze-filename-78 nil
  (should (equal ""
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "..."))))

(ert-deftest string-utils-squeeze-filename-79 nil
  (should (equal ""
                 (string-utils-squeeze-filename "/Volumes/projects/repos/lisp/string-utils/string-utils.el" 0 t "..."))))

(ert-deftest string-utils-squeeze-filename-80 nil
  (should (equal "test_long_file_n…sion"
                 (string-utils-squeeze-filename "test_long_file_name_no_extension" 21 nil "…"))))

(ert-deftest string-utils-squeeze-filename-81 nil
  (should (equal "test_long_file_name_…"
                 (string-utils-squeeze-filename "test_long_file_name_no_extension" 21 nil "…" 'no-tail))))


;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; string-utils-test.el ends here
