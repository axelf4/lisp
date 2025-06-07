;;; mylisp-mode.el --- Major mode for MyLisp code  -*- lexical-binding: t -*-

;;; Code:

(require 'lisp-mode)

(defvar mylisp-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry '(0 . 127) "_" table)

    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)

    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\r " " table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?, "'" table)
    (modify-syntax-entry ?` "'" table)

    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)

    table))

(defconst mylisp-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\("
                (regexp-opt '("fn" "if" "let" "set" "defmacro" "def"))
                "\\)\\_>[ \t]*")
       (1 font-lock-keyword-face)))))

(defun mylisp-indent-function (indent-point state)
  (let ((normal-indent (current-column))
        (parents (nth 9 state)))
    (goto-char (1+ (elt state 1))) ; Go to opening parenthesis
    (let* ((containing-form-indent (current-column))
           (function (buffer-substring (point) (progn (forward-sexp) (point))))
           (method (get (intern-soft function) 'mylisp-indent-function)))
      (cond ((eq method 'defun)
             (lisp-indent-defform state indent-point))
            ((integerp method)
             (lisp-indent-specform method state indent-point normal-indent))
            (method
             (funcall method indent-point state))
            ;; Handle `let' specially
            (t (goto-char (elt state 1))
               (condition-case nil (backward-sexp)
                 (:success
                  (let ((function2 (buffer-substring (point)
                                                     (progn (forward-sexp) (point)))))
                    (when (string= function2 "let") containing-form-indent)))
                 (scan-error)))))))

(put 'fn 'mylisp-indent-function 'defun)
(put 'if 'mylisp-indent-function 2)
(put 'let 'mylisp-indent-function 1)

(put 'defmacro 'mylisp-indent-function 'defun)
(put 'def 'mylisp-indent-function 'defun)

;;;###autoload
(define-derived-mode mylisp-mode prog-mode "MyLisp"
  (setq-local multibyte-syntax-as-symbol t
              indent-tabs-mode nil
              comment-start ";"
              comment-start-skip ";+ *"
              comment-add 1
              comment-use-syntax t
              electric-pair-open-newline-between-pairs nil
              electric-pair-skip-whitespace 'chomp
              indent-line-function #'lisp-indent-line
              lisp-indent-function #'mylisp-indent-function
              font-lock-defaults '(mylisp-font-lock-keywords))
  (set-syntax-table mylisp-mode-syntax-table))

(provide 'mylisp-mode)
;;; mylisp-mode.el ends here
