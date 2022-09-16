;; Elisp

;;; Prettifying.
;;; I'm not a huge fan of ligatures in general.
;;; But the lambda symbol is a nice shortening.
(add-hook 'lisp-mode-hook
          #'(lambda ()
              (setq-local prettify-symbols-alist
                              (append prettify-symbols-alist '(("lambda" . λ))))
              (prettify-symbols-mode)))

;;; Learning Elisp.

(defun crj/open-intro-to-elisp ()
  "Open the manual for \"An Introduction to Programming in Emacs Lisp\" (Info node '(eintr)'), re-using the current window."
  (interactive)
  (info "eintr")
  (delete-other-windows))

(defun crj/open-elisp-reference-manual ()
  "Open the manual for the \"GNU Emacs Lisp Reference Manual\" (Info node '(elisp)'), re-using the current window."
  (interactive)
  (info "elisp")
  (delete-other-windows))

(map!
 :leader
 (:prefix "o"
  :n "l" #'crj/open-elisp-reference-manual
  :n "L" #'crj/open-intro-to-elisp))

;;; Evaluating Elisp

(map! :leader :n "e" #'+eval:region)
(map! :leader :n "E" #'+eval:replace-region)

;;; Manipulating Expressions
;;; Lisp structural editing commands without a lispy-like mode.
(map!
 :leader
 (:prefix ("y" . "lisp")
  :desc "slurp" :n "s" #'sp-forward-slurp-sexp
  :desc "barf" :n "b" #'sp-forward-barf-sexp
  :desc "raise" :n "r" #'sp-raise-sexp))

;;; Gotta catch 'em all.
(map!
 :leader
 (:prefix ("t" . "Toggle")
  :desc "Toggle debugging during elisp errors."
  :n "B" #'toggle-debug-on-error))
