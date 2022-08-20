; Learning elisp.

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

(map! :leader (:prefix "o"
               :n "l" #'crj/open-elisp-reference-manual
               :n "L" #'crj/open-intro-to-elisp))

;; Quicker evaluation keybindings.
(map! :leader (:prefix "c"
               :desc "Evaluate buffer/region."
                :n "b" #'+eval/buffer-or-region
               :desc "Evaluate last expression."
                :n "e" #'eros-eval-last-sexp
               :desc "Evaluate and replace region."
                :n "E" #'+eval:replace-region
               :desc "Evaluate top-level form."
               :n "f" #'eros-eval-defun))
