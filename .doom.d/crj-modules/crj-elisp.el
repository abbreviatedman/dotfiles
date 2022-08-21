; Elisp

;; Learning elisp.

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

;; Evaluating Elisp

(map! :leader :n "e" #'+eval:region)
(map! :leader :n "E" #'+eval:replace-region)
