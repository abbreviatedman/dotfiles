; Learning elisp.

(defun crj/open-intro-to-elisp ()
  "Open the manual for \"An Introduction to Programming in Emacs Lisp\" (Info node '(eintr)'), re-using the current window."
  (interactive)
  (info (Info-find-file "eintr"))
  (delete-other-windows))

(map! :leader (:prefix "o"
               :n "l" #'crj/open-intro-to-elisp))

;; Evaluate things in Info Mode.
(map! :map Info-mode-map
      :n "C-c C-e" #'eros-eval-last-sexp
      :n "C-c e" #'eros-eval-last-sexp)
