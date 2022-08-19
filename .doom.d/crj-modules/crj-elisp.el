(map! :map Info-mode-map
      :n "C-c C-e" #'eros-eval-last-sexp
      :n "C-c e" #'eros-eval-last-sexp)

(setq initial-major-mode 'emacs-lisp-mode)

