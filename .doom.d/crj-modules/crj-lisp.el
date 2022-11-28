(use-package! ob-racket)
;; (use-package! racket
;;   :init
;;   (setq org-babel-command:racket "/usr/sbin/racket"))

;; Manipulating Expressions
;; Lisp structural editing commands without a lispy-like mode.
(use-package! on-parens
  :config
  (map!
   :leader
   (:prefix ("y" . "lisp")
    :desc "slurp" :n "s" #'on-parens-forward-slurp
    :desc "slurp backwards" :n "S" #'on-parens-backward-slurp
    :desc "barf" :n "b" #'on-parens-forward-barf
    :desc "barf" :n "B" #'on-parens-backward-barf
    :desc "raise" :n "r" #'sp-raise-sexp)))

(use-package! racket-mode
  :init
  (add-hook 'racket-mode-hook #'geiser-mode))
