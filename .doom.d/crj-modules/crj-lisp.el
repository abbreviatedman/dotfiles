(use-package! ob-racket)
;; (use-package! racket
;;   :init
;;   (setq org-babel-command:racket "/usr/sbin/racket"))

(use-package! on-parens)

(use-package! racket-mode
  :init
  (add-hook 'racket-mode-hook #'geiser-mode))
