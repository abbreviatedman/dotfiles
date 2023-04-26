;; (map! :leader :n "d" #'+default/dired)

;; (use-package dired)

(defun crj-set-up-dired ()
  (dired-omit-mode)
  (dired-hide-details-mode))

(use-package! dired-x
  :config
  (add-hook 'dired-mode-hook #'crj-set-up-dired))
