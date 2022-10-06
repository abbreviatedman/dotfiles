(map! :leader :n "d" #'+default/dired)

(use-package dired
  :config
  (define-key dired-mode-map (kbd "C-c f") #'dired-create-empty-file))

(defun crj-set-up-dired ()
  (dired-omit-mode)
  (dired-hide-details-mode))

(use-package! dired-x
  :config
  (add-hook 'dired-mode-hook #'crj-set-up-dired))
