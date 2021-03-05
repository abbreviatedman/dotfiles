;;; Dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(map! :map :n :leader "d" #'dired-jump)
(evil-define-key 'normal dired-mode-map "T" 'dired-create-empty-file)
(defun toggle-peep-dired () (interactive) (peep-dired))
(evil-define-key 'normal dired-mode-map (kbd "C-;") 'toggle-peep-dired)
