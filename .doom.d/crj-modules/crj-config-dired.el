;;; Dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(map! :leader :n "d" #'dired-jump)
(defun toggle-peep-dired () (interactive) (peep-dired))
(evil-define-key 'normal dired-mode-map (kbd "C-;") 'toggle-peep-dired)
(evil-define-key 'normal dired-mode-map (kbd "F") #'dired-create-empty-file)
