;;; Dired
(defun setup-dired () (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook #'setup-dired)
(map! :map :n :leader "d" #'dired-jump)
(evil-define-key 'normal dired-mode-map "T" nil)
(evil-define-key 'normal dired-mode-map "T" 'dired-create-empty-file)
