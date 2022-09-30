;;; file-management.el -*- lexical-binding: t; -*-


; File Extensions

(add-to-list 'auto-mode-alist '("\\.m(arkdown|d)\\'" . gfm-mode)) ; .md/.markdown
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.trello" . org-mode))

; General Settings

;; save to backup directory
(setq! auto-save-default t)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/backups/" t)))

;; automatically reload file (on focus) with changes on local filesystem
;; useful for when you change the file in another app
;; or it's changed programmatically
(global-auto-revert-mode t)

;; (use-package recentf
;;   :init
;;   (recentf-mode)
;;   :config
;;   (setq recentf-max-saved-items 1000
;;         recentf-auto-cleanup 'never)
;;   (global-set-key (kbd "C-c C-r") #'consult-recent-file))

;; TODO add projectile cache update advice to new file creation
