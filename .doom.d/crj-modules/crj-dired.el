;;; Dired

;; (evil-define-key 'normal dired-mode-map (kbd "$") 'dirvish-quick-access)

;; (add-load-path! "/home/abbreviatedman/.emacs.d/.local/straight/repos/dirvish/extensions/")
;; (add-load-path! "/home/abbreviatedman/.emacs.d/.local/straight/repos/dirvish/")

;; (use-package! dired
;; :hook dired-omit-mode
;; :config (evil-define-key 'normal dired-mode-map "q" nil))

(add-hook 'dired-mode-hook #'dired-omit-mode)

(use-package! dirvish
  :after (dired)
  :init
  (dirvish-override-dired-mode)
  :config
  (evil-define-key 'normal dired-mode-map "q" nil)
  (setq dirvish-hide-details t
        dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dired-listing-switches "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
        dirvish-attributes '(git-msg
                             vc-state
                             expanded-state
                             file-size
                             all-the-icons))
  :bind
  (("C-c f" . dirvish-fd)
   :map dired-mode-map
   ("q" . dirvish-quit)
   ("b" . dirvish-goto-bookmark)
   ("z" . dirvish-show-history)
   ("F" . dirvish-toggle-fullscreen)
   ("l" . dired-find-file)
   ("h" . dired-up-directory)
   ("C-c ." . dired-omit-mode)
   ("e"   . dired-create-empty-file )
   ("f"   . dirvish-file-info-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump)
   ("s"   . dirvish-quicksort)
   ("v"   . dirvish-vc-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))
;; (map! :map dired-mode-map
;;         :n "q" #'dirvish-quit
;;         :n "b" #'dirvish-goto-bookmark
;;         :n "z" #'dirvish-show-history
;;         :n "F" #'dirvish-toggle-fullscreen
;;         :n "l" #'dired-find-file
;;         :n "h" #'dired-up-directory
;;         :n "C-c" #'dired-omit-mode
;;         :n "e"   #'dired-create-empty-file
;;         :n "f"   #'dirvish-file-info-menu
;;         :n "N"   #'dirvish-narrow
;;         :n "^"   #'dirvish-history-last
;;         :n "h"   #'dirvish-history-jump
;;         :n "s"   #'dirvish-quicksort
;;         :n "v"   #'dirvish-vc-menu
;;         :n "TAB" #'dirvish-subtree-toggle
;;         :n "M-f" #'dirvish-history-go-forward
;;         :n "M-b" #'dirvish-history-go-backward
;;         :n "M-l" #'dirvish-ls-switches-menu
;;         :n "M-m" #'dirvish-mark-menu
;;         :n "M-t" #'dirvish-layout-toggle
;;         :n "M-s" #'dirvish-setup-menu
;;         :n "M-e" #'dirvish-emerge-menu
;;         :n "M-j" #'dirvish-fd-jump)

(use-package! dirvish-collapse
  :after (dirvish))

(use-package! dirvish-emerge
  :after (dirvish))

(use-package! dirvish-history
  :after (dirvish))

(use-package! dirvish-history
  :after (dirvish))

(use-package! dirvish-icons
  :after (dirvish))

(use-package! dirvish-layout
  :after (dirvish)
  :config
  (map! :leader :n "d" #'dirvish-dwim))

(use-package! dirvish-ls
  :after (dirvish))

(use-package! dirvish-narrow
  :after (dirvish))

(use-package! dirvish-peek
  :after (dirvish)
  :config (dirvish-peek-mode))

(use-package! dirvish-quick-access
  :after (dirvish)
  :custom
  (dirvish-quick-access-entries '(("d" "~/Downloads/" "Downloads")
                                  ("h" "~/"           "Home")))
  ; :bind ("C-c a"   . dirvish-quick-access)
  )

(use-package! dirvish-side
  :after (dirvish)
  :config
  (map! :leader (:prefix ("o" . "+open")
                 :desc "Open file-manager sidebar."
                 :n "p" #'dirvish-side)))

(use-package! dirvish-subtree
  :after (dirvish))

(use-package! dirvish-vc
  :after (dirvish))

(use-package! dirvish-yank
  :after (dirvish)
  :config
  (setq dirvish-yank-new-name-style 'append-to-filename
        dirvish-yank-keys '(("p" "Yank (paste) here" dirvish-yank)
                            ("m" "Move here" dirvish-move)
                            ("s" "Make symlinks here" dirvish-symlink)
                            ("r" "Make relative symlinks here" dirvish-relative-symlink)
                            ("h" "Make hardlinks here" dirvish-hardlink)))
  :bind (:map dirvish-mode-map
  ("p"   . dirvish-yank-menu)))

(use-package! dirvish-fd
  :after (dirvish))

(use-package! dired-x
  :after (dired)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))
