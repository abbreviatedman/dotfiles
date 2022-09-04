;;; Dired

;; For now, prioritize dirvish bookmarks over the debugger.
(map! :leader
        (:prefix ("o" . "+open")
         :n "d" nil
         :desc "Go to bookmarked directories."
         :n "d" #'dirvish-quick-access
         :desc "Open debugger."
         :n "D" #'+debugger/start))

;; Dirvish improves dired while retaining all of its native amazing features.
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
  (map! :map dirvish-mode-map
        "C-c ." #'dired-omit-mode
        :n "q" #'dirvish-quit
        :n "b" #'dirvish-goto-bookmark
        :n "F" #'dirvish-layout-toggle
        :n "l" #'dired-find-file
        :n "h" #'dired-up-directory
        :n "e"   #'dired-create-empty-file
        :n "f"   #'dirvish-file-info-menu
        :n "N"   #'dirvish-narrow
        :n "^"   #'dirvish-history-last
        :n "z"   #'dirvish-history-jump
        :n "s"   #'dirvish-quicksort
        :n "v"   #'dirvish-vc-menu
        :n "TAB" #'dirvish-subtree-toggle
        :n "M-f" #'dirvish-history-go-forward
        :n "M-b" #'dirvish-history-go-backward
        :n "M-l" #'dirvish-ls-switches-menu
        :n "M-m" #'dirvish-mark-menu
        :n "M-s" #'dirvish-setup-menu
        :n "M-e" #'dirvish-emerge-menu
        :n "M-j" #'dirvish-fd-jump))

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
                                  ("h" "~/"           "Home"))))

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
  (map!
   :map dirvish-mode-map
   :n "p" #'dirvish-yank-menu))

(use-package! dirvish-fd
  :after (dirvish))

(use-package! dired-x
  :after (dired)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))
