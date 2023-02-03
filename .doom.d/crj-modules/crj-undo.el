;; (use-package! undo-fu-session
;;   :init (global-undo-fu-session-mode)
;;   :custom (undo-fu-session-directory (concat doom-cache-dir "undo-fu-session/"))
;;   :config
;;   (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

;;   ;; HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
;;   ;;      filenames to prevent file paths that are too long, so we force
;;   ;;      `undo-fu-session--make-file-name' to use it instead of its own
;;   ;;      home-grown overly-long-filename generator.
;;   ;; TODO PR this upstream; should be a universal issue
;;   (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
;;     :override #'undo-fu-session--make-file-name
;;     (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
;;       (concat (make-backup-file-name-1 file)
;;               (if undo-fu-session-compression ".gz" ".el"))))

;;   ;; HACK Use the faster zstd to compress undo files instead of gzip
;;   (when (executable-find "zstd")
;;     (defadvice! +undo--append-zst-extension-to-file-name-a (filename)
;;       :filter-return #'undo-fu-session--make-file-name
;;       (if undo-fu-session-compression
;;           (concat (file-name-sans-extension filename) ".zst")
;;         filename))))

(use-package! vundo)
