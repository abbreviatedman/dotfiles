;; Like looking-at but with an offset
(defun my-looking-at (regexp &optional offset)
  (let ((pos (+ (or offset 0) (point))))
    (when (and (>= pos (point-min)) (< pos (point-max)))
      (string-match regexp (string (char-after pos))))))

;; Call the right forward function and move past otherchars
(defun my-forward (&optional backward)
  (let ((ispell-casechars (ispell-get-casechars))
        (ispell-otherchars (ispell-get-otherchars))
        (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
        (offset (if backward -1 0))
        (dir (if backward -1 1))
        (continue t))
    (if subword-mode (subword-forward dir) (forward-word dir))
    (while (and continue
                (not (string= "" ispell-otherchars))
                (my-looking-at ispell-otherchars offset)
                (my-looking-at ispell-casechars (+ dir offset)))
      (if subword-mode (subword-forward dir) (forward-word dir))
      (setq continue ispell-many-otherchars-p))))
(defun my-backward () (my-forward t))

;; Properly find boundaries of words in CamelCase
(defun my-ispell-get-word (orig-fun &optional following extra-otherchars)
  (if (not subword-mode)
      (funcall orig-fun following extra-otherchars)
    (if following (my-forward) (if (not (eobp)) (forward-char)))
    (let* ((beg (progn (my-backward) (point-marker)))
           (end (progn (my-forward) (point-marker)))
           (word (buffer-substring-no-properties beg end)))
      (list word beg end))))
(advice-add #'ispell-get-word :around #'my-ispell-get-word)
(advice-add #'flyspell-get-word :around #'my-ispell-get-word)

;; Simplify and use my-forward to handle CamelCase words
(defun my-flyspell-small-region (orig-fun beg end)
  (save-excursion
    (if (> beg end) (setq beg (prog1 end (setq end beg))))
    (if (< beg (point-min)) (setq beg (point-min)))
    (if (> end (point-max)) (setq end (point-max)))
    (goto-char beg)
    (while (< (point) end)
      (flyspell-word t)
      ;; (sit-for 0) ;; uncomment to enable animation
      (my-forward))))
(advice-add #'flyspell-small-region :around #'my-flyspell-small-region)

;; Fake handling of CamelCase words in flyspell-large-region
;; Much depends on aspell.
(defun my-flyspell-large-region (orig-fun beg end)
  (let ((ispell-extra-args ispell-extra-args)
        (subword-mode subword-mode))
    (when (and subword-mode (string-match "aspell\\'" ispell-program-name))
      (push "--run-together" ispell-extra-args)
      (push "--run-together-limit=6" ispell-extra-args)
      (push "--run-together-min=2" ispell-extra-args)
      (setq subword-mode nil))
    (funcall orig-fun beg end)))
(advice-add #'flyspell-large-region :around #'my-flyspell-large-region)

;; Use aspell if installed
;; (when (executable-find "aspell")
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-list-command "--list"))
; LocalWords:  otherchars

;; Only check the previous word if no longer editing it
(defun my-flyspell-check-pre-word-p ()
  (and (eq flyspell-pre-buffer (current-buffer))
       (numberp flyspell-pre-point)
       (/= (save-excursion
             (goto-char (1- flyspell-pre-point))
             (my-forward)
             (point))
           (save-excursion
             (if (not (bobp)) (backward-char))
             (my-forward)
             (point)))))

;; Simplify and use flyspell-region in the post-command-hook
(defun my-flyspell-post-command-hook (orig-fun)
  (when flyspell-mode
    (with-local-quit
      (let ((command this-command)
            deactivate-mark)
        ;; Handle word at previous location
        (when (my-flyspell-check-pre-word-p)
          (save-excursion
            (goto-char (1- flyspell-pre-point))
            (flyspell-word)))
        ;; Handle word at current location
        (when (flyspell-check-word-p)
          (flyspell-word))
        ;; Handle all other recent changes
        (while (and (not (input-pending-p)) (consp flyspell-changes))
          (let* ((change (pop flyspell-changes))
                 (beg (car change))
                 (end (cdr change)))
            (flyspell-region beg end)))
        (setq flyspell-previous-command command)))))
(advice-add #'flyspell-post-command-hook :around #'my-flyspell-post-command-hook)

(add-hook 'prog-mode-hook #'flyspell-mode)
(remove-hook 'prog-mode-hook #'flyspell-prog-mode)
