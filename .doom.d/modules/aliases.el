(defun eshell/mkcd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(defun eshell/d (&optional dir)
  "
Open dired in the current directory.
  If passed a directory, opens dired in that directory instead.
"
  (interactive)
  (if dir
      (dired-jump nil dir)
    (dired-jump)))

;; open a vterm terminal here
(defun eshell/th ()
  (open-terminal-other-frame))

;; open a non-emacs terminal here
(defun eshell/kh ()
  (shell-command "kitty"))

(defun eshell/up (match)
"search parent directories by search string"
  (eshell-up match))

(defun eshell/r ()
  "read in history from other eshell buffers"
  (interactive)
  (eshell-read-history))
