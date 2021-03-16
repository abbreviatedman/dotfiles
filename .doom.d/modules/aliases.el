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
