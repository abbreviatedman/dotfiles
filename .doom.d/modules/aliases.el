;; TODO
;;; generalize from ni alias
;;; git remote -v
;;; hub fork


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

(defun eshell/th ()
"open a vterm terminal here"
  (open-terminal-other-frame))

(defun eshell/kh ()
"Open a non-emacs terminal here"
  (shell-command "kitty"))

(defun eshell/up (match)
"search parent directories by search string"
  (eshell-up match))

(defun eshell/r ()
  "read in history from other eshell buffers"
  (interactive)
  (eshell-read-history))

; hub

(defun eshell/hcl (repo)
  "Clones one repo from GitHub."
  (shell-command (concat "hub clone " repo)))

(defun eshell/hcr (repo)
  "Creates a repo on GitHub."
  (shell-command (concat "hub create " repo)))

(defun eshell/v (file)
  "Open a file in vim in an external terminal."
  (shell-command (concat "kitty --detach vim " file)))

(defun eshell/ni (&rest args)
  "Run npm install with an arbitrary number of arguments."
  (shell-command (mapconcat 'identity (cons "npm install" args) " ")))
