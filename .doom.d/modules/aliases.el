;; TODO
;;; add yay --colors=always alias
;;; generalize from ni alias
;;; git remote -v
;;; hub fork


(defun eshell/ni (&rest args)
  "Run npm install with an arbitrary number of arguments."
  (shell-command (mapconcat 'identity (cons "npm install" args) " ")))

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

(defun eshell/up (match)
"search parent directories by search string"
  (eshell-up match))

(defun eshell/r ()
  "read in history from other eshell buffers"
  (interactive)
  (eshell-read-history))

;; Terminals And External Programs

(defun eshell/th ()
  "open a vterm terminal here"
  (open-vterm-other-frame))

(defun eshell/kh ()
"Open a non-emacs terminal here"
  (shell-command "kitty"))

(defun eshell/v (file)
  "Open a file in vim in an external terminal."
  (shell-command (concat "kitty --detach nvim " file)))

;; git(hub)

(defun eshell/gn ()
  "Creates a new git repository, removing any existing ones."
  (shell-command "rm -rf .git && git init"))

(defun eshell/hcl (repo)
  "Clones one repo from GitHub."
  (shell-command (concat "hub clone " repo)))

(defun eshell/hcr (&optional repo)
  "Creates a repo on GitHub."
  (shell-command (concat "hub create " repo)))

;; sequelize

(defun eshell/sdm ()
  (shell-command "sequelize db:migrate"))
