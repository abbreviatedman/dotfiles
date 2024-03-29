(defun crj/emacs-everywhere-for-i3wm ()
  (let ((id (emacs-everywhere-call "xdotool" "getactivewindow")))
    (find-file-other-frame "~/Sync/org/quick-note.md")
    (insert id)
    (shell-command "i3-msg [class='Google-chrome'] focus")))

(defun crj/emacs-everywhere-for-i3wm--paste-from-register ()
  "Pastes from the primary clipboard register and enters insert mode."

"A simplified version of emacs-everywhere-insert-selection from tecosaur/emacs-everywhere."

  (insert (gui-get-selection 'PRIMARY 'UTF8_STRING))
  (evil-insert-state))

;; the two functions below are from https://www.reddit.com/r/i3wm/comments/aau22p/integrating_emacs_and_chrome_on_i3wm/
(defun crj/emacs-everywhere-for-i3wm--copy-to-chrome ()
  (copy-whole-buffer)
  (let ((inhibit-message t))
    (shell-command "~/scripts/paste_to_chrome.sh")))

(defun crj/emacs-everywhere-for-i3wm--copy-whole-buffer ()
  (interactive)
  (clipboard-kill-ring-save
   (point-min)
   (point-max)))

(defun crj/delete-consecutive-blank-lines ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^$" (point-max) t)
      (delete-blank-lines)
      (forward-line))))

(use-package! emacs-everywhere
  :config
  ;; (add-hook 'emacs-everywhere-init-hooks #'crj/delete-consecutive-blank-lines)
  (setq crj/no-markdown-apps '("Slack"))
  (dolist (app crj/no-markdown-apps)
    (delete app emacs-everywhere-markdown-apps)
    (delete app emacs-everywhere-markdown-windows))
  )
