(setq crj/immortal-buffer-names '("*scratch*" "#emacs" "*Messages*" shell-command-buffer-name shell-command-buffer-name-async))

(defun crj/kill-or-bury-current-buffer ()
  (interactive)
  (if (member (buffer-name (current-buffer)) crj/immortal-buffer-names)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun crj/save-and-kill-or-bury-current-buffer ()
  (interactive)
  (save-buffer)
  (if (member (buffer-name (current-buffer)) crj/immortal-buffer-names)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(map! :leader
      (:prefix ("b" . "+buffer")
       :desc "Save file and close buffer."
       :n "q"
       #'crj/save-and-kill-or-bury-current-buffer
       :desc "Kill buffer."
       :n "d"
       #'crj/kill-or-bury-current-buffer)
      (:prefix ("f" . "+file")
       :desc "Save file and close buffer."
       :n "q"
       #'crj/save-and-kill-or-bury-current-buffer))
