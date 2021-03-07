;; TODO command to close all vterm buffers
  (defun kill-terminals ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (eq 'vterm-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer)))
         (buffer-list)))

(map! :map :n :leader (:prefix-map ("q" . "quit/session") :desc "Kill all terminals." "t" #'kill-terminals))
;; start every emacs frame as a terminal by default
(add-hook 'emacs-startup-hook 'vterm)
(setq vterm-shell "/usr/sbin/zsh")
