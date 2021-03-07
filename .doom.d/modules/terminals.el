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

(defun open-terminal-other-frame ()
  (interactive)
  (make-frame-command)
  (let ((buf (current-buffer)))
    (switch-to-buffer buf)
    (switch-to-buffer-other-frame buf))
  (+vterm/here nil))

;; open a terminal in a new frame
(map! :leader
      :desc "open terminal other frame" "o T" #'open-terminal-other-frame)
(map! :leader
      :desc "open terminal current frame" "o t" #'+vterm/here)
