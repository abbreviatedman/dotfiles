  (defun kill-terminals ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (string-match-p (regexp-quote "vterm") (buffer-name buffer))
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

;; set up did-you-mean suggestions
(eshell-did-you-mean-setup)

;; switches doom's default terminal opening shortcuts
(map! :map :n :leader (:prefix-map ("o" . "open")
                       :desc "Open eshell buffer" "e" #'+eshell/here
                       :desc "Toggle eshell popup" "E" #'+eshell/toggle))

;aliases

;; open a vterm terminal here
(defun eshell/th ()
  (open-terminal-other-frame))

;; open a non-emacs terminal here
(defun eshell/kh ()
  (shell-command "kitty"))

;; search parent directories by search string
(defun eshell/up (match)
  (eshell-up match))
