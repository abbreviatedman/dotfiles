  (defun kill-terminals ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (string-match-p (regexp-quote "vterm") (buffer-name buffer))
             (kill-buffer buffer)))
         (buffer-list)))

(map! :map :n :leader (:prefix-map ("q" . "quit/session") :desc "Kill all terminals." "t" #'kill-terminals))

;; start every emacs frame as a terminal by default
(add-hook 'emacs-startup-hook '+eshell/here)

;; zsh baby
(setq vterm-shell "/usr/sbin/zsh")

(defun open-vterm-other-frame ()
  (interactive)
  (make-frame-command)
  (let ((buf (current-buffer)))
    (switch-to-buffer buf)
    (switch-to-buffer-other-frame buf))
  (+vterm/here nil))

(defun open-eshell-other-frame ()
  (interactive)
  (make-frame-command)
  (let ((buf (current-buffer)))
    (switch-to-buffer buf)
    (switch-to-buffer-other-frame buf))
  (+eshell/here nil))

;; set up did-you-mean suggestions
(eshell-did-you-mean-setup)

;; open terminals
(map! :map :n :leader (:prefix-map ("o" . "open")
                       :desc "Open vterm buffer" "v" #'+vterm/here
                       :desc "Open vterm in other frame" "V" #'open-vterm-other-frame
                       :desc "Open eshell buffer" "e" #'+eshell/here
                       :desc "Open eshell in other frame" "E" #'open-eshell-other-frame))

;; Toggleable vterm/eshell popups
(map! :map evil-normal-state-map :leader
      (:prefix-map ("t" . "toggle")
       :desc "Toggle eshell popup" "e" #'+eshell/toggle
       :desc "Toggle vterm popup" "v" #'+vterm/toggle))

;; TODO switch eshell buffers

;; remember moar better
(setq eshell-history-size 100000)


;; read in history
(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "presentation")
       :desc "read eshell history in" "r" #'eshell/r))



;; turn off their history saving so we can do it more often
(setq eshell-save-history-on-exit nil)

(defun eshell-append-history ()
  "Append to eshell's command history."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

;; always write to eshell history after every command
(add-hook 'eshell-pre-command-hook #'eshell-append-history)
