(defun crj/toggle-eshell-scrollback ()
  (interactive)
  (if (equal eshell-scroll-to-bottom-on-output 'all)
      (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-scroll-to-bottom-on-output 'all)))

;; command to toggle scroll-on-output on or off
(map! :map evil-normal-state :leader
      (:prefix ("z" . "more toggling")
        :desc "toggle terminal auto-scroll" "s" #'crj/toggle-eshell-scrollback
       :desc "read eshell history in" "r" #'eshell/r))


(setq vterm-always-compile-module t)

  (defun kill-terminals ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (string-match-p (regexp-quote "vterm") (buffer-name buffer))
             (kill-buffer buffer)))
         (buffer-list)))

(map! :leader (:prefix ("q" . "quit/session") :desc "Kill all terminals." :n "t" #'kill-terminals))


(map! :leader (:prefix ("v" . "view")
                       :desc "View available eshell buffers." :n "e" #'+eshell/switch-to))

;; start every emacs frame as a terminal by default
;; (add-hook 'emacs-startup-hook '+eshell/here)

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
  (+eshell/here))

;; set up did-you-mean suggestions
; (eshell-did-you-mean-setup)

;; open terminals
(map! :leader (:prefix ("o" . "open")
                       :desc "Open vterm buffer" :n "v" #'+vterm/here
                       :desc "Open vterm in other frame" :n "V" #'open-vterm-other-frame
                       :desc "Open eshell buffer" :n "e" #'+eshell/here
                       :desc "Open eshell in other frame" :n "E" #'open-eshell-other-frame))

;; Toggleable vterm/eshell popups
(map! :map evil-normal-state :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle eshell popup" "e" #'+eshell/toggle
       :desc "Toggle vterm popup" "v" #'+vterm/toggle))

;; TODO switch eshell buffers

;; remember moar better
(setq eshell-history-size 100000)

;; Some
(map! :map 'vterm-mode-map
      "C-c <escape>" #'vterm-send-escape
      "C-c :" #'vterm--self-insert)


;; turn off their history saving so we can do it more often
(setq eshell-save-history-on-exit nil)

(defun crj/eshell-update-history ()
  "Append to eshell's command history and read it everywhere."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

;; always write to eshell history after every command
(add-hook 'eshell-pre-command-hook 'crj/eshell-update-history)
;; When creating a new terminal, get the history of all previous terminals.
(add-hook 'eshell-hist-load-hook 'eshell-read-history)

(setq async-shell-command-buffer 'new-buffer)

(defun crj/async-shell-command-no-window ()
  "A version of `async-shell-command' that won't create a window to display its output buffer."
  (interactive)
  (crj/call-and-bury-window-from-interactive-command
   #'async-shell-command
   shell-command-buffer-name-async))

(defun crj/shell-command-no-window ()
  "A version of `shell-command' that won't create a window to display its output buffer."
  (interactive)
  (crj/call-and-bury-window-from-interactive-command
   #'shell-command
   shell-command-buffer-name))

(defun crj/projectile-run-shell-command-in-root-no-window ()
  "A version of `projectile-run-shell-command-in-root' that won't create a window to display its output buffer."
  (interactive)
  (crj/call-and-bury-window-from-interactive-command
   #'projectile-run-shell-command-in-root
   shell-command-buffer-name))

(defun crj/projectile-run-async-shell-command-in-root-no-window ()
  "A version of `projectile-run-async-shell-command-in-root' that won't create a window to display its output buffer."
  (interactive)
  (crj/call-and-bury-window-from-interactive-command
   #'projectile-run-async-shell-command-in-root
   shell-command-buffer-name-async))

(map! :leader :n "7" #'crj/async-shell-command-no-window)
(map! :leader :n "&" #'async-shell-command)
(map! :leader :n "1" #'crj/shell-command-no-window)
(map! :leader :n "!" #'shell-command)
(map! :leader (:prefix "p"
               :desc "Run project async shell command without a window opening."
               :n "7" #'crj/projectile-run-async-shell-command-in-root-no-window
               :desc "Run project shell command without a window opening."
               :n "1" #'crj/projectile-run-shell-command-in-root-no-window))

(defun crj/call-and-bury-window-from-interactive-command (command window-name)
  "A helper function that calls COMMAND interactively while preventing any buffer with the name WINDOW-NAME from creating a window.

This is useful for times when you want a command to create an output buffer without distracting the user."
  (let
      ((display-buffer-alist
        '(window-name '(#'display-buffer-no-window))))
    (call-interactively command)))
