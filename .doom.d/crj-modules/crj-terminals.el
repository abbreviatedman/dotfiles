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

(map! :map :n :leader (:prefix ("q" . "quit/session") :desc "Kill all terminals." "t" #'kill-terminals))

(map! :map :n :leader (:prefix ("p" . "project")
                       :desc "Open a project vterm" "v" #'projectile-run-vterm
                       :desc "Open a new project vterm" "V" #'(lambda () (interactive) (projectile-run-vterm 1))
                       :desc "Open a project eshell" "e" 'projectile-run-eshell
                       :desc "Open a new project eshell" "E" #'(lambda () (interactive) (projectile-run-eshell 1))
                       :desc "Edit project dir-locals" "l" #'projectile-edit-dir-locals))

;; SPC p as leader to:
;;; v - projectile vterm
;;; V - projectile new vterm
;;; e - projectile eshell
;;; E - projectile new eshell

;; SPC p f as leader to:
;;; f - find-file-other-frame
;;; v - projectile vterm other frame
;;; V - projectile new vterm other frame
;;; e - projectile eshell other frame
;;; E - projectile new eshell other frame

(map! :map :n :leader (:prefix ("v" . "view")
                       :desc "View available eshell buffers." "e" #'+eshell/switch-to))

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
(map! :map :n :leader (:prefix ("o" . "open")
                       :desc "Open vterm buffer" "v" #'+vterm/here
                       :desc "Open vterm in other frame" "V" #'open-vterm-other-frame
                       :desc "Open eshell buffer" "e" #'+eshell/here
                       :desc "Open eshell in other frame" "E" #'open-eshell-other-frame))

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
