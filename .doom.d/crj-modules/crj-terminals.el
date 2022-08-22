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

(defun crj/set-up-vterm ()
  (turn-off-evil-mode))

(add-hook 'vterm-mode-hook #'crj/set-up-vterm)

(defun crj/toggle-evil-in-vterm ()
  "Toggles evil mode in Vterm so as to leave ESC to the shell's keybindings.

To use:

;; Start with evil turned off.
(add-hook 'vterm-mode-hook #'turn-off-evil-mode)
;; Add to the copy-mode hook.
;; Which runs on entering /or/ leaving Vterm Copy Mode.
(add-hook 'vterm-copy-mode-hook #'crj/toggle-evil-in-vterm)

The problem we're trying to solve here is that if you want to use
both Emacs' Evil Mode and your shell's Vim keybindings, then when
you press ESC to switch to Normal Mode, do you want Evil Mode's
Normal Mode or the shell's Normal Mode? Without a solution, Evil
Mode will greedily gobble the ESC up and you'll be unable to
enter your shell's Vim emulation. So we need to be able to
disambiguate which Normal Mode we want to enter.

This function provides a solution that leverages Vterm Copy Mode
to give Emacs an alternative way to exit to Evil's Normal Mode,
leaving ESC to the shell.

An alternative solution is to keep ESC for Evil's Normal Mode and
use a different keybinding for the shell's Normal Mode.

Here are a couple ways to do that:

1. Set a non-ESC keybinding for entering normal mode in your
shell's config file.

- You can do this in your bash configuration file:

bind '\"jk\":vi-movement-mode'

Source: URL'https://unix.stackexchange.com/a/303401'

- For zsh, you can use the following in your config file:

bindkey -M viins jk vi-cmd-mode

Source: URL'https://unix.stackexchange.com/a/697026'

- For zsh, you could also use the excellent zsh plugin Zsh Vim
  Mode (URL'https://github.com/softmoth/zsh-vim-mode'), which has
  many benefits including additional ways you can customize
  entering Normal Mode.

2. An alternative method for keeping ESC reserved for Evil Mode
is to leverage Vterm Mode's ability to send a key directly to the
shell. You can do this, for example:

(define-key vterm-mode-map (kbd \"C-c <escape>\") #'vterm-send-escape)

This will mean that using ~C-c <escape>~ will send the escape key
to your shell. ~ESC~ goes to Evil, while ~C-c ESC~ sends ESC to
your shell. This means you can use ESC in other places in your
shell besides the direct command line, which can be useful for
interactive shell programs as well. (Thus, this is recommended
even with this function's solution.)

In short, this function's solution allows you to have the same
exact experience in a Vterm as you do in a non-Emacs terminal
emulator.

The advantage of the other solutions is that you keep the same
Emacs Evil keybinding of ESC even in Vterm Mode.

Which solution works for you likely depends on which workflow you
prioritize and which context you want to context-switch in."

  (interactive)
  (if (bound-and-true-p vterm-copy-mode)
      (progn
        (turn-on-evil-mode)
        (evil-normal-state))
    (evil-insert-state)
    (turn-off-evil-mode)))

(map! :map vterm-mode-map
      "C-c t" #'vterm-copy-mode
      "C-c n" #'vterm-copy-mode)

(map! :map vterm-copy-mode-map
      "C-c t" #'vterm-copy-mode
      "C-c i" #'vterm-copy-mode)

;; (add-hook 'vterm-copy-mode-hook #'crj/toggle-evil-in-vterm)
