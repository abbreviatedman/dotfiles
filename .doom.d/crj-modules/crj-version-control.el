;; set up yadm to work with magit
(add-to-list 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(map! :map evil-normal-state-map :leader
      :desc "magit with yadm" "g d" (lambda () (interactive (magit-status "/yadm::"))))

;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Colin Jaffe"
      user-mail-address "balloonasaurus@gmail.com")

;; keep line numbers in magit
(setq magit-disable-line-numbers nil)
(setq magit-section-disable-line-numbers nil)

(setq password-cache-expiry nil)
(setq ssh-agency-askpass t)

(require 'keychain-environment)
(after! keychain-environment (keychain-refresh-environment))

;; Adapted from this SO answer: https://emacs.stackexchange.com/questions/21597/using-magit-for-the-most-basic-add-commit-push/64991#64991.
;; Only changes were:
;; 1. Removing the command to save all open buffers. we /could/ save the visited buffer only, though even that should likely be a discrete operation
;; 2. Removing user input from the commit message altogether. It now creates a commit message using the current projectile project name.
;; 3. Removing the pop-up git status window using a helper function.
(defun crj/push-all ()
  (interactive)
  (magit-stage-modified)
  (let
      ((display-buffer-alist
        '(shell-command-buffer-name '(#'display-buffer-no-window)))
       (inhibit-message t))
    (shell-command
     (format "git commit -m \"Updates %s.\""
             (projectile-default-project-name
              (projectile-project-name))))
    (shell-command "git push")))
