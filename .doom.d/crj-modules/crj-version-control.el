;; set up yadm to work with magit
(add-to-list 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))


(map! :leader (:prefix "g" :desc "Checkout pull request." :n "v" #'forge-checkout-pullreq))

(map! :map evil-normal-state-map :leader
      :desc "magit with yadm" "g d" (lambda () (interactive (magit-status "/yadm::"))))

;; keep line numbers in magit
(setq magit-disable-line-numbers nil)
(setq magit-section-disable-line-numbers nil)

(setq password-cache-expiry nil)
(setq ssh-agency-askpass t)

(require 'keychain-environment)
(after! keychain-environment (keychain-refresh-environment))

(defun crj/git-cloud-save ()
  "Adds, commits, and pushes without any further input from the user.

Adapted from this SO answer: https://emacs.stackexchange.com/questions/21597/using-magit-for-the-most-basic-add-commit-push/64991#64991.

Only changes were:
1. Removing the command to save all open buffers. We /could/ save the visited buffer only, though even that should likely be a discrete operation.
2. Removing user input from the commit message altogether. It now composes a commit message using the current projectile project name.
3. Disabling the pop-up git status window. (It still shows in the minibuffer, as well as the buffer `shell-command-buffer-name'.)"
  (interactive)
  (magit-stage-modified)
  (let
      ((display-buffer-alist
        '(shell-command-buffer-name '(#'display-buffer-no-window))))
    (shell-command
     (format "git commit -m \"Update %s.\""
             (projectile-default-project-name
              (projectile-project-name))))
    (shell-command "git push")))

(map! :leader
      (:prefix "g"
       :desc "Add/commit/push to remote."
       :n "p" #'crj/git-cloud-save
       :desc "Magit pull."
       :n "P" #'magit-pull))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(setq code-review-auth-login-marker 'forge)
