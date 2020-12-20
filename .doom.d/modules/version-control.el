;; set up yadm to work with magit
(require 'tramp)
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
