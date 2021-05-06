(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)
(setq user-mail-address "abbreviatedman@posteo.net"
      user-full-name  "Colin Jaffe"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval  150
      mu4e-main-buffer-hide-personal-addresses t
      mu4e-drafts-folder "/abbreviatedman/Drafts"
      mu4e-trash-folder "/abbreviatedman/Trash"
      mu4e-sent-folder "/abbreviatedman/Sent"
      mu4e-index-cleanup t
      mu4e-index-lazy-check nil
      mu4e-maildir-shortcuts
      '(("/abbreviatedman/inbox"      . ?i)
        ("/abbreviatedman/Sent" . ?s)
        ("/abbreviatedman/Drafts"     . ?d)
        ("/abbreviatedman/Trash"      . ?t)))

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'sent)

(setq mu4e-attachment-dir "~/Downloads")
(evil-define-key 'normal mu4e-headers-mode-map "M" 'mu4e-headers-mark-all-unread-read)


;; Always BCC myself
;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
;; (defun my-add-header ()
;;   "Add CC: and Bcc: to myself header."
;;   (save-excursion (message-add-header
;;                    ;; pre hook above changes user-mail-address.
;;                    (concat "Bcc: " user-mail-address "\n"))))

;; (add-hook 'mu4e-compose-mode-hook 'my-add-header)
