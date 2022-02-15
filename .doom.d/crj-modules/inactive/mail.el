(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)
(setq user-mail-address "abbreviatedman@posteo.net"
      user-full-name  "Colin Jaffe"
      mu4e-get-mail-command "mbsync -a"
      smtpmail-smtp-service 587
      mu4e-update-interval  nil
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

(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-sent-messages-behavior 'sent)

;; command to mark all read
(evil-define-key 'normal mu4e-headers-mode-map "M" 'mu4e-headers-mark-all-unread-read)

(require 'org-msg)
(defun send-mail ()
  (interactive)
  (setq smtpmail-smtp-server "posteo.de")
  (org-ctrl-c-ctrl-c))

(setq org-msg-default-alternatives '(html))
(map! :map 'org-msg-edit-mode-map :ni "C-c C-c" #'send-mail)



(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
	(setq user-mail-address
	  (cond
	    ((mu4e-message-contact-field-matches msg :to "colin.jaffe@gmail.com")
	      "colin.jaffe@gmail.com")
	    ((mu4e-message-contact-field-matches msg :to "balloonasaurus@gmail.com")
	      "balloonasaurus@gmail.com")
	    ((mu4e-message-contact-field-matches msg :to "colin@pursuit.org")
	      "colin@pursuit.org")
	    (t "abbreviatedman@posteo.net")))))))
