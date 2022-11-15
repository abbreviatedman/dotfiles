(use-package! mu4e
  :init
  (setq user-mail-address "abbreviatedman@posteo.net"
        doom-modeline-mu4e nil
        +mu4e-alert-bell-cmd nil
        sendmail-program (executable-find "msmtp")
        smtpmail-smtp-service 587
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        user-full-name  "Colin Jaffe"
        mu4e-get-mail-command "mbsync --all -c ~/.mbsyncrc"
        auth-source-cache-expiry 28800
        smtpmail-servers-requiring-authorization "*"
        mu4e-main-hide-personal-addresses t
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        mu4e-sent-folder "/Sent"
        mu4e-index-cleanup t
        mu4e-update-interval 60
        mu4e-index-lazy-check nil
        mu4e-attachment-dir "~/Downloads"
        smtpmail-debug-info t
        +mu4e-personal-addresses '("colin@pursuit.org"
                                   "colin.jaffe@gmail.com"
                                   "balloonasaurus@gmail.com"
                                   "abbreviatedman@posteo.net")
        mu4e-maildir-shortcuts '(("/INBOX"      . ?i)
                                 ("/Sent" . ?s)
                                 ("/Drafts"     . ?d)
                                 ("/Trash"      . ?t)))
  :config
  (map! :map mu4e-view-mode-map :n (kbd "M-o") #'ace-link-help)
  (mu4e-alert-disable-notifications)
  (mu4e-alert-disable-mode-line-display))


(defun crj/mark-all-as-read ()
  "Marks all visible messages as read.

Also updates the server to match."
  (interactive)
  (mu4e-headers-mark-for-each-if
   '(read . nil)
   (lambda (msg _param) t))
  (mu4e-mark-execute-all t)
  (mu4e-update-mail-and-index t))

(map! :map mu4e-headers-mode-map :n "M" 'crj/mark-all-as-read)

(defun crj/set-from-address ()
  "Set the From address based on the To address of the original.

Forget where I got this, but it's pretty sweet."
  (let ((msg mu4e-compose-parent-message))
    (when msg
      (setq user-mail-address
            (cond
             ((mu4e-message-contact-field-matches msg :to "colin.jaffe@gmail.com")
              "colin.jaffe@gmail.com")
             ((mu4e-message-contact-field-matches msg :to "balloonasaurus@gmail.com")
              "balloonasaurus@gmail.com")
             ((mu4e-message-contact-field-matches msg :to "colin@pursuit.org")
              "colin@pursuit.org")
             (t "abbreviatedman@posteo.net"))))))

(add-hook 'mu4e-compose-pre-hook #'crj/set-from-address)

(after! mu4e
  (add-to-list 'mu4e-headers-custom-markers
               '("All"
                 (lambda (msg param) t)
                 (lambda () nil))))

;; TODO Should probably return to the below to see if I can manage multiple addresses better
;; (set-email-account!
;;  "posteo.de"
;;  '((mu4e-sent-folder . "/posteo/Sent")
;;    (mu4e-drafts-folder . "/posteo/Drafts")
;;    (mu4e-trash-folder . "/posteo/Trash")
;;    (mu4e-refile-folder . "/posteo/INBOX")
;;    (smtpmail-smtp-user . "abbreviatedman@posteo.net")
;;    (+mu4e-personal-addresses . '("colin@pursuit.org"
;;                                  "colin.jaffe@gmail.com"
;;                                  "balloonasaurus@gmail.com"
;;                                  "abbreviatedman@posteo.net"
;;                                  "abbreviatedman@posteo.af")))
;;  t)


;; Mark all.
