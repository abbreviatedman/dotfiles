;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)
;; (require 'mu4e-contrib)
;; (require 'smtpmail)
;; (require 'org-mime)

(setq user-full-name  "Colin Jaffe"
      mu4e-attachment-dir "~/Downloads"
      user-mail-address "colin@pursuit.org"
      smtpmail-default-smtp-server "Posteo.de"
      smtpmail-smtp-server "posteo.de"
      smtpmail-smtp-service 587
      ;; mu4e-maildir-shortcuts
      ;; '(("/INBOX"      . ?i)
      ;;   ("/am-posteo/[am].Sent" . ?s)
      ;;   ("/am-posteo/[am].Drafts"     . ?d)
      ;; ("/am-posteo/[am].Trash"      . ?t))
      smtpmail-stream-type 'starttls
      )

(set-email-account!
 "posteo.net"
 '((mu4e-sent-folder       . "/am-posteo/[am].Sent")
   (mu4e-drafts-folder     . "/am-posteo/Drafts")
   (mu4e-trash-folder      . "/am-posteo/[am].Trash")
   (mu4e-refile-folder     . "/am-posteo/INBOX")
   (smtpmail-smtp-user     . "abbreviatedman@posteo.net")
   (+mu4e-personal-addresses . '("colin@pursuit.org"
                                 "colin.jaffe@gmail.com"
                                 "balloonasaurus@gmail.com"
                                 "abbreviatedman@posteo.net"
                                 "abbreviatedman@posteo.af")))
 t)


(require 'org-msg)

(defun crj/send-mail ()
  (interactive)
  (setq smtpmail-smtp-server "posteo.de")
  (org-ctrl-c-ctrl-c))

;; (setq org-msg-default-alternatives '(html))
;; (map! :map 'org-msg-edit-mode-map :ni "C-c C-c" #'crj/send-mail)


;; Forget where I got this. It wasn't mine!
;; (But I wasn't always good about crediting sources.)
(defun crj/set-from-address ()
  "Set the From address based on the To address of the original."
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

(after! 'mu4e
  (add-to-list 'mu4e-headers-custom-markers
               '("All"
                 (lambda (msg param) t)
                 (lambda () nil))))

(defun crj/mark-all-as-read ()
  (interactive)
  (mu4e-headers-mark-for-each-if
   '(read . nil)
   (lambda (msg _param) t))
  (mu4e-mark-execute-all t))

(map! :map mu4e-headers-mode-map :n "M" 'crj/mark-all-as-read)
