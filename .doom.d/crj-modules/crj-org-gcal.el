;;; crj-modules/crj-org-gcal.el -*- lexical-binding: t; -*-

(defun org-gcal--get-access-token ()
  (oauth2-auto-access-token-sync "colin@pursuit.org" 'google '(calendar)))

;; (org-gcal--get-access-token)

(use-package! org-gcal
  :init
  (setq org-gcal-client-id "121232499602-86a5169f02g77jh9o5gqa1ikuc0ph7cp.apps.googleusercontent.com"
        plstore-cache-passphrase-for-symmetric-encryption t
        org-gcal-client-secret "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"
        org-gcal-fetch-file-alist '(("colin@pursuit.org" . "~/org-stuff/gcal/colin@pursuit.org.org"))))
