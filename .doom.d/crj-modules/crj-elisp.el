;; Elisp

;;; Prettifying.
;;; I'm not a huge fan of ligatures in general.
;;; But the lambda symbol is a nice shortening.
;; (add-hook 'lisp-mode-hook
;;           #'(lambda ()
;;               (setq-local prettify-symbols-alist
;;                               (append prettify-symbols-alist '(("lambda" . λ))))
;;               (prettify-symbols-mode)))

;;; Learning Elisp.

(defun crj/open-intro-to-elisp ()
  "Open the manual for \"An Introduction to Programming in Emacs Lisp\" (Info
  node '(eintr)'), re-using the current window."
  (interactive)
  (info "eintr")
  (delete-other-windows))

(defun crj/open-elisp-reference-manual ()
  "Open the manual for the \"GNU Emacs Lisp Reference Manual\" (Info node
  '(elisp)'), re-using the current window."
  (interactive)
  (info "elisp")
  (delete-other-windows))

(map!
 :leader
 (:prefix "o"
  :n "l" #'crj/open-elisp-reference-manual
  :n "L" #'crj/open-intro-to-elisp))

;;; Evaluating Elisp

(map! :leader :n "e" #'+eval:region)
(map! :leader :n "E" #'+eval:replace-region)


;; Lisp Layer
;; (use-package symex
;;   :init
;;   (setq evil-symex-state-cursor `(box ,(modus-themes-color 'fg-main))
;;         symex--user-evil-keyspec
;;         '(("j" . symex-go-up)
;;           ("k" . symex-go-down)
;;           ("C-j" . symex-climb-branch)
;;           ("C-k" . symex-descend-branch)
;;           ("M-j" . symex-goto-highest)
;;           ("M-k" . symex-goto-lowest)
;;           ("^" . symex-goto-first)
;;           ("K" . +lookup/documentation)
;;           ("C-S-j" . symex-emit-forward)
;;           ("C-S-k" . symex-emit-backward)))
;;   :config
;;   (symex-initialize)
;;   (evil-define-key 'normal symex-mode-map
;;     (kbd "<escape>") 'symex-mode-interface)
;;   (evil-define-key 'insert symex-mode-map
;;     (kbd "<escape>") #'symex-mode-interface))

;;; Gotta catch 'em all.
(map!
 :leader
 (:prefix ("t" . "Toggle")
  :desc "debug during elisp errors."
  :n "B" #'toggle-debug-on-error))
