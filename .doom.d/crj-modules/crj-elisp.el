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
  "Open the manual for \"An Introduction to Programming in Emacs Lisp\" (Info node '(eintr)'), re-using the current window."
  (interactive)
  (info "eintr")
  (delete-other-windows))

(defun crj/open-elisp-reference-manual ()
  "Open the manual for the \"GNU Emacs Lisp Reference Manual\" (Info node '(elisp)'), re-using the current window."
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

;;; Manipulating Expressions
;;; Lisp structural editing commands without a lispy-like mode.
;; (map!
;;  :leader
;;  (:prefix ("y" . "lisp")
;;   :desc "slurp" :n "s" #'sp-forward-slurp-sexp
;;   :desc "barf" :n "b" #'sp-forward-barf-sexp
;;   :desc "raise" :n "r" #'sp-raise-sexp))

;; Lisp Layer
(use-package symex
  :init
  (setq evil-symex-state-cursor `(box ,(modus-themes-color 'fg-main))
        symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)
          ("^" . symex-goto-first)
          ("K" . +lookup/documentation)))
  :config
  (symex-initialize)
  (evil-define-key 'normal symex-mode-map
    (kbd "<escape>") 'symex-mode-interface)
  (evil-define-key 'insert symex-mode-map
    (kbd "<escape>") #'symex-mode-interface))

;; stay in Symex editing by default in lisp
;; (map! :map emacs-lisp-mode-map :i "<escape>" nil)
;; (map! :map emacs-lisp-mode-map :i "<escape>" #'(lambda ()
;;                                                  (interactive)
;;                                                  (evil-normal-state)
;;                                                  (symex-mode-interface)))
;; (map! :map emacs-lisp-mode-map
;;  :n "M-<escape>" #'symex-mode-interface)
;;; Gotta catch 'em all.
(map!
 :leader
 (:prefix ("t" . "Toggle")
  :desc "debug during elisp errors."
  :n "B" #'toggle-debug-on-error))
