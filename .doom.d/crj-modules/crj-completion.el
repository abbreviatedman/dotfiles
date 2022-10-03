;; (setq tide-completion-show-source t)
;; (setq tide-sort-completions-by-kind t)

(setq completion-ignore-case t)

; snippet settings

(defun crj/set-up-snippets ()
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-reload-all)
  ;; I like having a specific (non-tab) and easy key for expanding snippets
  (map! :i [M-tab] #'yas-expand
        :map yas-keymap "TAB" nil
        "<tab>" nil
        [M-tab] #'yas-next-field-or-maybe-expand))

(require 'yasnippet)
(after! yasnippet (crj/set-up-snippets))

;; don't add newlines to end of snippet files
(defun no-final-newline-in-buffer ()
  (setq-local require-final-newline nil))
(add-hook! 'snippet-mode-hook 'no-final-newline-in-buffer)

;; tide and LSP
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))

(map! :i [C-tab] nil)
(map! :i [C-tab] #'emmet-expand-line)
(map! :map emmet-mode-keymap "<tab>" nil)

(defun toggle-eldoc-mode ()
  (interactive)
  (if eldoc-mode
      (eldoc-mode -1)
    (eldoc-mode 1)))

(map!
 :leader
 (:prefix ("t" . "toggle")
  :desc "Toggle eldoc mode." :n "k" #'toggle-eldoc-mode))

(defun crj/set-up-orderless ()
  (setq orderless-component-separator "\_"
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

;; (use-package vertico
;;   :after orderless
;;   :config
;;   (crj/set-up-orderless)
;;   (vertico-indexed-mode)
;;   (map! :leader
;;         :desc "Select from previous completions." "\"" #'vertico-repeat-select)
;;   (map! :map vertico-map "C-S-P" #'vertico-scroll-down)
;;   (map! :map vertico-map "C-S-N" #'vertico-scroll-up))

;; embark act and resume completion
;; (defun crj/embark-act-without-quitting ()
;;   (interactive)
;;   (let ((embark-quit-after-action nil))
;;     (embark-act)))

(map! :map vertico-map "C-:" #'crj/embark-act-without-quitting)

(use-package corfu
  :config
  (define-key corfu-map (kbd "M-g") #'corfu-quit)
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-j") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "C-k") #'corfu-previous)
  (define-key corfu-map (kbd "C-S-n") #'corfu-scroll-up)
  (define-key corfu-map (kbd "C-S-j") #'corfu-scroll-up)
  (define-key corfu-map (kbd "C-S-p") #'corfu-scroll-down)
  (define-key corfu-map (kbd "C-S-k") #'corfu-scroll-down)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "RET") #'corfu-complete)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "<tab>") #'corfu-insert)
  (define-key corfu-map (kbd "TAB") #'corfu-insert)

  (setq corfu-commit-predicate nil
        completion-category-overrides '((eglot (styles orderless)))
        corfu-auto t
        corfu-auto-prefix 1
        corfu-quit-no-match t
        corfu-cycle t))

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-completion-provider :none
;;         lsp-diagnostics-attributes `((unnecessary
;;                                       :foreground unspecified
;;                                       :underline "gray")
;;                                      (deprecated
;;                                       :strike-through t)))

;;   :config
;;   (setq company-mode -1
;;         lsp-enable-symbol-highlighting nil
;;         lsp-eldoc-enable-hover nil))

;; (add-hook 'lsp-after-initialize-hook #'crj/fix-lsp)

(map! :i "C-n" nil)
(map! :i "C-p" nil)

;; Use "_" as the start of Consult's project-wide search (to play better with Orderless)
(use-package! consult
  :config
  (plist-put (alist-get 'perl consult-async-split-styles-alist) :initial "_"))

;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;; (setq read-extended-command-predicate #'command-completion-default-include-p)

;; (add-hook 'web-mode-hook #'lsp)
;; (add-hook 'sql-mode-hook #'lsp)

;; (setq lsp-sqls-workspace-config-path "workspace")
;; (setq lsp-sqls-connections
;;     '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=abbreviatedman sslmode=disable dbname"))))

(map! :map global-map "M-g" nil)


;; completion source extensions
(use-package! cape
  ;; Bind dedicated completion commands
  :bind (("C-c p" . completion-at-point))
  :init
  (defun crj/set-up-cape ()
  "Add `completion-at-point-functions' from cape to the list."
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-symbol))
  :config
  (crj/set-up-cape))

(after! projectile
  (add-to-list 'projectile-project-root-files ".git"))

;; I don't need the "symbol class" info in my documentation.
(after! marginalia
  (setf (alist-get 'variable marginalia-annotator-registry)
        '(crj/custom-marginalia-annotate-variable builtin none))
  (setf (alist-get 'symbol marginalia-annotator-registry)
        '(crj/custom-marginalia-annotate-symbol builtin none))
  (setf (alist-get 'function marginalia-annotator-registry)
        '(crj/custom-marginalia-annotate-function none)))

;; TODO remove permissions from annotation of file
;; things to keep in mind:
;; - which fields show up are in helper function
;; - there's a project file version as well
;; (defun crj/custom-marginalia-annotate-file (cand))

(defun crj/custom-marginalia-annotate-symbol (cand)
  "Annotate symbol CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (concat
     (marginalia-annotate-binding cand)
     (marginalia--fields
      ((cond
        ((fboundp sym) (marginalia--function-doc sym))
        ((facep sym) (documentation-property sym 'face-documentation))
        (t (documentation-property sym 'variable-documentation)))
       :truncate 1.0 :face 'marginalia-documentation)))))

(defun crj/custom-marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia--variable-value sym) :truncate 0.5)
     ((documentation-property sym 'variable-documentation)
      :truncate 1.0 :face 'marginalia-documentation))))

(defun crj/custom-marginalia-annotate-function (cand)
  "Annotate function CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (when (fboundp sym)
      (concat
       (marginalia-annotate-binding cand)
       (marginalia--fields
        ((marginalia--function-args sym) :face 'marginalia-value
         :truncate 0.5)
        ((marginalia--function-doc sym) :truncate 1.0
         :face 'marginalia-documentation))))))


;; uncomment the following function and its hook to get corfu minibuffer completion.
;; (defun corfu-enable-always-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;   (unless (or (bound-and-true-p mct--active)
;;               (bound-and-true-p vertico--input))
;;     (setq-local corfu-auto t)
;;     (corfu-mode 1)))

;; (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;; (setq lsp-completion-provider :none
;;       lsp-auto-guess-root t)

(map! :leader "/" #'+default/search-buffer)

;; (cl-defmacro teco/lsp-org-babel-enable (lang)
;;   "Support LANG in org source code block."
;;   (setq centaur-lsp 'lsp-mode)
;;   (cl-check-type lang string)
;;   (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
;;          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
;;     `(progn
;;        (defun ,intern-pre (info)
;;          (let ((file-name (->> info caddr (alist-get :file))))
;;            (unless file-name
;;              (setq file-name (make-temp-file "babel-lsp-")))
;;            (setq buffer-file-name file-name)
;;            (lsp-deferred)))
;;        (put ',intern-pre 'function-documentation
;;             (format "Enable lsp-mode in the buffer of org source block (%s)."
;;                     (upcase ,lang)))
;;        (if (fboundp ',edit-pre)
;;            (advice-add ',edit-pre :after ',intern-pre)
;;          (progn
;;            (defun ,edit-pre (info)
;;              (,intern-pre info))
;;            (put ',edit-pre 'function-documentation
;;                 (format "Prepare local buffer environment for org source block (%s)."
;;                         (upcase ,lang))))))))

;; (defvar org-babel-lang-list
;;   '("go" "python" "ipython" "bash" "sh" "js" "javascript" "sql" "sql-mode"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(teco/lsp-org-babel-enable ,lang)))

;; (defun org-babel-edit-prep:javascript (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

;; (defun org-babel-edit-prep:js (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

(map! :leader
      :desc "Yank from kill ring with completion." :n "P" #'yank-from-kill-ring)

;; (map! :i "C-SPC" #'complete-symbol)


(defun crj/marginalia-toggle ()
  (interactive)
  (mapc
   (lambda (x)
     (setcdr x (append (reverse (remq 'none
                                      (remq 'builtin (cdr x))))
                       '(builtin none))))
   marginalia-annotator-registry))

;; use completion version—for now, it works better for me
;; it also works better on smaller screens!
(setq embark-prompter 'embark-completing-read-prompter)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles . (partial-completion))))
        orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp))
  :config
  (setq orderless-component-separator "\_"))
;; (add-to-list completion-category-overrides '(file (styles . partial-completion)))
  ;; (add-to-list completion-category-overrides '(eglot (styles . orderless))))
  ;; (setq completion-category-overrides '((file (styles . (partial-completion))))))

(use-package eglot
  :config
  ;; not sure I need cape
  ;; (crj/set-up-cape)
  )

(use-package elisp-mode
  :config
  ;; not sure I need cape
  ;; (crj/set-up-cape)
  )

(use-package! flycheck
  ;; The below is not a great long-term solution... see if this gets resolved:
  ;; https://github.com/doomemacs/doomemacs/issues/6466
  :after eglot
  :config
  (delq! 'eglot flycheck-checkers))
