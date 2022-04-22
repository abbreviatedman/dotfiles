(setq tide-completion-show-source t)
(setq tide-sort-completions-by-kind t)

(setq completion-ignore-case t)

; snippet settings

(after! yasnippet (crj/set-up-snippets))

(defun crj/set-up-snippets ()
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-reload-all)
  ;; I like having a specific (non-tab) and easy key for expanding snippets
  (map! :i [M-tab] #'yas-expand
        :map yas-keymap "TAB" nil
        "<tab>" nil
        [M-tab] #'yas-next-field-or-maybe-expand))

(add-hook 'before-make-frame-hook #'crj/set-up-orderless)

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

(setq lsp-diagnostics-attributes
      `((unnecessary :foreground "unspecified" :underline "gray")
        (deprecated  :strike-through t)))

(defun crj/setup-lsp ()
  (interactive)
  (company-mode -1)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-eldoc-enable-hover nil))

(add-hook 'lsp-mode-hook #'crj/setup-lsp)
(map! :leader (:prefix "t"
               :desc "Toggle eldoc mode." :n "k" #'toggle-eldoc-mode))

(after! orderless #'crj/set-up-orderless)

(defun crj/set-up-orderless ()
  (setq completion-category-defaults nil
        orderless-component-separator "\_"
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package vertico
  :after orderless
  :config
  (crj/set-up-orderless)
  (vertico-indexed-mode)
  (map! :leader
        (:desc "Select from previous completions." "\"" #'vertico-repeat-select))
  (map! :map vertico-map "C-S-P" #'vertico-scroll-down)
  (map! :map vertico-map "C-S-N" #'vertico-scroll-up))

;; embark act and resume completion
(defun crj/embark-act-without-quitting ()
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(map! :map vertico-map "C-:" #'crj/embark-act-without-quitting)

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (vterm-mode . corfu-mode))
  :config
  (define-key corfu-map (kbd "M-g") #'corfu-quit)
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "C-S-n") #'corfu-scroll-up)
  (define-key corfu-map (kbd "C-S-p") #'corfu-scroll-down)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "RET") #'corfu-complete)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "<tab>") #'corfu-insert)
  (define-key corfu-map (kbd "TAB") #'corfu-insert)

  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map (kbd "^") #'corfu-beginning-of-prompt)
  (define-key corfu-map (kbd "$") #'corfu-end-of-prompt)

  (setq corfu-commit-predicate nil
        completion-category-defaults nil
        completion-category-overrides nil
        corfu-auto t
        corfu-auto-prefix 1
        corfu-quit-no-match t
        corfu-cycle t))

(use-package orderless
  :after corfu
  :config
  (setq completion-category-defaults nil
        orderless-component-separator "\_"))

(map! :i "C-n" nil)
(map! :i "C-p" nil)

; Use "_" as the start of Consult's project-wide search (to play better with Orderless)
(after! consult
  (plist-put (alist-get 'perl consult-async-split-styles-alist) :initial "_"))

(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(setq exec-path (append exec-path '("~/go/bin")))

(setq read-extended-command-predicate #'command-completion-default-include-p)

(add-hook 'web-mode-hook #'lsp)

(add-hook 'sql-mode-hook #'lsp)
(setq lsp-sqls-workspace-config-path "workspace")
;; (setq lsp-sqls-connections
;;     '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=abbreviatedman sslmode=disable dbname"))))

(map! :map global-map "M-g" nil)

;; Search for snippets.
(map! :i "C-x s" #'consult-yasnippet)
(map! :i "C-x C-s" #'consult-yasnippet)

;; completion source extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p" . completion-at-point))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

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
       :truncate marginalia-truncate-width :face 'marginalia-documentation)))))

(defun crj/custom-marginalia-annotate-variable (cand)
  "Annotate variable CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia--variable-value sym) :truncate (/ marginalia-truncate-width 2))
     ((documentation-property sym 'variable-documentation)
      :truncate marginalia-truncate-width :face 'marginalia-documentation))))

(defun crj/custom-marginalia-annotate-function (cand)
  "Annotate function CAND with its documentation string."
  (when-let (sym (intern-soft cand))
    (when (functionp sym)
      (concat
       (marginalia-annotate-binding cand)
       (marginalia--fields
        ((marginalia--symbol-class sym) :face 'marginalia-type)
        ((marginalia--function-args sym) :face 'marginalia-value
         :truncate (/ marginalia-truncate-width 2))
        ((marginalia--function-doc sym) :truncate marginalia-truncate-width
         :face 'marginalia-documentation))))))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto t)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(after! lsp
  (setq lsp-completion-provider :none
        lsp-auto-guess-root t))

; Switch regular evil ex search for consult's search.
(map! :map (evil-motion-state-map) "/" #'evil-ex-search-forward
      :leader "/" #'+default/search-buffer)

(cl-defmacro teco/lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh" "js" "javascript" "sql" "sql-mode"))
(dolist (lang org-babel-lang-list)
  (eval `(teco/lsp-org-babel-enable ,lang)))

(defun org-babel-edit-prep:javascript (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(defun org-babel-edit-prep:js (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(map! :leader (:desc "Yank from kill ring with completion." :n "P" #'yank-from-kill-ring))

(map! :i "C-SPC" #'complete-symbol)
