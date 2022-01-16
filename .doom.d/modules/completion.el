
(setq tide-completion-show-source t)
(setq tide-sort-completions-by-kind t)

(setq completion-ignore-case t)

; snippet settings

(use-package yasnippet
  :config
  (yas-global-mode)
  ;; I like having a specific (non-tab) and easy key for expanding snippets
  (map! :i [M-tab] #'yas-expand)
  )

(after! yasnippet
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-reload-all))

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

(use-package vertico
  :init
  (vertico-indexed-mode)
  (map! :map vertico-map "C-S-P" #'vertico-scroll-down)
  (map! :map vertico-map "C-S-N" #'vertico-scroll-up))

;; embark act and resume completion
(defun crj/embark-act-witout-quitting ()
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(map! :map vertico-map "C-:" #'crj/embark-act-witout-quitting)

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (vterm-mode . corfu-mode))
  :init
  (setq corfu-commit-predicate nil)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match t)
  (setq corfu-cycle t)
  (corfu-global-mode))

(map! :i "C-n" nil)
(map! :i "C-p" nil)

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

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package! lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-auto-guess-root t)
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(map! :map global-map "M-g" nil)

;; Search for snippets.
(map! :i "C-x s" #'consult-yasnippet)
(map! :i "C-x C-s" #'consult-yasnippet)

;; (define-key emmet-mode-keymap (kbd "TAB") nil)
;; (define-key emmet-mode-keymap (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map [(tab)] nil)

;; completion source extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

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
