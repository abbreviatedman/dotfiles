;; Adds the ability to type Meta-n to go to the nth item in an Ivy buffer.
(defun ivy-call-number (n)
  (interactive
   (list (let* ((type (event-basic-type last-command-event))
                (char (if (characterp type)
                          ;; Number on the main row.
                          type
                        ;; Keypad number, if bound directly.
                        (car (last (string-to-list (symbol-name type))))))
                (n (- char ?0)))
           (if (zerop n) 10 n))))
  (ivy-set-index (1- n))
  (ivy--exhibit)
  (ivy-done))


(use-package ivy
  :config
  (dotimes (i 10)
    (define-key ivy-minibuffer-map (read-kbd-macro (format "M-%d" i)) 'ivy-call-number)))

(global-company-mode 1)
(setq company-global-modes '(not shell-mode eaf-mode markdown-mode gfm-mode org-mode org-msg-edit-mode))

(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  ;; Trigger completion immediately.
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  (company-backends nil))

(setq tide-completion-show-source t)
(setq tide-sort-completions-by-kind t)
(setq tide-completion-ignore-case t)

;; ;; make company prettier
;; (company-posframe-mode 1)
;; (require 'desktop)
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)

;; remove . and .. from ivy completion buffers
(setq ivy-extra-directories nil)

;; snippet settings
(use-package! yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.doom.d/snippets")))

(yas-reload-all)

;; don't add newlines to end of snippet files
(defun no-final-newline-in-buffer ()
  (setq-local require-final-newline nil))
(add-hook! 'snippet-mode-hook 'no-final-newline-in-buffer)

;; tide and lsp
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))

(add-hook 'js2-mode-hook #'setup-tide-mode)


(map! :i [C-tab] nil)
(map! :i [M-tab] #'+web/indent-or-yas-or-emmet-expand)

(defun ivy-copy-to-buffer-action (x)
  (with-ivy-window
   (insert x)))

(defun toggle-eldoc-mode ()
  (interactive)
  (if eldoc-mode
      (eldoc-mode -1)
    (eldoc-mode 1)))

(defun turn-off-global-eldoc-mode ()
  (interactive)
  (global-eldoc-mode -1))

(defun cj/lsp-signature-hack ()
  (interactive)
  (setq lsp-eldoc-enable-hover nil))

(add-hook 'lsp-mode-hook #'cj/lsp-signature-hack)
(map! :leader (:prefix "t"
               :desc "Toggle eldoc mode." :n "k" #'toggle-eldoc-mode))

(map! :i (kbd "C-x s") #'company-yasnippet)
;; (map! :map eshell-mode-map :i (kbd "<tab>") #'company-capf)

(after! company
  (define-key! company-active-map
    "RET" nil
    (kbd "TAB") nil
    (kbd "<return>") nil))

 (let ((map company-active-map))
    (define-key map (kbd "<tab>") 'company-complete-selection))
