(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map company-active-map
   ([tab] . smarter-tab-to-complete)
   ([tab] . smarter-tab-to-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  (company-backends nil)
  :config
  (global-company-mode 1)


  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick))
              (func-list '(org-cycle yas-expand yas-next-field)))
          (catch 'func-suceed
            (dolist (func func-list)
              (ignore-errors (call-interactively func))
              (unless (and (eq old-point (point))
                           (eq old-tick (buffer-chars-modified-tick)))
                (throw 'func-suceed t)))
            (company-complete-common))))))

(setq tide-completion-show-source t)
(setq tide-sort-completions-by-kind t)
(setq tide-completion-ignore-case t)


;; remove . and .. from ivy completion buffers
(setq ivy-extra-directories nil)

;; snippet settings
(use-package! yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("/home/abbreviatedman/.snippets")))

(yas-reload-all)

;; don't add newlines to end of snippet files
(defun no-final-newline-in-buffer ()
  (setq-local require-final-newline nil))
(add-hook! 'snippet-mode-hook 'no-final-newline-in-buffer)
