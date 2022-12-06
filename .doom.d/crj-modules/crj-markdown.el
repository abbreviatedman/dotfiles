(defun crj/kill-and-stop-editing ()
  (interactive)
  (crj/kill-all-text-in-buffer)
  (evil-force-normal-state))

(use-package! markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-list-indent-width 2
        markdown-indent-on-enter 'indent-and-new-item
        markdown-header-scaling t
        markdown-header-scaling-values '(2.5
                                         2.0
                                         2.0
                                         1.5
                                         1.0
                                         1.0))

  (define-key markdown-mode-command-map (kbd "d") #'crj/kill-and-stop-editing)
  (define-key markdown-mode-command-map (kbd "C-d") #'crj/kill-and-stop-editing)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode)
  (evil-define-key
    'normal markdown-mode-map
    "gj" #'evil-next-visual-line
    "gk" #'evil-previous-visual-line)
  (evil-define-key 'insert markdown-mode-map
    (kbd "TAB") #'markdown-demote
    (kbd "<backtab>") #'markdown-promote
    (kbd "M-b") #'backward-word)
  (define-key markdown-mode-style-map (kbd "b") #'backward-word)

  (map!
   (:prefix "["
    :n "[" #'markdown-backward-same-level)
   (:prefix "]"
    :n "]" #'markdown-forward-same-level)))

(use-package! evil-markdown
  :after markdown-mode
  :config
  (evil-define-key 'insert evil-markdown-mode-map (kbd "M-b") nil)
  (evil-define-key 'normal evil-markdown-mode-map
    "gj" #'evil-next-visual-line
    "gk" #'evil-previous-visual-line))
