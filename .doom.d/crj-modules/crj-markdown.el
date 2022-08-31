(map!
 :map markdown-mode-map
 :n "gj" #'evil-next-visual-line
 :n "gk" #'evil-previous-visual-line
 :i "TAB" #'markdown-demote
 :i "<backtab>" #'markdown-promote)

(setq markdown-header-scaling t)
(setq markdown-header-scaling-values '(2.5 2.0 2.0 1.5 1.0 1.0))

(map!
 :map markdown-mode-map
 (:prefix "["
  :n "[" #'markdown-backward-same-level)
 (:prefix "]"
  :n "]" #'markdown-forward-same-level))

;; Tell markdown mode to stop over-indenting lists.
(setq markdown-list-indent-width 2)
;; Make markdown continue lists on enter.
(setq markdown-indent-on-enter 'indent-and-new-item)
;; (slow) syntax coloration in markdown blocks
(setq markdown-fontify-code-blocks-natively t)