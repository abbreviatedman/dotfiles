(require 'mmm-mode)
(setq mmm-global-mode t)
(setq mmm-submode-decoration-level 0) ;; Turn off background highlight

;; Add css mode for CSS in JS blocks
(mmm-add-classes
  '((mmm-styled-mode
    :submode css-mode
    :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
    :back "`")))

(mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)

(add-hook! 'typescript-mode-hook #'jest-minor-mode #'emmet-mode)

(mmm-add-classes
 '((mmm-js-mode
    :face highlight
    :front "`"
    :front-offset -1
    :back "`"
    :back-offset 1
    :submode typescript-mode)))

(mmm-add-mode-ext-class 'typescript-mode nil 'mmm-js-mode)

(mmm-add-classes
  '((mmm-jsx-mode
     :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
     :front-offset -1
     :back ">\n?\s*)"
     :back-offset 1
     :submode web-mode)))

(mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)

(defun mmm-reapply ()
  (mmm-mode)
  (mmm-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (string-match-p "\\.js" buffer-file-name)
              (mmm-reapply))))
