;;; crj-fonts.el -*- lexical-binding: t; -*-

(setq crj/variable-font "Fira Code")
(setq crj/doom-modeline-default-height 0.6)
(setq doom-font (font-spec :family crj/variable-font :size 24))
(setq doom-big-font (font-spec :family crj/variable-font :size 48))
(setq doom-variable-pitch-font (font-spec :family crj/variable-font :size 24))
(set-face-attribute 'fixed-pitch nil :family crj/variable-font :height 1.0)
(set-face-attribute 'default nil :family crj/variable-font :height 180)

(defun crj/change-modeline-height (multiplier &optional reset)
  (interactive)
  (let* ((height (internal-get-lisp-face-attribute 'mode-line :height))
         (new-height (if (not reset) (* height multiplier) crj/doom-modeline-default-height)))
    (custom-set-faces
     `(mode-line ((t (:family crj/variable-font :height ,new-height))))
     `(mode-line-inactive ((t (:family crj/variable-font :height ,new-height))))))
(setq doom-modeline-height 1))
(internal-get-lisp-face-attribute 'default :height)
(crj/change-modeline-height 1 t)

; Ligatures
(dolist (char/ligature-re
         `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
           (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
           (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
           (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                             "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
                             "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                         (+ "<"))))
           (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
           (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
           (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
           (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
           (?&  ,(rx (+ "&")))
           (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                         (+ "|"))))
           (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
           (?+  ,(rx (or "+>" (+ "+"))))
           (?\[ ,(rx (or "[<" "[|")))
           (?\{ ,(rx "{|"))
           (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
           (?\; ,(rx (+ ";")))
           (?_  ,(rx (or "_|_" "__")))
           (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  ,(rx "$>"))
           (?^  ,(rx "^="))
           (?\] ,(rx "]#"))))
  (apply (lambda (char ligature-re)
           (set-char-table-range composition-function-table char
                                 `([,ligature-re 0 font-shape-gstring])))
         char/ligature-re))

;; (add-hook 'emacs-lisp-mode-hook #'crj/set-up-elisp-prettify-mode)
(setq prettify-symbols-alist nil)

;; (defun crj/set-up-elisp-prettify-mode ()
;;   (prettify-symbols-mode)
;;   (setq-local prettify-symbols-alist '((lambda . 955))))
