;;; crj-fonts.el -*- lexical-binding: t; -*-
;; (setq doom-unicode-font (font-spec :family "Fira Code"))
(setq crj/variable-font "IBM Plex Serif")
(setq fontaine-presets
      '((regular
         :default-family "Input"
         :default-weight normal
         :default-height 100
         :fixed-pitch-family "Input"
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "IBM Plex Serif"
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family "Input"
         :italic-slant italic
         :line-spacing 1)
        ;; (large
        ;;  :default-family "Iosevka"
        ;;  :default-weight normal
        ;;  :default-height 150
        ;;  :fixed-pitch-family nil ; falls back to :default-family
        ;;  :fixed-pitch-weight nil ; falls back to :default-weight
        ;;  :fixed-pitch-height 1.0
        ;;  :variable-pitch-family "FiraGO"
        ;;  :variable-pitch-weight normal
        ;;  :variable-pitch-height 1.05
        ;;  :bold-family nil ; use whatever the underlying face has
        ;;  :bold-weight bold
        ;;  :italic-family nil ; use whatever the underlying face has
        ;;  :italic-slant italic
        ;;  :line-spacing 1)
        ))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
(setq org-hide-emphasis-markers t)
(setq modus-themes-mixed-fonts t)
(add-hook 'text-mode-hook #'mixed-pitch-mode)

(setq crj/doom-modeline-default-height 0.7)
;; (setq doom-font (font-spec :family crj/variable-font :size 18))
;; (setq doom-big-font (font-spec :family crj/variable-font :size 48))
;; (setq doom-variable-pitch-font (font-spec :family crj/variable-font :size 18))
;; (set-face-attribute 'fixed-pitch nil :family crj/variable-font :height 1.0)
;; (set-face-attribute 'default nil :family crj/variable-font :height 120)

(setq all-the-icons-scale-factor 1.0)

;; (defun crj/change-modeline-height (multiplier &optional reset)
;;   (interactive)
;;   (let* ((height (internal-get-lisp-face-attribute 'mode-line :height))
;;          (new-height (if (not reset)
;;                          (* height multiplier)
;;                        crj/doom-modeline-default-height)))
;;     (custom-set-faces
;;      `(mode-line ((t (:family ,crj/variable-font :height ,new-height))))
;;      `(mode-line-inactive ((t (:family ,crj/variable-font :height ,new-height))))))
;;   (setq doom-modeline-height 1))

;; testing code
;; (internal-get-lisp-face-attribute 'default :height)
;; (crj/change-modeline-height 1 t)

;; modeline appearance
(setq doom-modeline-buffer-file-name-style 'relative-to-project)

;; Leaving the ligature code below in case I return to liking them.
;; Currently, my feeling about ligatures is: nah.
;; yup, nah.

; Ligatures
;; (dolist (char/ligature-re
;;          `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
;;            (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
;;            (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
;;            (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
;;                              "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
;;                              "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
;;                          (+ "<"))))
;;            (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
;;            (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
;;            (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
;;            (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
;;            (?&  ,(rx (+ "&")))
;;            (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
;;                          (+ "|"))))
;;            (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
;;            (?+  ,(rx (or "+>" (+ "+"))))
;;            (?\[ ,(rx (or "[<" "[|")))
;;            (?\{ ,(rx "{|"))
;;            (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
;;            (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
;;            (?\; ,(rx (+ ";")))
;;            (?_  ,(rx (or "_|_" "__")))
;;            (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
;;            (?$  ,(rx "$>"))
;;            (?^  ,(rx "^="))
;;            (?\] ,(rx "]#"))))
;;   (apply (lambda (char ligature-re)
;;            (set-char-table-range composition-function-table char
;;                                  `([,ligature-re 0 font-shape-gstring])))
;;          char/ligature-re))

(defun crj/set-up-elisp-prettify-mode ()
  "About the only ligature I like. An iconic symbol for a homoiconic language."
  (prettify-symbols-mode)
  (setq-local prettify-symbols-alist '((lambda . 955))))

(setq prettify-symbols-alist '())
(add-hook 'emacs-lisp-mode-hook #'crj/set-up-elisp-prettify-mode)

(defun crj/make-custom-face-adjustments ()
  "Customizations to faces whenever the theme is changed.

Fixes many things according to how the author likes them.

Including some pretty annoying issues with line numbers being variable pitch when you're mixing pitches"

  (interactive)
  (custom-set-faces
   ;; don't THINK we need this line anymore... makes the fixed-pitch font inherit from the variable font... why?
   ;; I'll get rid of it if I can't figure it out!
   ;; '(fixed-pitch ((t :family crj/variable-font :inherit 'default)))
   '(highlight ((t :background "#b5d0ff")))
   '(line-number ((t :family "Hack")))
   '(mode-line-highlight ((t :foreground "#d7d7d7" :background "#0030b4")))
   '(success ((t :foreground "#0031a9")))
   '(line-number-current-line ((t :family "Hack")))))

(add-hook 'doom-load-theme-hook #'crj/make-custom-face-adjustments)
