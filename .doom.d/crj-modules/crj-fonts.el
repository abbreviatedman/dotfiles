;;; crj-fonts.el -*- lexical-binding: t; -*-
;; Great if you go back to fixed-pitch programming fonts!

(setq crj/variable-pitch-font "IBM Plex Serif")
(setq crj/ui-fixed-pitch-font "Hack")
(setq crj/code-font "Input")

;; (setq doom-font (font-spec :family crj/code-font :size 12))
;; (setq doom-variable-pitch-font (font-spec :family crj/variable-pitch-font :size 12))

(defface monospace
  '((t
     :family "Hack"
     :foundry unspecified
     :width normal
     :height 1.0
     :weight normal
     :slant normal
     :foreground "#505050"
     :distantForeground unspecified
     :background "#f8f8f8"
     :underline nil
     :overline nil
     :strike-through nil
     :box nil
     :inverse nil
     :stipple nil
     :font "Hack"
     :fontset unspecified
     :extend nil))

  "Face for monospace fonts.")

;; (setq doom-font (font-spec :family crj/code-font :size 12))
;; (setq doom-variable-pitch-font crj/variable-font)
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(defun crj/make-custom-face-adjustments ()
  "Customizations to faces whenever the theme is changed.

Fixes many things according to how the author likes them.

Including some pretty annoying issues with line numbers being variable pitch
when you're mixing pitches"

  ;; (interactive)
  ;; (custom-theme-set-faces
  ;;   'user
  ;;   '(default ((t (:family crj/ui-fixed-pitch-font :height 100 ))))
  ;;   '(variable-pitch ((t (:family crj/variable-font :height 1.0))))
  ;;   '(fixed-pitch ((t ( :family crj/code-font :height 1.0)))))
  ;; (set-face-attribute 'line-number t :font "Hack" :inherit 'monospace)
  ;; (set-face-attribute 'line-number-current-line t :font "Hack"))
  ;; '(fixed-pitch ((t :family crj/variable-font :inherit 'default)))
  ;; '(highlight ((t :background "#b5d0ff")))
  ;; '(line-number ((t :family "Hack")))
  ;; '(mode-line-highlight ((t :foreground "#d7d7d7" :background "#0030b4")))
  ;; '(success ((t :foreground "#0031a9")))
  ;; '(line-number-current-line ((t :family "Hack")))))
  ;; don't THINK we need this line anymore... makes the fixed-pitch font inherit from the variable font... why?
  ;; I'll get rid of it if I can't figure it out!
  ;; '(fixed-pitch ((t :family "Hack" :inherit 'default)))
  ;; '(default ((t (:family crj/ui-fixed-pitch-font :height 100))))
  ;; '(variable-pitch ((t (:family crj/variable-font :height 1.0))))
  ;; '(fixed-pitch ((t ( :family crj/code-font :height 1.0))))))
  ;; '(highlight ((t :background "#b5d0ff")))
  ;; '(line-number ((t :family "Hack")))
  ;; '(line-number-current-line ((t :family "Hack")))
  ;; '(mode-line-highlight ((t :foreground "#d7d7d7" :background "#0030b4")))
  ;; '(success ((t :foreground "#0031a9")))))
  )

(require 'mixed-pitch)
(setq crj/fixed-pitch-faces '(line-number-major-tick
                              line-number-current-line
                              line-number
                              line-number-minor-tick))

(defun crj/make-line-number-face-monospace ()
  (interactive)
  (dolist (face crj/fixed-pitch-faces)
    (face-remap-add-relative face :family crj/ui-fixed-pitch-font)))

(crj/make-line-number-face-monospace)
(add-hook 'doom-switch-buffer-hook #'crj/make-line-number-face-monospace)
(advice-add 'mu4e-view-mode :after #'crj/make-line-number-face-monospace)
(advice-add 'revert-buffer :after #'crj/make-line-number-face-monospace)
(add-hook 'text-mode-hook #'mixed-pitch-mode)

;; Sometimes I really do want fixed-pitch for alignment, such as with terminals.
(defun crj/use-fixed-pitch ()
  (set (make-local-variable 'buffer-face-mode-face) 'monospace)
  (buffer-face-mode t))

;; Hooks for modes I want to use fixed pitch in.
(setq crj/fixed-pitch-mode-hooks
      '(vterm-mode-hook
        calendar-mode-hook
        mu4e-headers-mode-hook))

(dolist (hook crj/fixed-pitch-mode-hooks)
  (add-hook hook #'crj/use-fixed-pitch))

(setq emojify-display-style 'unicode)
(if (>= emacs-major-version 27)
    (set-fontset-font
     t
     'symbol
     (font-spec :family "Noto Color Emoji")))


(setq fontaine-presets
      '((regular
         :default-family "Input"
         :default-weight normal
         :default-height 100
         :fixed-pitch-family "Hack"
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

(when (display-graphic-p)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

(setq org-hide-emphasis-markers t)

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

(defun crj/set-up-js-prettify-mode ()
  "Set ligatures, but, for now, don't turn them on."
  (setq-local prettify-symbols-alist '(("=>" . 8658) (">=" . 8805) ("<=" . 8804)))
  ;; and in fact we need to turn it off, because something's turning it on...
  (prettify-symbols-mode -1))

(defun crj/set-up-elisp-prettify-mode ()
  "About the only ligature I like. An iconic symbol for a homoiconic language."
  (setq-local prettify-symbols-alist '(("lambda" . 955)))
  (prettify-symbols-mode))

(setq prettify-symbols-alist '())
(global-auto-composition-mode -1)
(add-hook 'emacs-lisp-mode-hook #'crj/set-up-elisp-prettify-mode)
(add-hook 'rjsx-mode-hook #'crj/set-up-elisp-prettify-mode)

(add-hook 'doom-load-theme-hook #'crj/make-custom-face-adjustments)
(crj/make-custom-face-adjustments)
