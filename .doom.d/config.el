;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.doom.d/modules/")
(load-library "secrets")
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; (helm-flx-mode +1)

;; helm flx
;; (setq helm-flx-for-helm-find-files t ;; t by default
      ;; helm-flx-for-helm-locate t) ;; nil by default

;; start every emacs frame as a terminal
(add-hook 'emacs-startup-hook 'vterm)

;; keep line numbers in magit
(setq magit-disable-line-numbers nil)
(setq magit-section-disable-line-numbers nil)

;; always show emojis
(add-hook 'after-init-hook #'global-emojify-mode)

;; don't show mode-line
(global-hide-mode-line-mode)

;; automatically reload file (on focus) with changes on local filesystem
;; useful for when you change the file in another app
;; or it's changed programmatically
(global-auto-revert-mode t)


(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; initialize the targets package
(targets-setup t)

;; company auto-completion
;; (use-package company
;;   :init
;;   (setq company-require-match nil) ; Don't require match, so you can still move your cursor as expected.
;;   (setq company-tooltip-align-annotations t) ; Align annotation to the right side.
;;   (setq company-eclim-auto-save nil)         ; Stop eclim auto save.
;;   (setq company-dabbrev-downcase nil)        ; No downcase when completion.
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-selection-wrap-around t)
;;   :config
;;   ;; Enable downcase only when completing the completion.
;;   (defun jcs--company-complete-selection--advice-around (fn)
;;     "Advice execute around `company-complete-selection' command."
;;     (let ((company-dabbrev-downcase t))
;;       (call-interactively fn)))
;;   (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))

;; (company-tng-configure-default)

;; TODO command to close all vterm buffers

;; move line up by COUNT lines, NOT bringing point
(defun space-yank-send-line-up (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (save-excursion
      (kill-whole-line)
      (forward-line (or (* n -1) -1))
      (yank))
    (forward-line -1)))

;; move line down by COUNT lines, NOT bringing point
(defun space-yank-send-line-down (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (save-excursion
      (kill-whole-line)
      (forward-line (or n 1))
      (yank))
    (forward-line -1)))

;; move line up by COUNT lines, bringing point
(defun space-yank-bring-line-up (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or (* n -1) -1))
    (yank)
    (forward-line -1)))

;; move line down by COUNT lines, bringing point
(defun space-yank-bring-line-down (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or n 1))
    (yank)
    (forward-line -1)))




(map! :map evil-normal-state-map :leader
      (:prefix-map ("y" . "space-yank")
       (:prefix-map ("s" . "send line")
        :desc "send line up" "k" #'space-yank-send-line-up
        :desc "send line down" "j" #'space-yank-send-line-down)))

(defun space-jump-down-visual-mode (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or n 1))
    (yank))
  (evil-visual-line)
  (forward-line -1))




(map! :map evil-normal-state-map "SPC k" #'space-yank-bring-line-up)
(map! :map evil-normal-state-map "SPC j" #'space-yank-bring-line-down)
(map! :map evil-visual-state-map "SPC j" #'space-jump-down-visual-mode)

(map! :map evil-normal-state-map "SPC DEL" #'evil-switch-to-windows-last-buffer)
(map! :map evil-normal-state-map "SPC TAB" #'evil-switch-to-windows-last-buffer)

;; save to backup directory
(setq! auto-save-default t)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/backups/" t)))

;; keep cursor on current character when leaving insert mode
;; (regular vim moves it back by one)
(setq! evil-move-cursor-back nil)


;; get rid of prompts
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-emacs nil)

;; Make markdown continue lists on enter.
(setq markdown-indent-on-enter 'indent-and-new-item)
;; uppercase markdown checkboxes
(setq markdown-gfm-uppercase-checkbox 1)
;; always be gfm-ing
(add-hook 'markdown-mode-hook 'gfm-mode)

;; whitespace settings
(setq! show-trailing-whitespace 1)
(setq! tab-width 2)

;; stop clobbering my system cliboard, emacs you fiend
(setq save-interprogram-paste-before-kill t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Colin Jaffe"
      user-mail-address "balloonasaurus@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font "FiraCode Nerd Font Mono 24")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")


;; line number settings

;; always display line numbers
(global-display-line-numbers-mode t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; soft wrap lines
(global-visual-line-mode 1)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; tab styles
(setq centaur-tabs-style "rounded")
(setq centaur-tabs-height 16)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-close-button nil)

;; tab switching key bindings
(map! :map evil-normal-state-map "g 8" nil)
(map! :map evil-normal-state-map "g 0" nil)
(map! :map evil-normal-state-map :prefix ("g" . "go")
      :desc "Go to the first tab" "0" #'centaur-tabs-select-beg-tab
      :desc "Go to tab 1" "1" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 2" "2" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 3" "3" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 4" "4" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 5" "5" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 6" "6" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 7" "7" #'centaur-tabs-select-visible-tab
      :desc "Go to tab 8" "8" #'centaur-tabs-select-visible-tab
      :desc "Go to the last tab" "9" #'centaur-tabs-select-end-tab
      :desc "Move tab to the left" "C-S-t" #'centaur-tabs-move-current-tab-to-left
      :desc "Move tab to the right" "C-t" #'centaur-tabs-move-current-tab-to-right
)


;; custom snipe-and-transpose functions
(defun evil-snipe-back-and-transpose (chars)
  (interactive "M")
  (evil-with-single-undo
    (evil-snipe-S 1 chars)
    (evil-forward-char)
    (transpose-chars 1)
    (evil-backward-char 2)))

(defun evil-snipe-and-transpose (chars)
  (interactive "M")
  (evil-with-single-undo
    (evil-snipe-s 1 chars)
    (evil-forward-char)
    (transpose-chars 1)
    (evil-backward-char 2)))

;; These mappings are backwards of the usual "lowercase goes forward, uppercase goes backwards" vim standards, but I almost always want to transpose backwards, having made a mistake and moved on in the text. You could always change them back!
(map! :map evil-normal-state-map "SPC r" #'evil-snipe-back-and-transpose)
(map! :map evil-normal-state-map "SPC R" #'evil-snipe-and-transpose)

;; markdown (and some org) key-bindings
;; first, switch gj and gk back to regular evil standards
(map! :map evil-markdown-mode "g j" nil)
(map! :map evil-markdown-mode "g k" nil)
(map! :map gfm-mode "g j" nil)
(map! :map gfm-mode "g k" nil)
(map! :map org-mode "g j" nil)
(map! :map org-mode "g k" nil)

;; now good mappings
(map! :map gfm-mode :leader
      (:prefix-map ("e" . "editing")
      :desc "Add markdown item" "i" #'markdown-insert-list-item
      :desc "Go to next section" "j" #'markdown-forward-same-level
      :desc "Go to previous section" "k" #'markdown-backward-same-level
      :desc "Toggle checkbox" "m" #'markdown-toggle-gfm-checkbox
      ))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "zoom")
       :desc "zoom in" "i" #'doom/increase-font-size
       :desc "zoom out" "o" #'doom/decrease-font-size
       :desc "zoom reset" "z" #'doom/reset-font-size
       ))

(map! :map evil-markdown-mode :leader
      (:prefix-map ("e" . "editing")
      :desc "Add markdown item" "i" #'markdown-insert-list-item
      :desc "Go to next section" "j" #'markdown-forward-same-level
      :desc "Go to previous section" "k" #'markdown-backward-same-level
      :desc "Toggle checkbox" "m" #'markdown-toggle-gfm-checkbox
      )
)

(defun evil-snipe-back-and-transpose (chars)
  (interactive "M")
  (evil-with-single-undo
    (evil-snipe-S 1 chars)
    (evil-forward-char)
    (transpose-chars 1)
    (evil-backward-char 2)))

(defun open-terminal-other-frame ()
  (interactive)
  (make-frame-command)
  (let ((buf (current-buffer)))
    (switch-to-buffer buf)
    (switch-to-buffer-other-frame buf))
  (+vterm/here nil))

; open the result of a search in a new frame
(map! :leader
      :desc "find file other frame" "o f" #'find-file-other-frame)
; open a terminal in a new frame
(map! :leader
      :desc "open terminal other frame" "o T" #'open-terminal-other-frame)

;; titlecase the selection
(map! :map evil-visual-state-map "g t" #'upcase-initials-region)


;; set up yadm to work with magit
(require 'tramp)
(add-to-list 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; symc which org file handles which gcal
(setq org-gcal-fetch-file-alist '(("colin.jaffe@gmail.com" . "~/Sync/org/gcal/colin.jaffe.org")))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "zoom")
       :desc "zoom in" "i" #'doom/increase-font-size
       :desc "zoom out" "o" #'doom/decrease-font-size
       :desc "zoom reset" "z" #'doom/reset-font-size
       ))


;; org agenda setup
(setq! org-agenda-files '("~/Sync/org"))
(after! org
  (org-edna-mode)

  (setq org-startup-folded 'content)

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("a" . "agenda")
       :desc "view agenda" "a" #'org-agenda
       :desc "view todo-list" "t" (lambda () (interactive)  (org-todo-list 2))
       :desc "capture" "x" (lambda () (interactive) (find-file "~/Sync/org/capture.org"))
       :desc "view mobile file" "m" (lambda () (interactive) (find-file "~/Sync/org/phone.org"))
       :desc "view inbox" "i" (lambda () (interactive) (find-file "~/Sync/org/notes.org"))
       :desc "view projects" "p" (lambda () (interactive) (find-file "~/Sync/org/projects.org"))
       :desc "file" "f" #'org-refile
       (:prefix-map ("c" . "calendar")
        :desc "view" "v" (lambda () (interactive) (find-file "~/Sync/org/gcal/colin.jaffe.org"))
        :desc "post" "p" #'org-gcal-post-at-point
        :desc "delete" "d" #'org-gcal-delete-at-point
        :desc "sync" "s" #'org-gcal-sync
        :desc "fetch" "f" #'org-gcal-fetch)))

(map! :map evil-normal-state-map :leader
      :desc "magit with yadm" "g d" (lambda () (interactive (magit-status "/yadm::"))))
;; some available keybinding prefixes
;; SPC d
;; SPC l
;; SPC v
;; SPC y

(setq org-pomodoro-manual-break t)

(use-package! yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("/home/abbreviatedman/.snippets")))

(yas-reload-all)

;; don't add newlines to end of snippet files
(defun no-final-newline-in-buffer ()
  (setq-local require-final-newline nil))
(add-hook! 'snippet-mode-hook 'no-final-newline-in-buffer)

;; zooming in and out
(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "zoom")
       :desc "zoom in" "i" #'doom/increase-font-size
       :desc "zoom out" "o" #'doom/decrease-font-size
       :desc "zoom reset" "z" #'doom/reset-font-size
       ))

;; Snipe settings
;; Look through whole buffer, not just line

(setq evil-snipe-scope 'whole-buffer)
(setq evil-snipe-repeat-scope 'whole-buffer)


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


(setq undo-fu-allow-undo-in-region t)

;; configure prettier integration
(add-hook 'after-init-hook #'global-prettier-mode)
(setenv "NODE_PATH" "/home/abbreviatedman/.asdf/installs/nodejs/14.9.0/.npm/lib/node_modules")

;; remove . and .. from ivy completion buffers
(setq ivy-extra-directories nil)

;; absolute numbers in insert mode
(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t)
(add-hook 'evil-insert-state-entry-hook (lambda () (setq-local display-line-numbers t)))
(add-hook 'evil-insert-state-exit-hook (lambda () (setq-local display-line-numbers 'visual)))

(beacon-mode 1)
(setq beacon-size 10)
(setq beacon-blink-duration 0.1)
