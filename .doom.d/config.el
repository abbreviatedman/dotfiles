(add-to-list 'load-path "~/.doom.d/modules/")
(load-library "secrets")
(load-library "version-control")
(load-library "config-org-agenda")
(load-library "completion")
(load-library "snipe-and-transpose")
(load-library "space-liner")
(load-library "frame-fns")
(load-library "frame-cmds")
(load-library "fira-code-mode")

;; start every emacs frame as a terminal by default
(add-hook 'emacs-startup-hook 'vterm)

;; TODO command to close all vterm buffers

;; always show emojis
(add-hook 'after-init-hook #'global-emojify-mode)


;; Except composed from things like "8)" (ascii emojis).
;; And I don't want GitHub's alternate set clogging things up, either.
(setq emojify-emoji-styles '(unicode))

;; don't show mode-line
(global-hide-mode-line-mode 1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; initialize the targets package
(targets-setup t)




(map! :map evil-normal-state-map "SPC DEL" #'evil-switch-to-windows-last-buffer)
(map! :map evil-normal-state-map "SPC TAB" #'evil-switch-to-windows-last-buffer)

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

;; Commands for changing the general look of fonts.
;; These are mostly for presenting things to others.

;; turn on transparency to start with
(set-frame-parameter (selected-frame) 'alpha '(85 . 75))
(add-to-list 'default-frame-alist '(alpha . (85 . 75)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "font presentation")
       :desc "zoom in" "i" #'doom/increase-font-size
       :desc "zoom out" "o" #'doom/decrease-font-size
       :desc "zoom reset" "z" #'doom/reset-font-size
       :desc "toggle ligatures" "l" '(lambda ()
                                       (interactive)
                                       (fira-code-mode 'toggle))
       :desc "toggle transparency" "t" #'toggle-transparency))

;; start every emacs frame with transparency
(add-hook 'emacs-startup-hook 'toggle-transparency)

;; and every code buffer with ligatures
(add-hook 'prog-mode-hook '(lambda () (interactive) (fira-code-mode 1)))


;; Tabs should be 2 spaces by default.
(setq! indent-tabs-mode nil)
(setq! tab-width 2)
(setq! tab-stop-list (number-sequence 2 120 2))

;; Stop clobbering my system cliboard, emacs you fiend.
(setq save-interprogram-paste-before-kill t)

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

(setq doom-font (font-spec :family "Fira Code" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(require 'modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-slanted-constructs t)
(setq modus-themes-syntax 'alt-syntax-yellow-comments)
(setq modus-themes-paren-match 'intense-bold)
(setq modus-themes-headings
      '((t . rainbow)))
(setq modus-themes-scale-headings t)
(setq modus-themes-completions 'opinionated)
(setq doom-theme 'modus-vivendi)



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

;; soft wrap lines
(global-visual-line-mode 1)


;; ;; line number settings

;; always display line numbers
(global-display-line-numbers-mode t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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

(map! :map evil-markdown-mode :leader
      (:prefix-map ("e" . "editing")
       :desc "Add markdown item" "i" #'markdown-insert-list-item
       :desc "Go to next section" "j" #'markdown-forward-same-level
       :desc "Go to previous section" "k" #'markdown-backward-same-level
       :desc "Toggle checkbox" "m" #'markdown-toggle-gfm-checkbox
       )
      )

(defun open-terminal-other-frame ()
  (interactive)
  (make-frame-command)
  (let ((buf (current-buffer)))
    (switch-to-buffer buf)
    (switch-to-buffer-other-frame buf))
  (+vterm/here nil))


;; open the result of a search in a new frame
(map! :leader
      :desc "find file other frame" "o f" #'find-file-other-frame)
;; open a terminal in a new frame
(map! :leader
      :desc "open terminal other frame" "o T" #'open-terminal-other-frame)

;; titlecase the selection
(map! :map evil-visual-state-map "g t" #'upcase-initials-region)




;; Snipe settings
;; Look through whole buffer, not just line

(setq evil-snipe-scope 'whole-buffer)
(setq evil-snipe-repeat-scope 'whole-buffer)




(setq undo-fu-allow-undo-in-region t)

;; configure prettier integration
(add-hook 'after-init-hook #'global-prettier-mode)
(setenv "NODE_PATH" "/home/abbreviatedman/.asdf/installs/nodejs/14.9.0/.npm/lib/node_modules")

(beacon-mode 1)
(setq beacon-size 10)
(setq beacon-blink-duration 0.1)

(python-black-on-save-mode)
(setq python-black-command "/home/abbreviatedman/.local/bin/black")

;; some available keybinding prefixes
;; SPC d
;; SPC l
;; SPC v
;; SPC y

(global-set-key (kbd "M-y") #'counsel-yank-pop)

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

(dotimes (i 10)
  (define-key ivy-minibuffer-map (read-kbd-macro (format "M-%d" i)) 'ivy-call-number))

(defun do.minimal.line-numbers/toggle-line-numbers (&optional relative)
  "Toggle the display of line numbers in the buffer. I am not sure why I need this."
  (interactive)
  (cond (display-line-numbers (display-line-numbers-mode nil)
                              (setq display-line-numbers nil))
        (t (display-line-numbers-mode t)
           (setq display-line-numbers (or relative t)))))

(defun do.minimal.line-numbers/toggle-relative-line-numbers ()
  "Toggle the display of relative line numbers."
  (interactive)
  (do.minimal.line-numbers/toggle-line-numbers 'relative))

(global-set-key (kbd  "<f2>") #'do.minimal.line-numbers/toggle-line-numbers)
(global-set-key (kbd  "<f5>") #'do.minimal.line-numbers/toggle-relative-line-numbers)

