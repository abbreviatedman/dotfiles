(add-to-list 'load-path "~/.doom.d/modules/")
(load-library "version-control")
(load-library "secrets")
(load-library "org-stuff")
(load-library "completion")
(load-library "space-liner")
(load-library "config-dired")
(load-library "terminals")
(load-library "aliases")
(load-library "mail")

;; TODO make returning to transparency not additive

;; Reverse the shortcuts between window splitting with follow vs. without.
;; This is because I'm a lot more likely to want to do something with the new split immediately than later.
(map! :leader (:prefix "w"
                       :desc "split window vertically and follow" :n "v" #'+evil/window-vsplit-and-follow
                       :desc "split vertically" :n "V" #'evil-window-vsplit
                       :desc "split window and follow" :n "s" #'+evil/window-split-and-follow
                       :desc "split window" :n "S" #'evil-window-split))


(setq auth-sources (quote (macos-keychain-internet macos-keychain-generic)))


(map! :map markdown-mode-map "M-l" #'markdown-demote)
(map! :map markdown-mode-map "M-h" #'markdown-promote)


;; always show emojis
(add-hook 'after-init-hook #'global-emojify-mode)

;; Except composed from things like "8)" (ascii emojis).
;; And I don't want GitHub's alternate set clogging things up, either.
(setq emojify-emoji-styles '(unicode))

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; initialize the targets package
(targets-setup t)

(map! :map evil-normal-state-map "SPC DEL" #'evil-switch-to-windows-last-buffer)
(map! :map evil-normal-state-map "SPC TAB" #'evil-switch-to-windows-last-buffer)

;; operator that replaces a motion/text object with what's in a register (the " register by default).
(map! :map evil-normal-state-map :leader :desc "Replace with register" "r" #'evil-replace-with-register)

;; keep cursor on current character when leaving insert mode
;; (regular vim moves it back by one)
(setq! evil-move-cursor-back nil)


;; get rid of prompts
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-emacs nil)

; Markdown

;; Tell markdown mode to stop over-indenting lists.
(setq markdown-list-indent-width 2)
;; Make markdown continue lists on enter.
(setq markdown-indent-on-enter 'indent-and-new-item)
;; uppercase markdown checkboxes
(setq markdown-gfm-uppercase-checkbox 1)
;; always be gfm-ing
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))

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

(defun add-fira-code-mode-hook ()
  (interactive)
  (add-hook 'prog-mode-hook 'fira-code-mode))
(defun remove-fira-code-mode-hook ()
  (interactive)
  (remove-hook 'prog-mode-hook 'fira-code-mode))

(map! :leader
      (:prefix "z"
       :desc "zoom in" :n "i" #'doom/increase-font-size
       :desc "zoom out" :n "o" #'doom/decrease-font-size
       :desc "zoom in buffer" :n "I" #'text-scale-increase
       :desc "zoom out buffer" :n "O" #'text-scale-decrease
       :desc "zoom hydra" :n "z" #'+hydra/text-zoom/body
       :desc "turn ligatures on globally" :n "+" #'add-fira-code-mode-hook
       :desc "turn ligatures off globally" :n "-" #'remove-fira-code-mode-hook
       :desc "toggle ligatures for this file" :n "l" #'fira-code-mode
       :desc "toggle prettier globally" :n "p" #'global-prettier-mode
       :desc "toggle transparency" :n "t" #'toggle-transparency))



;; start every emacs frame with transparency
(add-hook 'emacs-startup-hook 'toggle-transparency)

;; TODO remove markdown meta-p mapping

;; Ligatures
(add-hook 'prog-mode-hook 'fira-code-mode)
(setq fira-code-mode-disabled-ligatures '("x" "[]"))
(use-package python
  :config
  (setq python-prettify-symbols-alist (delete '("and" . 8743) python-prettify-symbols-alist))
  (setq python-prettify-symbols-alist (delete '("or" . 8744) python-prettify-symbols-alist)))

;; Tabs should be 2 spaces by default.
(setq! indent-tabs-mode nil)
(setq! tab-width 2)
(setq! tab-stop-list (number-sequence 2 120 2))

;; Stop clobbering my system clipboard, emacs you fiend.
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
;; This was necessary to fix some line of code somewhere giving my
;; Fixed Pitch fonts an absolute height. This meant they didn't scale
;; to other font sizes. Someday, we'll find the culprit!
(set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 1.0)

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


;; Search specific engines.

(engine-mode t)

(defengine duck-duck-go
  "https://duckduckgo.com/?q=%s")
(defengine google
  "https://www.google.com/search?q=%s")
(defengine google-images
  "https://www.google.com/search?tbm=isch&q=%s")
(map! :leader (:prefix "s"
                       :desc "Search DuckDuckGo" :n "h" #'engine/search-duck-duck-go
                       (:prefix "g"
                        :desc "Search Google" :n "g" #'engine/search-google
                        :desc "Search Google Images" :n "i" #'engine/search-google-images)))


(map! :map org-mode-map :leader
      (:prefix "m"
       :desc "Next todo GTD-style" :n "m" '(lambda ()
                                          (interactive)
                                          (org-todo 'done)
                                          (org-forward-heading-same-level 1)
                                          (org-todo 2))))



;; markdown (and some org) key-bindings
;; now good mappings

(map! :map (evil-markdown-mode gfm-mode) :leader
      (:prefix "e"
       :desc "Add markdown item" :n "i" #'markdown-insert-list-item
       :desc "Go to next section" :n "j" #'markdown-forward-same-level
       :desc "Go to previous section" :n "k" #'markdown-backward-same-level
       :desc "Repair list" :n "r" #'org-list-repair
       :desc "Toggle checkbox" :n "m" #'markdown-toggle-gfm-checkbox))

;; open the result of a search in a new frame
(map! :leader
      :desc "find file other frame" "o f" #'find-file-other-frame)

(evil-define-operator evil-titlecase (beg end type)
  "Convert text to title case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-titlecase beg end nil)
    (upcase-initials-region beg end)))

(map! :nm "g o" #'evil-titlecase)

;; use org to open links.
(map! :n "g b" #'org-open-at-point)

;; Snipe settings
;; Look through whole buffer, not just line

(setq evil-snipe-scope 'whole-buffer)
(setq evil-snipe-repeat-scope 'whole-buffer)


;; set spelling dictionary
(setq ispell-dictionary "en")
;; spelling dictionary location
(setq ispell-personal-dictionary "~/.doom.d/spelling/en.pws")


(setq undo-fu-allow-undo-in-region t)

;; TODO configure prettier and blacken to run only in certain (and different) modes

;; configure prettier integration
(add-hook 'after-init-hook #'global-prettier-mode)

;; black integration
;; (add-hook 'after-init-hook #'blacken-mode)
(setq blacken-only-if-project-is-blackened t)

;; set where node is located
(setenv "NODE_PATH" nil)
;; asdf linux version
;; (setenv "PATH" (concat (getenv "PATH") ":~/.asdf/shims"))
;; (setq exec-path (append exec-path '("~/.asdf/shims")))
;; standard mac version
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/local/node"))
(setq exec-path (append exec-path '("/usr/local/bin")))


(setq projectile-track-known-projects-automatically nil)

;; Opens minibuffer to select a root folder from which to discover projects.
(map! :leader (:prefix "p" :desc "Discover projects in directory" :n "D" #'projectile-discover-projects-in-directory))

;; Quick cursor highlight on major change.
;; Testing turning it off.
;; (beacon-mode 1)
(setq beacon-size 10)
(setq beacon-blink-duration 0.1)

;; Pick from kill ring... with completion!
(global-set-key (kbd "M-p") #'counsel-yank-pop)


;; smooth scrolling config

(require 'scroll-on-jump)

;; most of the "Scroll Smoother!" advices below are from the default "Complete Example" config (https://gitlab.com/ideasman42/emacs-scroll-on-jump)
;; I've added the following advices:
;; * goto-first-line
;; * goto-line
;; * the better-jumper ones
;; * the scroll-page ones

(with-eval-after-load 'evil
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  (scroll-on-jump-advice-add evil-goto-first-line)
  (scroll-on-jump-advice-add better-jumper-jump-forward)
  (scroll-on-jump-advice-add better-jumper-jump-backward)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-page-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-page-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom)
  (scroll-on-jump-with-scroll-advice-add evil-goto-line))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

(global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
(global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk))

(setq scroll-on-jump-duration 1)

(setq eradio-channels '(
                        ("SomaFM - Mission Control" . "https://somafm.com/missioncontrol.pls")
                        ("SomaFM - Cliqhop IDM" . "https://somafm.com/cliqhop.pls")
                        ("SomaFM - Beat Blender" . "https://somafm.com/beatblender.pls")
                        ("SomaFM - Fluid" . "https://somafm.com/fluid.pls")
                        ("SomaFM - Secret Agent" . "https://somafm.com/secretagent.pls")
                        ("SomaFM - DEF CON" . "https://somafm.com/defcon.pls")
                        ("SomaFM - Space Station Soma" . "https://somafm.com/spacestation.pls")
                        ("SomaFM - Deep Space One" . "https://somafm.com/deepspaceone.pls")
                        ("SomaFM - Groove Salad." . "https://somafm.com/groovesalad.pls")))

;; Config for when it's on:
(remove-hook 'doom-modeline-mode-hook 'column-number-mode)
(remove-hook 'doom-modeline-mode-hook 'size-indication-mode)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(line-number-mode 0)

(map! :leader (:prefix "b" :desc "Rename buffer" :n "R" #'rename-buffer))

;; Turn the modeline on and off.
(defun toggle-mode-line-buffer () (interactive) (hide-mode-line-mode 'toggle) (redraw-display))

(defun toggle-mode-line-global () (interactive) (if global-hide-mode-line-mode (global-hide-mode-line-mode 0) (global-hide-mode-line-mode)) (redraw-display))

(map! :leader
      (:prefix "t"
       :desc "toggle radio" :n "m" #'eradio-toggle
       :desc "toggle modeline for buffer" :n "d" #'toggle-mode-line-buffer
       :desc "toggle modeline" :n "D" #'toggle-mode-line-global
       :desc "toggle pomodoro clock" :n "c" #'org-pomodoro
       :desc "play radio channel" :n "M" #'eradio-play))

(setq org-re-reveal-title-slide nil)
(setq org-re-reveal-theme "league")
(require 'org-tempo)
(require 'ox-reveal)
(setq org-reveal-highlight-css "%r/lib/css/vs.css")


(setq indium-chrome-executable "google-chrome-stable")


(map! :leader
      (:prefix "v"
       :desc "ibuffer filter by content" :n "u" #'ibuffer-update
       :desc "ibuffer filter by content" :n "/" #'ibuffer-filter-by-content
       :desc "ibuffer filter by mode" :n "m" #'ibuffer-filter-by-mode
       :desc "remove ibuffer filter" :n "?" #'ibuffer-filter-disable))

;; use web mode for ejs
(add-to-list 'auto-mode-alist
             '("\\.ejs\\'" . gfm-mode))

;; Use RJSX's version of js2-mode for .js files.
;; This is one way to make sure that JSX files in .js
;; files get handled properly.
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . rjsx-mode))

(add-hook! 'i3wm-config-mode-hook #'rainbow-mode)

; Pomodoro settings

;; Mode-line appearance
(setq org-pomodoro-format "P%s")
(setq org-pomodoro-time-format "%m")
(setq org-pomodoro-long-break-format "L~%s")
(setq org-pomodoro-short-break-format "S~%s")

;; Allow manual breaks in Pomodoro.
(setq org-pomodoro-manual-break t)

;; A canceled Pomodoro is the same as a completed Pomodoro.
(setq org-pomodoro-keep-killed-pomodoro-time t)


;; set up exec-path-from-shell
(when (or (memq window-system '(mac ns x)) (daemonp))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; open links through ace-link
(define-key help-mode-map (kbd "M-o") #'ace-link-help)
(define-key compilation-mode-map (kbd "M-o") #'ace-link-compilation)
(map! :map org-mode-map :n (kbd "M-o") #'ace-link-org)
(map! :map mu4e-view-mode-map :n (kbd "M-o") #'ace-link-org)

; some available keybinding prefixes
;; SPC l
;; SPC y
;; SPC and any capital letter

