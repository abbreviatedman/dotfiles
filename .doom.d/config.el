(add-to-list 'load-path "~/.doom.d/modules/")
(add-to-list 'load-path "~/.doom.d/modules/packages/")
(load-library "crj-utilities")
(load-library "version-control")
(load-library "secrets")
(load-library "org-stuff")
(load-library "completion")
(load-library "space-liner")
(load-library "config-dired")
(load-library "terminals")
(load-library "aliases")
;; (load-library "mail")
;; (load-library "macos")
;; (load-library "better-react-mmm-mode")
(load-library "linux")
(load-library "crj-spell")
(load-library "beespell")
(load-library "window-hydra")
(load-library "file-management")

;; TODO get mu4e working
;; TODO get working on Next Cloud
;; TODO Improve window-resize hydra.
;; TODO add feature to gtd-style next function to check if next todo is one it should mark todo
;; TODO fix beespell when dictionary buffer is closed
;; TODO add regex to org-agenda-file-regexp to exclude files with name "archive"
;; TODO add variable pitch font to comments?
;; TODO remove Forge-Corfu autocomplete on !
;; TODO Function to title-case each markdown and org heading
;; TODO make config literate
;; TODO add file templates
;; TODO make returning to transparency not additive
;; TODO remove Ctrl-h and Ctrl-K from Org mode insert mode bindings
;; TODO make markdown's enter work on opening a new line
;; TODO port markdown's enter over to org
;; TODO remove markdown meta-p mapping
;; DONE Figure out keybinding prefixes. (Check out "leader m" in org-stuff.)
;; DONE toggle line number type globally - and maybe switch to Modus Vivendi? And reverse toggle?

; Better window management.
;; Reverse the shortcuts between window splitting with follow vs. without.
;; This is because I'm a lot more likely to want to do something with the new split immediately than later.
;; Also, use the hydra as a better UI for window management.
;; Lastly, make window movement wrap around.
(map! :leader (:prefix "w"
               :desc "split window vertically and follow" :n "v" #'+evil/window-vsplit-and-follow
               :desc "split vertically" :n "V" #'evil-window-vsplit
               :desc "split window and follow" :n "s" #'+evil/window-split-and-follow
               :desc "split window" :n "S" #'evil-window-split
               :desc "Activate Window Hydra." :n "a" #'hydra/crj-window-nav/body))
(setq windmove-wrap-around t)


;; initialize the targets package
(targets-setup t)

(map! :map evil-normal-state-map "SPC TAB" #'evil-switch-to-windows-last-buffer)

;; operator that replaces a motion/text object with what's in a register (the " register by default).
(map! :map evil-normal-state-map :leader :desc "Replace with register" "r" #'evil-replace-with-register)

;; Make substitutions global by default.
(setq evil-ex-substitute-global 1)

;; Enable evil in the mini-buffer?
(setq evil-want-minibuffer nil)

;; get rid of prompts
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-emacs nil)

; Markdown

;; Promote/demote headlines.
(map! :map markdown-mode-map "M-l" #'markdown-demote)
(map! :map markdown-mode-map "M-h" #'markdown-promote)
;; Tell markdown mode to stop over-indenting lists.
(setq markdown-list-indent-width 2)
;; Make markdown continue lists on enter.
(setq markdown-indent-on-enter 'indent-and-new-item)

(map! :map (evil-markdown-mode gfm-mode) :leader
      (:prefix "e"
       :desc "Add markdown item" :n "i" #'markdown-insert-list-item
       :desc "Go to next section" :n "j" #'markdown-forward-same-level
       :desc "Go to previous section" :n "k" #'markdown-backward-same-level
       :desc "Repair list" :n "r" #'org-list-repair
       :desc "Toggle checkbox" :n "m" #'markdown-toggle-gfm-checkbox))

;; Set gj/gk to vim's visual line navigation instead of markdown's headline-jumping.
(evil-define-key '(normal visual) markdown-mode-map
  "gj" #'evil-next-visual-line
  "gk" #'evil-previous-visual-line)

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

;; (defun add-fira-code-mode-hook ()
;;   (interactive)
;;   (add-hook 'prog-mode-hook 'fira-code-mode))
;; (defun remove-fira-code-mode-hook ()
;;   (interactive)
;;   (remove-hook 'prog-mode-hook 'fira-code-mode))

(map! :leader
      (:prefix "z"
       :desc "zoom in" :n "i" #'doom/increase-font-size
       :desc "zoom out" :n "o" #'doom/decrease-font-size
       :desc "zoom in buffer" :n "I" #'text-scale-increase
       :desc "zoom out buffer" :n "O" #'text-scale-decrease
       :desc "zoom hydra" :n "z" #'+hydra/text-zoom/body
       ;; :desc "turn ligatures on globally" :n "+" #'add-fira-code-mode-hook
       ;; :desc "turn ligatures off globally" :n "-" #'remove-fira-code-mode-hook
       ;; :desc "toggle ligatures for this file" :n "l" #'fira-code-mode
       :desc "toggle prettier globally" :n "p" #'global-prettier-mode
       :desc "toggle transparency" :n "t" #'toggle-transparency))

;; start every emacs frame with transparency
(add-hook 'emacs-startup-hook 'toggle-transparency)

; Ligatures
;; Start with ligatures enabled.
;; turned off for now
;; (add-hook 'prog-mode-hook 'fira-code-mode)
;; (setq fira-code-mode-disabled-ligatures '("x" "[]" "+" ":" ">="))

;; (use-package python
;;   :config
;;   (setq python-prettify-symbols-alist (delete '("and" . 8743) python-prettify-symbols-alist))
;;   (setq python-prettify-symbols-alist (delete '("or" . 8744) python-prettify-symbols-alist)))

; Indentation

;; Tabs should be 2 spaces by default.
(setq! indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq! tab-width 2)
(setq! evil-shift-width 2)
(setq! tab-stop-list (number-sequence 2 120 2))
(dtrt-indent-mode)

;; use indent of 2 for html
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Stop clobbering my system clipboard, emacs you fiend.
(setq save-interprogram-paste-before-kill t)


; Font Settings

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 24))
;; This was necessary to fix some line of code somewhere giving my
;; Fixed Pitch fonts an absolute height. This meant they didn't scale
;; to other font sizes. Someday, we'll find the culprit!
(set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font Mono" :height 1.0)


; use better emojis (requires this font!)
(if (>= emacs-major-version 27)
    (set-fontset-font t '(#x1f000 . #x1faff)
              (font-spec :family "Noto Color Emoji")))

;; Theme Settings
;;; Modus Vivendi
(require 'modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-syntax '(alt-syntax yellow-comments green-strings))
(setq modus-themes-paren-match '(intense underline bold))
(setq modus-themes-headings
      '((t . rainbow)))
(setq modus-themes-scale-headings t)
(setq modus-themes-completions 'opinionated)
(setq modus-themes-hl-line '(intense accented))
(setq modus-themes-subtle-line-numbers t)
(setq modus-themes-deuteranopia t)

;;; doom-zenburn
(setq doom-zenburn-comment-bg t)
(setq doom-zenburn-brighter-comments t)
(setq doom-zenburn-brighter-modeline t)

;;; zenburn
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)

;;; set fave themes
(setq crj/working-theme 'zenburn)
(setq crj/presentation-theme 'modus-vivendi)

;;; Modus Vivendi is good for presenting code.
;;;; (setq doom-theme 'modus-vivendi)
;;; But let's use Zenburn for solo work.
(setq doom-theme crj/presentation-theme)
(setq doom-theme crj/working-theme)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

; line number settings

;; always display line numbers
(global-display-line-numbers-mode t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;; soft wrap lines
(global-visual-line-mode 1)

(defun crj/toggle-presentation-mode ()
  (interactive)
  (if (eq display-line-numbers-type t)
      (progn
        (setq display-line-numbers-type 'relative)
        (disable-theme crj/presentation-theme)
        (load-theme crj/working-theme t)
        (global-display-line-numbers-mode 1))
    (setq display-line-numbers-type t)
    (disable-theme crj/working-theme)
    (load-theme crj/presentation-theme t)
    (global-display-line-numbers-mode 1)))

; Search specific engines.
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
       :desc "Next todo GTD-style" :n "m" #'(lambda ()
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

(evil-define-key '(normal visual) markdown-mode-map
  "gj" #'evil-next-visual-line
  "gk" #'evil-previous-visual-line)

;; open the result of a search in a new frame
(map! :leader
      :desc "find file other frame" "o f" #'find-file-other-frame)

;; Title Case
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

(setq message-log-max 100000)
;; ;; set spelling dictionary
;; (setq ispell-dictionary "en")
;; ;; spelling dictionary location
;; (setq ispell-personal-dictionary "~/.doom.d/spelling/en.pws")

(setq undo-fu-allow-undo-in-region t)

;; Automatic formatting

;; configure prettier integration
(add-hook 'after-init-hook #'global-prettier-mode)

;; black integration
(setq blacken-only-if-project-is-blackened t)

(setq lsp-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'")

(setq projectile-track-known-projects-automatically nil)

;; Opens minibuffer to select a root folder from which to discover projects.
(map! :leader (:prefix "p" :desc "Discover projects in directory" :n "D" #'projectile-discover-projects-in-directory))

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

;; Rename buffers.
(map! :leader (:prefix "b" :desc "Rename buffer" :n "R" #'rename-buffer))

;; Doom Modeline settings.
(remove-hook 'doom-modeline-mode-hook 'column-number-mode)
(remove-hook 'doom-modeline-mode-hook 'size-indication-mode)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(line-number-mode 0)

;; Turn the modeline on and off.
(defun toggle-mode-line-buffer () (interactive) (hide-mode-line-mode 'toggle) (redraw-display))

(defun toggle-mode-line-global () (interactive) (if global-hide-mode-line-mode (global-hide-mode-line-mode 0) (global-hide-mode-line-mode)) (redraw-display))

; toggle for
;; radio
;; pomodoro
;; modeline
;; presenting code (theme and line number settings)

(map! :leader
      (:prefix "t"
       :desc "toggle radio" :n "m" #'eradio-toggle
       :desc "play radio channel" :n "M" #'eradio-play
       :desc "toggle pomodoro clock" :n "c" #'org-pomodoro
       :desc "toggle modeline" :n "D" #'toggle-mode-line-global
       :desc "toggle code presentation" :n "P" #'crj/toggle-presentation-mode
       :desc "toggle modeline for buffer" :n "d" #'toggle-mode-line-buffer))

;; Indium.
(setq indium-chrome-executable "google-chrome-stable")

;; Ibuffer commands.
(map! :leader
      (:prefix "v"
       :desc "ibuffer filter by content" :n "u" #'ibuffer-update
       :desc "ibuffer filter by content" :n "/" #'ibuffer-filter-by-content
       :desc "ibuffer filter by mode" :n "m" #'ibuffer-filter-by-mode
       :desc "remove ibuffer filter" :n "?" #'ibuffer-filter-disable))

;; Uses Vim's original meaning of `G`, which puts you at the last NON-EMPTY line.
(defun crj/end-of-buffer ()
  "Go to beginning of last line in buffer.
If last line is empty, go to beginning of penultimate one
instead."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line (and (looking-at-p "^$") 0)))

(define-key evil-normal-state-map "G" #'crj/end-of-buffer)

(add-hook! 'rjsx-mode-hook #'jest-minor-mode #'emmet-mode)

;; i3wm mode.
(add-hook! 'i3wm-config-mode-hook #'rainbow-mode)

;; Show digraphs.
(map! :n "SPC h D" #'evil-ex-show-digraphs)

; Pomodoro settings

;; Allow manual breaks in Pomodoro.
(setq org-pomodoro-manual-break t)

;; A canceled Pomodoro is the same as a completed Pomodoro.
(setq org-pomodoro-keep-killed-pomodoro-time t)

;; Mode-line appearance
(setq org-pomodoro-format "P%s")
(setq org-pomodoro-time-format "%m")
(setq org-pomodoro-long-break-format "L~%s")
(setq org-pomodoro-short-break-format "S~%s")

;; set up exec-path-from-shell
(when (or (memq window-system '(mac ns x)) (daemonp))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; open links through ace-link
(define-key help-mode-map (kbd "M-o") #'ace-link-help)
;; (define-key compilation-mode-map (kbd "M-o") #'ace-link-compilation)
(map! :map org-mode-map :n (kbd "M-o") #'ace-link-org)
(map! :map mu4e-view-mode-map :n (kbd "M-o") #'ace-link-help)

;; Switch frames. (Particularly useful on macOS.)
(map! :leader (:prefix "w" :n "f" #'other-frame))

;; use subwords always
;; makes 'w' work with the subwords of a camelCase word
;; use 'W' for the whole thing
;; or the text object 'o' for a symbol
(global-subword-mode)
;; weather config
(setq wttrin-default-cities '("Manhattan" ))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
(map! :leader (:prefix "o" :n "w" #'wttrin))

;; Simpler binding for Emacs Everywhere
(map! :map emacs-everywhere-mode-map "C-c DEL" #'emacs-everywhere-finish-or-ctrl-c-ctrl-c)

;; Lisp structural editing commands without a lispy-like mode.
(map! :leader
  (:prefix ("y" . "lisp")
   :desc "slurp" "s" #'sp-forward-slurp-sexp
   :desc "barf" "b" #'sp-forward-barf-sexp
   :desc "raise" "r" #'sp-raise-sexp))

(fset 'convert-react-class-to-functional-component
   (kmacro-lambda-form [?g ?g ?/ ?c ?l ?a ?s ?s return ?c ?i ?w ?c ?o ?n ?s ?t escape ?2 ?W ?c ?2 ?w ?= ?  ?\( ?\) ?  ?= ?> escape ?/ ?r ?e ?n ?d ?e ?r return ?$ ?% ?d ?d ?N ?d ?d] 0 "%d"))

;; Auto save the Messages buffer too.
(defun save-messages-buffer ()
  (with-current-buffer (get-buffer "*Messages*")
    (append-to-file nil nil "~/.messages-history.txt")))


(add-hook! 'auto-save-hook #'save-messages-buffer)


;; Opening very large files.
(require 'vlf-setup)
(setq vlf-application 'dont-ask)
; some available keybinding prefixes
;; SPC l
;; SPC and any capital letter
