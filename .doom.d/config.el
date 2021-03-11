(add-to-list 'load-path "~/.doom.d/modules/")
(load-library "version-control")
(load-library "secrets")
(load-library "config-org-agenda")
(load-library "completion")
(load-library "snipe-and-transpose")
(load-library "space-liner")
(load-library "config-dired")
(load-library "terminals")


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
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))

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

(defun add-fira-code-mode-hook ()
  (interactive)
  (add-hook 'prog-mode-hook 'fira-code-mode))
(defun remove-fira-code-mode-hook ()
  (interactive)
  (remove-hook 'prog-mode-hook 'fira-code-mode))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("z" . "font presentation")
       :desc "zoom in" "i" #'doom/increase-font-size
       :desc "zoom out" "o" #'doom/decrease-font-size
       :desc "zoom in buffer" "I" #'text-scale-increase
       :desc "zoom out buffer" "O" #'text-scale-decrease
       :desc "zoom hydra" "z" #'+hydra/text-zoom/body
       :desc "turn ligatures on globally" "+" #'add-fira-code-mode-hook
       :desc "turn ligatures off globally" "-" #'remove-fira-code-mode-hook
       :desc "toggle ligatures for this file" "l" #'fira-code-mode
       :desc "toggle prettier globally" "p" #'global-prettier-mode
       :desc "toggle transparency" "t" #'toggle-transparency))


;; start every emacs frame with transparency
(add-hook 'emacs-startup-hook 'toggle-transparency)

;; TODO remove markdown meta-p mapping

;; start with ligatures in programming modes
(add-hook 'prog-mode-hook 'fira-code-mode)
(setq fira-code-mode-disabled-ligatures '("x" "[]"))

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
;; now good mappings

(map! :map (evil-markdown-mode gfm-mode) :leader
      (:prefix-map ("e" . "editing")
       :desc "Add markdown item" "i" #'markdown-insert-list-item
       :desc "Go to next section" "j" #'markdown-forward-same-level
       :desc "Go to previous section" "k" #'markdown-backward-same-level
       :desc "Repair list" "r" #'org-list-repair
       :desc "Toggle checkbox" "m" #'markdown-toggle-gfm-checkbox))

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
(setq ispell-personal-dictionary "/home/abbreviatedman/.doom.d/spelling/en.pws")


(setq undo-fu-allow-undo-in-region t)

;; configure prettier integration
(add-hook 'after-init-hook #'global-prettier-mode)

;; set where node is located
(setenv "NODE_PATH" nil)
(setenv "PATH" (concat (getenv "PATH") ":/home/abbreviatedman/.asdf/shims"))
(setq exec-path (append exec-path '("/home/abbreviatedman/.asdf/shims")))


;; Opens minibuffer to select a root folder from which to discover projects.
(map! :map :n :leader (:prefix-map ("p" . "project") :desc "Discover projects in directory" "D" #'projectile-discover-projects-in-directory))

(beacon-mode 1)
(setq beacon-size 10)
(setq beacon-blink-duration 0.1)

;; Pick from kill ring... with completion!
(global-set-key (kbd "M-p") #'counsel-yank-pop)

;; Adds the ability to type Meta-n to go to the nth item in an Ivy buffer.
;; Not working on startup, but CAN be run after startup when the `ivy-minibuffer-map`
;; variable exists.
;;
;; Could map setting it up with a keypress when I discover the feature doesn't exist.
;; But there's probably a workaround where we can have it activate later on startup.
;;
;; When a file is first opened? We probably want to access it earlier.



;;
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
                        ("SomaFM - Groove Sala." . "https://somafm.com/groovesalad.pls")))

;; Keep modeline off by default
(global-hide-mode-line-mode 1)

;; Config for when it's on:
(remove-hook 'doom-modeline-mode-hook 'column-number-mode)
(remove-hook 'doom-modeline-mode-hook 'size-indication-mode)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(line-number-mode 0)

(map! :map :n :leader (:prefix-map ("b" . "buffer") :desc "Rename buffer" "R" #'rename-buffer))

;; Turn the modeline on and off.
(defun toggle-mode-line-buffer () (interactive) (hide-mode-line-mode 'toggle) (redraw-display))

(defun toggle-mode-line-global () (interactive) (if global-hide-mode-line-mode (global-hide-mode-line-mode 0) (global-hide-mode-line-mode)) (redraw-display))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("t" . "toggle")
       :desc "toggle radio" "m" #'eradio-toggle
       :desc "toggle modeline for buffer" "d" #'toggle-mode-line-buffer
       :desc "toggle modeline" "D" #'toggle-mode-line-global
       :desc "play radio channel" "M" #'eradio-play))

(setq org-re-reveal-title-slide nil)
(setq org-re-reveal-theme "league")
(require 'org-tempo)
(require 'ox-reveal)
(setq org-reveal-highlight-css "%r/lib/css/vs.css")


(setq indium-chrome-executable "google-chrome-stable")


(map! :map evil-normal-state-map :leader
      (:prefix-map ("v" . "view")
       :desc "ibuffer filter by content" "u" #'ibuffer-update
       :desc "ibuffer filter by content" "/" #'ibuffer-filter-by-content
       :desc "ibuffer filter by mode" "m" #'ibuffer-filter-by-mode
       :desc "remove ibuffer filter" "?" #'ibuffer-filter-disable))



;; some available keybinding prefixes
;; SPC l
;; SPC y
