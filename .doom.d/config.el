(mapc 'load (file-expand-wildcards "~/.doom.d/crj-modules/*.el"))

; TODO add function to swap ctrl and caps and back
; TODO OS should open files with emacsclient
; TODO experiment with de-bounce in Planck settings
; TODO add space-W for deleting word and capitalizing next one
; TODO space-something for lower-casing word and entering insert mode a word before it
;   "Help me!" -> "| help me!"
; TODO run projectile-cache-update (or whatever) every time we create a file
;; ensure that updating the cache is handled gracefully if we're not in a project
; TODO remove spelling tasing as you go
;; TODO above all, what is the order code runs?
;; TODO no completion in minibuffer without Ctrl-space
;; TODO no completion in comments
;; TODO Add variable for whether we're using variable pitch as code or not
;;   TODO make it easy to do one or the other
;; TODO turn off line completion
;; TODO get mu4e working
;; TODO improve everywhere hooks
;;   TODO start in Normal Mode
;;   TODO delete extra inter-paragraph whitespace
;;   TODO put focus back on input box? (maybe vimium is interfering with this?)
;; TODO swap =`= and ='=
;; TODO see about flyspell-popup
;; TODO add command for going to last misspelled word and popping up menu
;; TODO get dark mode system-wide, plug in as much as possible (browser, Discord, Emacs)
;; TODO debug ob-restclient
;; TODO add binding for doom-kill-buffer-and-windows
;; TODO Use variable pitch font again (mixing in text mode?)
;; TODO Remove Ctrl-J and Ctrl-K from vertico mapping
;; TODO BUG upgrading Doom loses our `eradio` history
;; TODO make adjustments to faces only if we're in a certain theme
;; TODO turn off repeat for keys
;; TODO change config filenames to avoid collisions
;; TODO add custom i3blocks to yadm
;; TODO fork and set upstream for i3blocks-contrib
;; TODO add different audio sources to i3blocks
;; TODO Add color/icon to custom battery i3block according to:
;;; - charging status
;;; - battery level
;; TODO improve/understand Corfu UX in eshell buffers
;; TODO Improve window-resize hydra.
;; TODO add feature to gtd-style next function to check if next todo is one it should mark todo
;; TODO fix beespell when dictionary buffer is closed
;; TODO add regex to org-agenda-file-regexp to exclude files with name "archive"
;; TODO FIXED??? BUG some JS files not starting with RJSX Mode
;; TODO remove Forge-Corfu autocomplete on !
;; TODO Function to title-case each markdown and org heading
;; TODO make config literate
;; TODO add file templates
;; TODO remove Ctrl-h and Ctrl-K from Org mode insert mode bindings
;; TODO make markdown's enter work on opening a new line
;; TODO port markdown's enter over to org
;; TODO make hl-line-subtle deal with other themes
;; TODO remove markdown meta-p mapping
;; TODO Figure out keybinding prefixes. (Check out "leader m" in org-stuff.)
;; DONE Fix code in text modes not scaling with regular text
;; DONE check Ctrl-O with other send-cursor-back setting
;; DONE Set up SQL LSP
;; DONE add key command ispell-buffer
;; DONE toggle line number type globally - and maybe switch to Modus Vivendi? And reverse toggle?
;; DONE add variable pitch font to comments?
;; DONE write theme-switcher function
;; DONE function to rename buffer with the project name as a prefix

(defun rename-buffer-with-project-name-prefix ()
"Prompts the user to rename the buffer, supplying the project prefix."
  (interactive)
  (let* ((project-prefix (concat (projectile-default-project-name (projectile-project-name)) "-"))
         (prompt (concat "New Buffer Name: " project-prefix))
         (name (concat project-prefix (read-string prompt))))
    (rename-buffer name)))

; Projects
;; TODO sexp/target text objects
;; TODO space liner (look to evil-surround)
;; TODO nocturn.el - runs hooks on daylight change
;; TODO Quokka Thing

                                        ; Better window management.
;; Reverse the shortcuts between window splitting with follow vs. without.
;; This is because I'm a lot more likely to want to do something with the new split immediately than later.
;; Also, use the hydra as a better UI for window management.
;; Lastly, make window movement wrap around.
(map! :leader (:prefix "w"
               :desc "split window vertically and follow"
                 :n "v" #'+evil/window-vsplit-and-follow
               :desc "split vertically"
                 :n "V" #'evil-window-vsplit
               :desc "split window and follow"
                 :n "s" #'+evil/window-split-and-follow
               :desc "split window"
                 :n "S" #'evil-window-split
               :desc "Activate Window Hydra."
                 :n "a" #'hydra/crj-window-nav/body))

(setq windmove-wrap-around t)


;; initialize the targets package
(targets-setup t)

(map! :map evil-normal-state-map "SPC TAB" #'evil-switch-to-windows-last-buffer)

;; operator that replaces a motion/text object with what's in a register (the " register by default).
(map! :map evil-normal-state-map :leader :desc "Replace with register" "r" #'evil-replace-with-register)

(setq evil-ex-substitute-global 1)
(setq evil-want-minibuffer nil)
(setq +evil-want-o/O-to-continue-comments nil)

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
;; (slow) syntax coloration in markdown blocks
(setq markdown-fontify-code-blocks-natively t)

(map! :map (evil-markdown-mode gfm-mode) :leader
      (:prefix "e"
       :desc "Add markdown item" :n "i" #'markdown-insert-list-item
       :desc "Go to next section" :n "j" #'markdown-forward-same-level
       :desc "Go to previous section" :n "k" #'markdown-backward-same-level
       :desc "Repair list" :n "r" #'org-list-repair
       :desc "Toggle checkbox" :n "m" #'markdown-toggle-gfm-checkbox))

;; Set gj/gk to vim's visual line navigation instead of markdown's headline-jumping.
(map!
 :map (markdown-mode-map gfm-mode-map org-mode-map)
  :n "gj" nil
  :n "gk" nil)
(map!
 :map (markdown-mode-map gfm-mode-map org-mode-map)
  :n "gj" #'evil-next-visual-line
  :n "gk" #'evil-previous-visual-line)

;; turn on transparency to start with
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 75))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 75)))

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

(map! :leader
      (:prefix "z"
       :desc "zoom in" :n "j" #'crj/zoom-in
       :desc "zoom out" :n "k" #'crj/zoom-out
       :desc "reset zoom" :n "b" #'crj/zoom-reset
       :desc "zoom in buffer" :n "J" #'crj/zoom-in-all-buffers
       :desc "zoom out buffer" :n "K" #'crj/zoom-out-all-buffers
       :desc "zoom out buffer" :n "B" #'crj/zoom-reset-all-buffers
       :desc "zoom hydra" :n "z" #'crj/hydra/text-zoom/body
       :desc "toggle ligatures globally" :n "l" #'global-auto-composition-mode
       :desc "toggle ligatures in buffer" :n "L" #'auto-composition-mode
       :desc "toggle prettier globally" :n "p" #'global-prettier-mode
       :desc "toggle transparency" :n "t" #'toggle-transparency))

;; start every emacs frame with transparency
;; (add-hook 'emacs-startup-hook 'toggle-transparency)


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

(defun crj/make-custom-face-adjustments ()
  "Customizations to faces whenever the theme is changed.

Fixes many things according to how the author likes them.

Also fixes a pernicious issue where line numbers become variable pitch fonts along with everything else. There's gotta be a better way to fix that than this, but... this works."

  (interactive)
  ;; (custom-set-faces
  ;;  '(fixed-pitch ((t :family crj/variable-font :inherit 'default)))
  ;;  '(highlight ((t :background "#b5d0ff")))
  ;;  '(line-number ((t :family "Hack")))
  ;;  '(mode-line-highlight ((t :foreground "#d7d7d7" :background "#0030b4")))
  ;;  '(success ((t :foreground "#0031a9")))
  ;;  '(line-number-current-line ((t :family "Hack"))))
  )

(add-hook 'doom-load-theme-hook #'crj/make-custom-face-adjustments)

(defun crj/swap-chars ()
  (interactive)
  (when (evil-normal-state-p)
    (transpose-chars 1)
    (backward-char)))

;; TODO make this its own function rather than advice.
(advice-add 'emojify-insert-emoji :after 'crj/swap-chars)

(defun crj/evil-tranpose-chars ()
  "Transpose characters as one evil action.

        Wraps the function `transpose-chars' so that it's more in the style of Evil Mode/Vim. (See info node `(evil)Overview')

        - Acts on the current character and the one to the right, which is more in line with Vim's Normal Mode style.
        - Adds the entire process as one action, adding undo/repeat ability.

        This differs greatly from the more Emacs-like `transpose-chars', which allows you to drag a character forward as far as you want, using a count,, but this author found that he preferred the atomicity of Normal Mode.

See `transpose-chars' for more info on the original function."
  (interactive)
  (evil-with-undo
    (forward-char)
    (transpose-chars 1)
    (backward-char 2)))

(map! :leader :desc "Evil transpose characters" :n "T"  #'crj/evil-tranpose-chars)

;; For doom-big-font-mode
(setq doom-big-font-increment 8)

;; Use smart parens version of showing matching pairs instead of the built-in show-paren method. Includes strings, and you can customize it to include more.
(show-paren-mode -1)
(show-smartparens-global-mode)
(setq sp-show-pair-from-inside nil)

                                        ; use better emojis (requires this font!)
(if (>= emacs-major-version 27)
    (set-fontset-font t '(#x1f000 . #x1faff)
                      (font-spec :family "Noto Color Emoji")))
;; Theme Settings
;;; Modus
(require 'modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-syntax '(alt-syntax yellow-comments green-strings))
(setq modus-themes-paren-match '(intense underline))
(setq modus-themes-subtle-line-numbers nil)
(setq modus-themes-deuteranopia t)
(setq modus-themes-markup '(background))
(setq modus-themes-region '(no-extend bg-only accented))
(setq modus-themes-hl-line '(intense underline))
(setq modus-themes-headings
      (quote ((1 . (rainbow 1.8))
              (2 . (rainbow 1.6))
              (3 . (rainbow 1.4))
              (4 . (rainbow 1.2)))))
(set-face-attribute 'modus-themes-hl-line nil
                    :extend nil
                    :background 'unspecified)
(setq modus-themes-completions (quote ((matches . (intense background underline bold))
                                       (selection . (accented intense bold))
                                       (popup . (accented intense bold)))))


;;; doom-zenburn
(setq doom-zenburn-comment-bg t)
(setq doom-zenburn-brighter-comments t)
(setq doom-zenburn-brighter-modeline t)

;;; zenburn
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)

;;; set fave themes
(setq crj/working-theme-daytime 'modus-operandi)
(setq crj/presentation-theme-daytime 'modus-operandi)
(setq crj/working-theme-nighttime 'modus-vivendi)
(setq crj/presentation-theme-nighttime 'modus-operandi)

;; But turn them off in text and shell modes.
(defun crj/turn-off-ligatures-in-buffer () (auto-composition-mode -1))
(setq crj/no-ligatures-hooks '(text-mode-hook vterm-mode-hook eshell-mode-hook))
(dolist (hook crj/no-ligatures-hooks)
  (add-hook hook #'crj/turn-off-ligatures-in-buffer))

(setq crj/daytime-p t)
(setq crj/presentation-mode-p nil)

(defun crj/get-current-theme ()
  "Get current theme, depending on time of day and presentation mode."
  (cond ((and crj/daytime-p crj/presentation-mode-p)
         crj/presentation-theme-daytime)
        ((and crj/daytime-p (not crj/presentation-mode-p))
         crj/working-theme-daytime)
        ((and (not crj/daytime-p) crj/presentation-mode-p)
         crj/presentation-theme-nighttime)
        ((and (not crj/daytime-p) (not crj/presentation-mode-p))
         crj/working-theme-nighttime)))

(setq markdown-header-scaling t)
(setq markdown-header-scaling-values '(2.5 2.0 2.0 1.5 1.0 1.0))

(defun crj/switch-to-appropriate-theme ()
  "Switch to the theme appropriate to the time of day and presentation mode."
  (mapc #'disable-theme custom-enabled-themes)
  (let ((new-theme (crj/get-current-theme)))
    (load-theme new-theme t)))

(crj/switch-to-appropriate-theme)
(crj/make-custom-face-adjustments)

(set-face-attribute 'highlight nil :background "#b5d0ff")

;; Constants for crj/toggle-presentation-mode
(setq crj/working-mode-line-height 160)
(setq crj/presentation-mode-line-height 320)
(setq crj/working-line-number-type 'relative)
(setq crj/presentation-line-number-type t)

(defun crj/toggle-presentation-mode ()
  "Toggles between presenting code and working with code.

It toggles:

- theme (ensuring we use a light theme),
- line number type,
- ligatures,
- and text size, both in regular buffer and the mode line."

  (interactive)
  (if crj/presentation-mode-p
      (progn
        (setq
         crj/presentation-mode-p nil
         display-line-numbers-type crj/working-line-number-type)
        (global-display-line-numbers-mode 1)
        (global-hl-line-mode -1)
        (global-auto-composition-mode 1)
        (set-face-attribute 'mode-line nil
                            :height crj/working-mode-line-height))
    (setq
     crj/presentation-mode-p t
     display-line-numbers-type crj/presentation-line-number-type)
    (global-display-line-numbers-mode 1)
    (global-hl-line-mode)
    (global-auto-composition-mode -1)
    (set-face-attribute 'mode-line nil
                        :height crj/presentation-mode-line-height))
  (crj/switch-to-appropriate-theme)
  (doom-big-font-mode))

;; Great if you go back to fixed-pitch programming fonts!
;; (use-package mixed-pitch
;;   :hook
;;   (text-mode . mixed-pitch-mode))
;; ;; If you use `org' and don't want your org files in the default location below,

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")
(setq debug-ignored-errors '("^Exit the snippet first!$" "^End of line$" "^Beginning of line$" beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive))
                                        ; line number settings

;; always display line numbers
(global-display-line-numbers-mode t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type crj/working-line-number-type)
;; soft wrap lines
(global-visual-line-mode 1)

(defun crj/toggle-theme-for-time-of-day ()
  (interactive)
  (setq crj/daytime-p (not crj/daytime-p))
  (crj/switch-to-appropriate-theme)
  (crj/make-custom-face-adjustments))

;; Best way to remove global-hl-line-mode in Doom.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

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
("Chill Lounge Florida" . "https://www.internet-radio.com/servers/tools/playlistgenerator/?u=http://us5.internet-radio.com:8283/listen.pls&t=.pls")
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
(map! :leader (:prefix "b" :desc "Rename buffer" :n "R" #'rename-buffer-with-project-name-prefix))

;; Doom Modeline settings.
(remove-hook 'doom-modeline-mode-hook 'column-number-mode)
(remove-hook 'doom-modeline-mode-hook 'size-indication-mode)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(setq doom-modeline-indent-info t)
(line-number-mode 0)

;; Turn the modeline on and off.
(defun toggle-mode-line-buffer () (interactive) (hide-mode-line-mode 'toggle) (redraw-display))

(defun toggle-mode-line-global () (interactive)
       (if global-hide-mode-line-mode
           (global-hide-mode-line-mode 0)
         (global-hide-mode-line-mode))
       (redraw-display))

                                        ; toggle for
;; radio
;; pomodoro
;; modeline
;; presenting code

(map! :leader
      (:prefix "t"
       :desc "toggle radio" :n "m" #'eradio-toggle
       :desc "play radio channel" :n "M" #'eradio-play
       :desc "toggle pomodoro clock" :n "c" #'org-pomodoro
       :desc "toggle modeline" :n "D" #'toggle-mode-line-global
       :desc "toggle day/night themes" :n "n" #'crj/toggle-theme-for-time-of-day
       :desc "toggle code presentation" :n "p" #'crj/toggle-presentation-mode
       :desc "org tree slide mode" :n "P" #'org-tree-slide-mode
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

;; Fixes a bug in RJSX Mode where it doesn't handle the fragment syntax.
(defun crj/rjsx-electric-gt-fragment-a (n)
  (if (or (/= n 1) (not (and (eq (char-before) ?<) (eq (char-after) ?/)))) 't
    (insert ?> ?<)
    (backward-char)))

(advice-add #'rjsx-electric-gt :before-while #'crj/rjsx-electric-gt-fragment-a)

;; Opening very large files.
(require 'vlf-setup)
(setq vlf-application 'dont-ask)

;; Lisp Layer
;; (use-package symex
;;   :init
;;   (setq symex--user-evil-keyspec
;;         '(("j" . symex-go-up)
;;           ("k" . symex-go-down)
;;           ("C-j" . symex-climb-branch)
;;           ("C-k" . symex-descend-branch)
;;           ("M-j" . symex-goto-highest)
;;           ("M-k" . symex-goto-lowest)
;;           ("^" . symex-goto-first)
;;           ("K" . +lookup/documentation)
;;           ("gK" . paredit-raise-sexp)))
;;   :config
;;   (symex-initialize))

;; stay in Symex editing by default in lisp
;; (map! :map emacs-lisp-mode-map :i "<escape>" nil)
;; (map! :map emacs-lisp-mode-map :i "<escape>" #'(lambda ()
;;                                                  (interactive)
;;                                                  (evil-normal-state)
;;                                                  (symex-mode-interface)))
;; (map! :map emacs-lisp-mode-map
;;  :n "M-<escape>" #'symex-mode-interface)

(map! :map Info-mode-map :n "q" nil)

(map! :leader (:prefix "b"
               :desc "Save and close buffer." :n "q" #'doom/save-and-kill-buffer))

(setq org-babel-header-args:sql-mode '((:product . :postgres) (:session . :any))
      org-babel-default-header-args:sql-mode '((:product . "postgres"))
      sql-connection-alist '(("animes" (sql-product 'postgres) (sql-user "abbreviatedman") (sql-database "animes_dev") (sql-server "")))
      )
;; PORT=3003
;; PG_HOST=localhost
;; PG_PORT=5432
;; PG_DATABASE=animes_dev
;; PG_USER=postgres
(setq sql-server "localhost"
      sql-port 5432
      sql-product "postgres")

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(add-hook! '(sql-mode-hook sql-interactive-mode-hook) #'sqlup-mode)
(remove-hook 'org-mode-hook #'sqlup-mode)
(map! "C-c U" 'sqlup-capitalize-keywords-in-region
        "C-c u" 'sqlup-capitalize-keywords-in-buffer
        :map sql-interactive-mode-map
        "M-RET" #'sql-accumulate-and-indent)

(after! sqlup-mode
  (add-to-list 'sqlup-blacklist "name"))


;; Browse with EWW. (& to switch to open in default browser after.)
(setq browse-url-browser-function 'eww)

; some available keybinding prefixes
;; SPC l
;; SPC and any capital letter
