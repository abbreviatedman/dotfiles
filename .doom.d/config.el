;; TODO fix display-buffer-alist in crj/git-cloud-save
;; TODO set :leader and number to persp switches
;; changing custom no-window shell command keybindings
;; TODO add persp buffer functions to SPC TAB map:
;; - SPC TAB a - add buffer to perspective
;; - SPC TAB k - remove buffer from perspective
;; - SPC TAB t - add buffer to perspective /temporarily/
;; - SPC TAB S - save workspaces to file
;; - SPC TAB L - load workspaces from file
;; TODO add doom modeline zoom-out
;; TODO add function to swap ctrl and caps and back
;; TODO OS should open files with emacsclient
;; TODO experiment with de-bounce in Planck settings
;; TODO add space-W for deleting word and capitalizing next one
;; TODO space-something for lower-casing word and entering insert mode a word before it
;;   "Help me!" -> "| help me!"
;; TODO run projectile-cache-update (or whatever) every time we create a file
;; ensure that updating the cache is handled gracefully if we're not in a project
;; TODO remove spelling tasing as you go
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

;; Projects
;; TODO sexp/target text objects
;; TODO space liner (look to evil-surround)
;; TODO nocturn.el - runs hooks on daylight change
;; TODO Quokka Thing
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(defun crj/split-line-between-pairs ()
  (interactive)
  (newline 2)
  (evil-indent-line (bol) (eol))
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "M-RET") #'crj/split-line-between-pairs)

(defun crj/split-line-between-pairs-advice ()
  (newline)
  (evil-indent-line (bol) (eol))
  (forward-line -1)
  (doom/forward-to-last-non-comment-or-eol))

(defun crj/split-pairs-maybe ()
  (interactive)
  (when (or (and (looking-back (regexp-quote "{") 1) (looking-at (regexp-quote "}")))
          (and (looking-back (regexp-quote "[") 1) (looking-at (regexp-quote "]"))))
      (crj/split-line-between-pairs-advice)))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  (setq tempel-path "~/.config/emacs/templates")
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode))

(setq bookmark-save-flag 1)

(defun crj/turn-off-visual-line-mode ()
  (visual-line-mode -1))

(use-package proced
  :init
  (add-hook 'proced-mode-hook #'crj/turn-off-visual-line-mode)
  (map! :leader
        (:prefix ("o" . "+open")
                 :desc "Proced" :n "p" #'proced))
  :config
  (define-key proced-mode-map (kbd "g z") #'proced-toggle-auto-update))

(add-to-list 'tramp-default-proxies-alist '(".*" "\\`root\\'" "/ssh:%h:"))

;; Some helpful utilities.

;; Taken from https://emacs.stackexchange.com/a/62238
;; TODO add completing-read
(defun crj/get-mode-inheritance (mode)
  "Gets a list of all modes a mode derives from."
  (interactive (list major-mode))
  (defun iter (mode)
    (and mode
         (cons mode
               (iter (get mode 'derived-mode-parent)))))
  (message "%s" (iter mode)))

(defun crj/cycle-setting (setting potential-values)
  "Cycle SETTING through POTENTIAL-VALUES.

SETTING is a quoted symbol.

POTENTIAL-VALUES is a list of values to cycle through."
  (let ((i (cl-position (eval setting) potential-values)))
    (set setting (if (eq (1+ i) (length potential-values))
                     (car potential-values)
                   (nth (1+ i) potential-values)))))

(defun crj/cycle-setting-reverse (setting potential-values)
  "Cycle SETTING backwards through POTENTIAL-VALUES.

TODO currently broken!

SETTING is a quoted symbol.

POTENTIAL-VALUES is a list of values to cycle through.

Backwards in this case means that we move towards the first element in the list,
cycling back to the last element if we wrap around."
  (let ((i (cl-position (eval setting) potential-values)))
    (set setting (if (zerop i)
                     (car (last potential-values))
                   (nth (1- i) potential-values)))))

(defun crj/toggle-boolean-setting (&rest booleans)
  "Toggle BOOLEANS between t and nil.

Each BOOLEAN must be a quoted symbol.

Like `setq', this function may be used on multiple symbols simultaneously.

Unlike `setq', they must be quoted."
  (dolist (boolean booleans)
    (set boolean (not (eval boolean)))))

(map! :leader
      (:prefix ("o" . "+open")
       :desc "Visualize undo tree."
       :n "u" #'vundo))

;;; Better window management.
;; Reverse the shortcuts between window splitting with follow vs. without.
;; This is because I'm a lot more likely to want to do something with the new split immediately than later.
;; Also, use the hydra as a better UI for window management.
;; Lastly, make window movement wrap around.
(map! :leader
      (:prefix "w"
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

;; TODO figure out how to use leader keys with built-in keybinding functions
;; (global-set-key (kbd "SPC h S-g") #'customize-group)
(map! :leader
      (:prefix ("h" . "+help")
       :desc "Customize group." :n "G" #'customize-group))

;; Start with lisp!
(setq initial-major-mode 'emacs-lisp-mode)

;; network interface
(require 'nm)
(map! :leader
      (:prefix ("o" . "+open")
               (:prefix ("n" . "+network")
                :desc "Connect to network." :n "c" 'nm/connect-with-profile
                :desc "Connect to new network." :n "C" 'nm/connect-basic
                :desc "Check network status." :n "s" #'nm/show-wifi-status)))

;; Rotate the symbol at point.
(use-package! parrot
  :config
  (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
  (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
  (setq parrot-rotate-dict (append parrot-rotate-dict '((:rot ("const" "let"))))))

(defun crj/flatten-list (list)
  "A function to flatten the given LIST.
Taken from https://stackoverflow.com/a/13173391.

With a minor bug fix of adding `cl-loop' in place of `loop'"
  (cl-loop for e in list
           nconc
           (if (consp e)
               (cl-copy-list e)
             (list e))))

(defun crj/parrot-extra--get-symbols-from-rotation-list (list)
  "Returns just the symbols from the rotation list LIST."
  (plist-get list :rot))

(defun crj/parrot-extra--get-parrot-symbols ()
  "This function returns every symbol in the `parrot-rotate-dict'"
  (crj/flatten-list
   (mapcar #'crj/parrot-extra--get-symbols-from-rotation-list parrot-rotate-dict)))

(defun crj/parrot-extra--parrot-symbol-p (symbol)
  "Returns `t' if SYMBOL is in `parrot-rotate-dict'."
  (member symbol (crj/parrot-extra--get-parrot-symbols)))

;; tests for the above
(crj/parrot-extra--parrot-symbol-p "colin")
(crj/parrot-extra--parrot-symbol-p "const")

;; initialize the targets package
(targets-setup t)

;; operator that replaces a motion/text object with what's in a register (the " register by default).
(map! :map evil-normal-state-map :leader :desc "Replace with register" "r" #'evil-replace-with-register)

;; Evil settings.
(setq evil-ex-substitute-global 1)
(setq evil-want-minibuffer nil)
(setq +evil-want-o/O-to-continue-comments nil)

;; get rid of prompts
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq confirm-kill-emacs nil)

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
       :desc "toggle ligatures in buffer" :n "l" #'org-toggle-link-display
       :desc "toggle prettier globally" :n "p" #'global-prettier-mode
       :desc "toggle transparency" :n "t" #'toggle-transparency))

;; start every emacs frame with transparency
;; (add-hook 'emacs-startup-hook 'toggle-transparency)

(use-package! pulsar
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay .01)
  (setq pulsar-iterations 30)
  (setq pulsar-face 'pulsar-generic)
  :config
  (dolist (function '(evil-scroll-up
                      evil-scroll-down
                      evil-goto-line
                      evil-beginend-prog-mode-goto-beginning
                      evil-beginend-prog-mode-goto-end
                      evil-beginend-org-mode-goto-beginning
                      evil-beginend-org-mode-goto-end
                      evil-beginend-dired-mode-goto-beginning
                      evil-beginend-dired-mode-goto-end
                      evil-beginend-message-mode-goto-beginning
                      evil-beginend-message-mode-goto-end
                      evil-beginend-org-agenda-mode-goto-beginning
                      evil-beginend-org-agenda-mode-goto-end
                      evil-beginend-compilation-mode-goto-beginning
                      evil-beginend-compilation-mode-goto-end
                      evil-beginend-magit-status-mode-goto-beginning
                      evil-beginend-magit-status-mode-goto-end
                      evil-beginend-magit-revision-mode-goto-beginning
                      evil-beginend-magit-revision-mode-goto-end
                      evil-goto-first-line
                      evil-goto-mark-line
                      evil-scroll-page-up
                      evil-scroll-page-down
                      +evil/window-move-up
                      bury-buffer
                      kill-buffer
                      doom/window-enlargen
                      crj/toggle-presentation-mode
                      doom
                      +evil/window-move-down
                      +evil/window-move-left
                      +evil/window-move-right
                      delete-other-windows
                      winner-undo
                      evil-window-up
                      evil-window-down
                      evil-window-left
                      evil-window-right
                      evil-window-new
                      evil-window-vnew))
  (add-to-list 'pulsar-pulse-functions function))

  (pulsar-global-mode 1)
  (map! :leader (:prefix ("v" . "+view")
                          :desc "Pulse current line." :n "l" #'pulsar-pulse-line
                          :desc "Highlight current line." :n "L" #'pulsar-highlight-line)))

(use-package! dimmer
  :config
  (dimmer-configure-org)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.2)
  (dimmer-mode t))

;; Indentation

;; Tabs should be 2 spaces by default.
(setq! indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq! tab-width 2)
(setq! evil-shift-width 2)
(setq standard-indent 2)
(setq! tab-stop-list (number-sequence 2 120 2))
(dtrt-indent-global-mode)

;; use indent of 2 for html
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Stop clobbering my system clipboard, emacs you fiend.
(setq save-interprogram-paste-before-kill t)

;; Not currently using this, could be useful again though.
(defun crj/swap-chars ()
  (interactive)
  (when (evil-normal-state-p)
    (transpose-chars 1)
    (backward-char)))

(use-package! emacs
  :bind
  (:map evil-insert-state-map
  ("C-S-k" . kill-line)))

(defun crj/evil-tranpose-chars ()
  "Transpose characters as one vim-style action.

Wraps the function `transpose-chars' so that it's more in the style of Evil
Mode/Vim. (See info node `(evil)Overview')

- Acts on the current character and the one to the right, which is more in line
with Vim's Normal Mode style.
- Adds the entire process as one action, adding undo/repeat ability.

This differs greatly from the more Emacs-like `transpose-chars', which allows
you to drag a character forward as far as you want, using a count, but this
author found that he preferred the atomicity of Normal Mode.

TODO - add ability to drag forward with count.

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

;; Theme Settings
;;; Modus
(use-package emacs
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-syntax '(alt-syntax yellow-comments green-strings)
        modus-themes-paren-match '(intense underline)
        modus-themes-subtle-line-numbers nil
        modus-themes-deuteranopia t
        modus-themes-markup '(background)
        modus-themes-org-blocks 'gray-background
        modus-themes-region '(no-extend bg-only accented)
        modus-themes-hl-line nil
        modus-themes-headings (quote
                               ((1 . (rainbow 1.8))
                                (2 . (rainbow 1.6))
                                (3 . (rainbow 1.4))
                                (4 . (rainbow 1.2))))
        modus-themes-completions (quote
                                  ((matches . (intense background underline bold))
                                   (selection . (accented intense bold))
                                   (popup . (accented intense bold)))))
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-operandi t)
  (setq evil-insert-state-cursor `((bar . 2) ,(modus-themes-color 'red-intense))
        evil-normal-state-cursor `(box ,(modus-themes-color 'blue-alt)))
  (set-face-attribute 'modus-themes-hl-line nil
                      :extend nil
                      :background 'unspecified))
;;; doom-zenburn
;; (setq doom-zenburn-comment-bg t)
;; (setq doom-zenburn-brighter-comments t)
;; (setq doom-zenburn-brighter-modeline t)

;;; zenburn
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)

;;; set fave themes
; (setq crj/working-theme-daytime 'modus-operandi)
; (setq crj/presentation-theme-daytime 'modus-operandi)
; (setq crj/working-theme-nighttime 'modus-vivendi)
; (setq crj/presentation-theme-nighttime 'modus-operandi)

; (setq crj/daytime-p t)
; (setq crj/presentation-mode-p nil)

;; (defun crj/get-current-theme ()
;;   "Get current theme, depending on time of day and presentation mode."
;;   (cond ((and crj/daytime-p crj/presentation-mode-p)
;;          crj/presentation-theme-daytime)
;;         ((and crj/daytime-p (not crj/presentation-mode-p))
;;          crj/working-theme-daytime)
;;         ((and (not crj/daytime-p) crj/presentation-mode-p)
;;          crj/presentation-theme-nighttime)
;;         ((and (not crj/daytime-p) (not crj/presentation-mode-p))
;;          crj/working-theme-nighttime)))

;; (defun crj/switch-to-appropriate-theme ()
;;   "Switch to the theme appropriate to the time of day and presentation mode."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (let ((new-theme (crj/get-current-theme)))
;;     (load-theme new-theme t)))

;; (crj/switch-to-appropriate-theme)

;; (set-face-attribute 'highlight nil :background "#b5d0ff")

;; Constants for crj/toggle-presentation-mode
;; (setq crj/working-mode-line-height 160)
;; (setq crj/presentation-mode-line-height 320)
(setq crj/working-line-number-type 'relative)
(setq crj/presentation-line-number-type t)

;; (defun crj/toggle-presentation-mode ()
;;   "Toggles between presenting code and working with code.

;; It toggles:

;; - theme (ensuring we use a light theme),
;; - line number type,
;; - and text size, both in regular buffer and the mode line."

;;   (interactive)
;;   (if crj/presentation-mode-p
;;       (progn
;;         (setq
;;          crj/presentation-mode-p nil
;;          display-line-numbers-type crj/working-line-number-type)
;;         (global-display-line-numbers-mode 1)
;;         (global-hl-line-mode -1)
;;         (global-auto-composition-mode 1)
;;         (set-face-attribute 'mode-line nil
;;                             :height crj/working-mode-line-height))
;;     (setq
;;      crj/presentation-mode-p t
;;      display-line-numbers-type crj/presentation-line-number-type)
;;     (global-display-line-numbers-mode 1)
;;     (global-hl-line-mode)
;;     (global-auto-composition-mode -1)
;;     (set-face-attribute 'mode-line nil
;;                         :height crj/presentation-mode-line-height))
;;   (crj/switch-to-appropriate-theme)
;;   (doom-big-font-mode))

;; ;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-stuff")
(setq debug-ignored-errors '("^Exit the snippet first!$" "^End of line$" "^Beginning of line$" beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive))
                                        ; line number settings

;; always display line numbers
(global-display-line-numbers-mode t)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type crj/working-line-number-type)
;; soft wrap lines
(global-visual-line-mode 1)

(add-hook 'after-change-major-mode-hook #'global-visual-line-mode)

(defun crj/toggle-theme-for-time-of-day ()
  (interactive)
  (setq crj/daytime-p (not crj/daytime-p))
  (crj/switch-to-appropriate-theme)
  (crj/make-custom-face-adjustments))

(global-hl-line-mode -1)
;; Best way to remove global-hl-line-mode in Doom.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Search specific engines.
(engine-mode t)

(defengine duck-duck-go
  "https://duckduckgo.com/?q=%s")
(defengine google
  "https://www.google.com/search?q=%s")
(defengine google-images
  "https://www.google.com/search?tbm=isch&q=%s")

(map!
 :leader
 (:prefix "s"
  :desc "Search DuckDuckGo" :n "h" #'engine/search-duck-duck-go
  (:prefix "g"
   :desc "Search Google" :n "g" #'engine/search-google
   :desc "Search Google Images" :n "i" #'engine/search-google-images)))

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

;; formatting

;; Decided to remove format-on-save.
;; Uncomment the below to bring it back.
;; (add-hook 'after-init-hook #'global-prettier-mode)

;; Manually format JS/HTML/MD instead!
(map! :leader
      (:prefix "b"
       :desc "Prettify current buffer."
       :n "p" #'prettier-prettify))

(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; black integration
(setq blacken-only-if-project-is-blackened t)

(setq lsp-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'")


;; Pick from kill ring... with completion!
(map! "M-p" #'consult-yank-from-kill-ring)

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
  ;; (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  ;; (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  ;; (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom)
  (scroll-on-jump-with-scroll-advice-add evil-goto-line))

(with-eval-after-load 'goto-chg
  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

(global-set-key (kbd "<C-M-next>") (scroll-on-jump-interactive 'diff-hl-next-hunk))
(global-set-key (kbd "<C-M-prior>") (scroll-on-jump-interactive 'diff-hl-previous-hunk))

(setq scroll-on-jump-duration 1)

(setq eradio-channels '(("SomaFM - Fluid" . "https://somafm.com/fluid.pls")
                        ("DropSignal - Chillstep" . "http://188.165.152.45:8000/listen.pls?sid=1&t=.pls")
                        ("LO FLY Radio" . "http://64.20.39.8:8421/listen.pls?sid=1&t=.pls")
                        ("Ethereal Radio" . "http://us4.internet-radio.com:8073/live.m3u")
                        ("test" . "	http://streams.fluxfm.de/Chillhop/mp3-320")
                        ("SomaFM - Mission Control" . "https://somafm.com/missioncontrol.pls")
                        ("SomaFM - Cliqhop IDM" . "https://somafm.com/cliqhop.pls")
                        ("SomaFM - Beat Blender" . "https://somafm.com/beatblender.pls")
                        ("SomaFM - Groove Salad." . "https://somafm.com/groovesalad.pls")
                        ("SomaFM - Secret Agent" . "https://somafm.com/secretagent.pls")
                        ("SomaFM - DEF CON" . "https://somafm.com/defcon.pls")
                        ("SomaFM - Space Station Soma" . "https://somafm.com/spacestation.pls")
                        ("SomaFM - Deep Space One" . "https://somafm.com/deepspaceone.pls")
                        ("Chill Lounge Florida" . "https://www.internet-radio.com/servers/tools/playlistgenerator/?u=http://us5.internet-radio.com:8283/listen.pls&t=.pls")))


;; Doom Modeline settings.
(remove-hook 'doom-modeline-mode-hook 'column-number-mode)
(remove-hook 'doom-modeline-mode-hook 'size-indication-mode)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(setq mode-line-percent-position nil)
(line-number-mode 0)

;; Turn the modeline on and off.
(defun crj/toggle-mode-line-buffer ()
  (interactive)
  (hide-mode-line-mode 'toggle)
  (redraw-display))

(defun crj/toggle-mode-line-global ()
  (interactive)
  (if global-hide-mode-line-mode
      (global-hide-mode-line-mode 0)
    (global-hide-mode-line-mode))
  (redraw-display))

(defun crj/toggle-presentation-mode ()
  (interactive)
  (doom-big-font-mode 'toggle)
  (crj/cycle-setting 'display-line-numbers '(relative t)))

(defun crj/cycle-line-numbers ()
  (interactive)
  (crj/cycle-setting 'display-line-numbers '(relative t nil)))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "line numbers" :n "l" #'crj/cycle-line-numbers
       :desc "org markers" :n "C" #'crj/org-toggle-character-markers
       :desc "radio" :n "m" #'eradio-stop
       :desc "play radio channel" :n "M" #'eradio-play
       :desc "modeline for buffer" :n "d" #'crj/toggle-mode-line-buffer
       :desc "modeline" :n "D" #'crj/toggle-mode-line-global
       :desc "day/night themes" :n "n" #'crj/toggle-theme-for-time-of-day
       :desc "code presentation" :n "p" #'crj/toggle-presentation-mode
       :desc "org tree slide mode" :n "P" #'org-tree-slide-mode
       :desc "pomodoro clock" :n "c" #'org-pomodoro))

;; Indium.
(use-package! indium
  :init
  (setq indium-chrome-executable "google-chrome-stable"))

;; Ibuffer commands.
(map! :leader
      (:prefix "v"
       :desc "ibuffer filter by content" :n "u" #'ibuffer-update
       :desc "ibuffer filter by content" :n "/" #'ibuffer-filter-by-content
       :desc "ibuffer filter by mode" :n "m" #'ibuffer-filter-by-mode
       :desc "remove ibuffer filter" :n "?" #'ibuffer-filter-disable))

(use-package jest-test-mode
  :commands jest-test-mode
  :hook (rjsx-mode typescript-mode js-mode typescript-tsx-mode)
  :config
  (add-to-list 'jest-test-options "--watch-all")
  (map!
   :map jest-test-mode-map
   (:prefix
    "C-c"
    (:prefix ("t" . "test")
     :desc "Run all project tests." "p" #'jest-test-run-all-tests
     :desc "Run current test file." "n" #'jest-test-run
     :desc "Re-run last test." "a" #'jest-test-rerun-test
     :desc "Run test at point." "t" #'jest-test-run-at-point))))

;; i3wm mode.
(add-hook! 'i3wm-config-mode-hook #'rainbow-mode)

;; Show digraphs.
(map! :n "SPC h D" #'evil-ex-show-digraphs)

(use-package! org-pomodoro
  :init
  (dolist (player-args-setting '(org-pomodoro-start-sound-args
                                 org-pomodoro-killed-sound-args
                                 org-pomodoro-ticking-sound-args
                                 org-pomodoro-finished-sound-args
                                 org-pomodoro-overtime-sound-args
                                 org-pomodoro-long-break-sound-args
                                 org-pomodoro-short-break-sound-args))
    (set player-args-setting "volume=15000"))

  (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-format "P%s")
  (setq org-pomodoro-time-format "%m")
  (setq org-pomodoro-long-break-format "L~%s")
  (setq org-pomodoro-short-break-format "S~%s"))

;; set up exec-path-from-shell
(when (or (memq window-system '(mac ns x)) (daemonp))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; open links through ace-link
(define-key help-mode-map (kbd "M-o") #'ace-link-help)
;; (define-key compilation-mode-map (kbd "M-o") #'ace-link-compilation)
(map! :map org-mode-map :n (kbd "M-o") #'ace-link-org)

;; Switch frames. (Particularly useful on macOS.)
(map! :leader
      (:prefix "w"
       :n "f" #'other-frame))

;; use subwords always
;; makes 'w' work with the subwords of a camelCase word
;; use 'W' for the whole thing
;; or the text object 'o' for a symbol
(global-subword-mode)
;; weather config
(setq wttrin-default-cities '("Manhattan" ))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
(map! :leader
      (:prefix "o"
       :n "w" #'wttrin))

(fset 'convert-react-class-to-functional-component
      (kmacro-lambda-form [?g ?g ?/ ?c ?l ?a ?s ?s return ?c ?i ?w ?c ?o ?n ?s ?t escape ?2 ?W ?c ?2 ?w ?= ?  ?\( ?\) ?  ?= ?> escape ?/ ?r ?e ?n ?d ?e ?r return ?$ ?% ?d ?d ?N ?d ?d] 0 "%d"))

;; Auto save the Messages buffer too.
(defun save-messages-buffer ()
  (with-current-buffer (get-buffer "*Messages*")
    (append-to-file nil nil "~/.messages-history.txt")))

(add-hook! 'auto-save-hook #'save-messages-buffer)

;; Switch to the messages buffer.
(defun crj/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*Messages*")))

(map! :leader
      (:prefix "o"
       :desc "Switch to messages buffer."
       :n "M" #'crj/switch-to-messages-buffer))

;; Fixes a bug in RJSX Mode where it doesn't handle the fragment syntax.
(defun crj/rjsx-electric-gt-fragment-a (n)
  (if (or (/= n 1) (not (and (eq (char-before) ?<) (eq (char-after) ?/)))) 't
    (insert ?> ?<)
    (backward-char)))

(advice-add #'rjsx-electric-gt :before-while #'crj/rjsx-electric-gt-fragment-a)

;; Opening very large files.
(require 'vlf-setup)
(setq vlf-application 'dont-ask)

(map! :map Info-mode-map :n "q" nil)

(setq org-babel-header-args:sql-mode '((:product . :postgres) (:session . :any))
      org-babel-default-header-args:sql-mode '((:product . "postgres"))
      sql-connection-alist '(("animes" (sql-product 'postgres) (sql-user "abbreviatedman") (sql-database "animes_dev") (sql-server ""))))
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

;; Browse with EWW. (& to open in default browser after.)
(defun crj/browse-with-eww-in-same-window (url &rest _args)
  "Function to browse eww in the same window.

Probably something like this already exists!"
  (eww-browse-url url t))

;; password management
(setq auth-sources '(password-store))
(setq browse-url-browser-function 'browse-url-default-browser)


;; Atomic Chrome

(defun crj/remove-some-html ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "</[^/>]*[^/]>" (point-max) t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<p>" (point-max) t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "<br>" (point-max) t)
    (replace-match "")))

(defun crj/fix-ghost-text-buffer ()
  (interactive)
  (goto-char (point-min))
  (insert orig-text)
  (crj/remove-some-html)
  (goto-char (point-min))
  (kill-line))

(defun crj/set-up-ghost-text-buffer (orig-fun &rest args)
  "Sanitizes text from Atomic Chrome.

Added as advice below. So... careful!"
  (let* ((orig-text (nth 3 args))
         (new-text (with-temp-buffer
                     (crj/fix-ghost-text-buffer)
                     (buffer-string)))
         (list (remove orig-text args)))
    (apply orig-fun (add-to-list 'list new-text t))))

(use-package! hungry-delete
  :init
  (setq hungry-delete-join-reluctantly t)
  :config
  (map! :i "S-<backspace>" #'hungry-delete-backward)
  (map! :i "S-<delete>" #'hungry-delete-forward))

(defun crj/kill-all-text-in-buffer ()
  (interactive)
  (evil-delete (point-min) (point-max)))

(use-package! atomic-chrome
  :init
  (atomic-chrome-start-server)
  (setq atomic-chrome-default-major-mode 'gfm-mode)
  (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (advice-add 'atomic-chrome-create-buffer :around #'crj/set-up-ghost-text-buffer)
  (map!
   :map atomic-chrome-edit-mode-map
   :leader
   (:prefix "c"
    :desc "Exit Ghost Text buffer."
    :n "z" #'atomic-chrome-close-current-buffer
    :desc "Fix up Ghost Text buffer."
    :n "p" #'crj/fix-ghost-text-buffer)))

;;; atomic chrome converts some text boxes to html, and we want to convert to markdown
;;; this function, or its individual pieces, can help
(defun crj/remove-html-from-markdown-and-clean-up ()
  (interactive)
  (html-to-markdown-this-buffer)
  (prettier-prettify))

;; taken from https://emacs.stackexchange.com/questions/18504/gnus-how-to-strip-all-html-tags-from-incoming-mails, which says it's from Sacha Chua (but the link doesn't work, so who knows)
;; Good function to learn from, though doesn't quite do what I want.
(defun sc/strip-html ()
  "Remove HTML tags from the current buffer,
   (this will affect the whole buffer regardless of the restrictions in effect)."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "<[^<]*>" (point-max) t)
        (replace-match "\\1"))
      (goto-char (point-min))
      (replace-string "&copy;" "(c)")
      (goto-char (point-min))
      (replace-string "&amp;" "&")
      (goto-char (point-min))
      (replace-string "&lt;" "<")
      (goto-char (point-min))
      (replace-string "&gt;" ">")
      (goto-char (point-min)))))

(use-package beginend
  :config
  (beginend-global-mode))

;; Load my (many) config files. But not the flycheck ones.
;; Not sure I /needed/ to avoid loading those.
;; But I /did/ want to practice my lisp-fu!

;; Utility function.

(defun crj--not-a-flycheck-filename-p (filename)
  (not (string-search "flycheck" filename)))

;; Forked repos.

;; My modules. Should probably combine these two snippets..
(let* ((unsanitized-modules
        (file-expand-wildcards "~/.doom.d/crj-modules/*.el"))
       (modules
        (seq-filter #'crj--not-a-flycheck-filename-p unsanitized-modules)))
  (mapc 'load modules))

(let* ((unsanitized-modules
        (file-expand-wildcards "~/.doom.d/external-modules/*.el"))
       (modules
        (seq-filter #'crj--not-a-flycheck-filename-p unsanitized-modules)))
  (mapc 'load modules))


;; some available keybinding prefixes
;; SPC l
;; SPC and any capital letter
