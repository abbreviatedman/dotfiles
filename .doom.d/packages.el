;; adds browsing for links
(package! browse-at-remote)

(package! json-process-client
  :recipe (:host github
           :repo "emacsmirror/json-process-client"))

(package! enwc)

;; send search to browser
(package! engine-mode)

;; internal ssh handling
(package! ssh-agency)

;; adds gcal integration
(package! org-gcal)

(package! emacs-nm
  :recipe
  (:host github
   :repo "abbreviatedman/emacs-nm"
   :branch "main"))

;; decent emoji support
(package! emojify)

;; better evil text objects
(package! targets
  :recipe (
           :host github
           :repo "noctuid/targets.el"))

;; Sync a buffer with a Chrome text box.
(package! atomic-chrome
  :recipe (
           :host github
           :repo "alpha22jp/atomic-chrome"))

;; convert html to markdown (atomic chrome gives you html unfortunately)
(package! html-to-markdown)

;; Wrapper for shell commands.
(package! dwim-shell-command)

;; Rotating thing at point.
(package! parrot)

;; cursor highlighting when scrolling or switching buffers
(package! beacon)

; Themes

;; Non-DOOM Solarized themes.
(package! solarized-theme)

;; MORE Solarized themes??
(package! color-theme-solarized)
(package! color-theme-sanityinc-solarized)

;; Modus themes
(package! modus-themes
  :recipe (:host nil
           :type git
           :repo "https://git.sr.ht/~protesilaos/modus-themes"))

;; Zenburn theme
(package! zenburn-theme)

;; pomodoro
(package! org-pomodoro)

;; company improvements
(package! company-posframe :disable t)
(package! company-box :disable t)
(package! company :disable t)

;; Smooth scrolling.
(package! scroll-on-jump
  :recipe (:host gitlab :repo "ideasman42/emacs-scroll-on-jump"))

;; ligatures
(package! fira-code-mode)

;; streaming radio
(package! eradio)

;; Org exporters.
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! ox-slack
  :recipe
  (:host github
   :repo "masukomi/ox-slack"
   :branch "community"))

(package! ox-reveal)

; automatic code reformatting

;; web development
(package! prettier)

;; python
(package! blacken)

;; JS debugging
(package! indium)

(package! peep-dired)
;; Nice replace speed.
(package! evil-replace-with-register)
(package! i3wm-config-mode)
(package! exec-path-from-shell)
(package! ace-link)

(package! org-trello)

(package! jest)

;; Weather
;; TODO needs patching!
(package! wttrin)

(package! mmm-mode)

;; case changing!
(package! string-inflection)

;; Better completion.
(package! corfu)
(package! cape)
(package! consult-yasnippet)

;; Slack
(package! request)
(package! oauth2)
(package! circe)
(package! emacs-slack
  :recipe (:host github :repo "stonekyx/emacs-slack"))

;; Don't use Doom's snippets:
;; (package! doom-snippets :ignore t)
(package! yasnippet)

;; Open large files.
(package! vlf)

;; GraphQL
(package! graphql-mode)

;; different font styles, same file
(package! mixed-pitch)

;; Elisp tree navigation system.
(package! symex)

(package! pcmpl-args)

(package! svg-lib)

;; SQL
(package! ob-sql-mode)
(package! sqlup-mode)

;; SSH
(package! keychain-environment)

;; Github Cloning
(package! github-clone)

;; Mail
(package! org-mime)

;; Better file manager.
(package! dirvish)

;; Visual regexp.
(package! visual-regexp)
;;; Allows for regexes that aren't escaped for Emacs.
(package! visual-regexp-steroids)

;; Undo

;;; Persist undo across restarts.
(package! undo-fu-session)

;;; Visualize undo.
(package! vundo)
