;; adds browsing for links
(package! browse-at-remote)

;; send search to browser
(package! engine-mode)

;; internal ssh handling
(package! ssh-agency)

;; adds gcal integration
(package! org-gcal)

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

;; cursor highlighting when scrolling or switching buffers
(package! beacon)

; Themes

;; Non-DOOM Solarized themes.
(package! solarized-theme)

;; MORE Solarized themes??
(package! color-theme-solarized)
(package! color-theme-sanityinc-solarized)

;; Modus themes
(package! modus-themes)

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
