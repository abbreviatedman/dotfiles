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
           :repo "noctuid/targets.el"
        )
)

;; Non-DOOM Solarized themes.
(package! solarized-theme)

;; cursor highlighting when scrolling or switching buffers
(package! beacon)


;; MORE Solarized themes??
(package! color-theme-solarized)
(package! color-theme-sanityinc-solarized)

;; Modus themes
(package! modus-themes)

;; pomodoro
(package! org-pomodoro)

;; company improvements
(package! company-posframe)
(package! company-box)

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

<<<<<<< HEAD
(package! org-trello)
(package! sentence-navigation :disable t)
(package! jest)

;; fix for gitconfig-mode recipe issue
(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
       :files ("gitconfig-mode.el")))
(package! gitignore-mode
	:recipe (:host github :repo "magit/git-modes"
           :files ("gitignore-mode.el")))
=======
(package! jest)
>>>>>>> 680e579c5d1d3a704ab7b152a4fae43aad1a71a1
