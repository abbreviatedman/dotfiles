;; adds browsing for links
(package! browse-at-remote)

(package! hungry-delete
  :recipe
  (:host github
   :repo "nflath/hungry-delete"))

(package! json-process-client
  :recipe
  (:host github
   :repo "emacsmirror/json-process-client"))

(package! enwc)

;; send search to browser
(package! engine-mode)

;; show functions you're running with your keypresses
(package! keycast)

;; internal ssh handling
(package! ssh-agency)

(package! pulsar)

;; adds gcal integration
;; (package! org-gcal)

;; (package! emacs-oauth2-auto
;;   :recipe
;;   (:host github
;;    :repo "rhaps0dy/emacs-oauth2-auto"))

(package! emacs-nm
  :recipe
  (:host github
   :repo "abbreviatedman/emacs-nm"
   :branch "main"))

;; decent emoji support
(package! emojify)

;; better evil text objects
(package! targets
  :recipe (:host github
           :repo "noctuid/targets.el"))

;; Sync a buffer with a Chrome text box.
(package! atomic-chrome
  :recipe (:host github
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

;; Many Solarized themes.
(package! solarized-theme)
(package! color-theme-solarized)
(package! color-theme-sanityinc-solarized)

(package! modus-themes)
(package! leuven-theme)
(package! ef-themes)

(package! org-pomodoro)

;; Smooth scrolling.
(package! scroll-on-jump)
;; https://codeberg.org/ideasman42/emacs-scroll-on-jump
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

(package! dimmer)

(package! peep-dired)

;; Nice replace speed.
(package! evil-replace-with-register)

(package! i3wm-config-mode)
(package! exec-path-from-shell)
(package! ace-link)

(package! jest-test-mode)

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

;; Jupyter Notebooks
(package! ein)

(unpin! evil-collection)
(package! evil-collection
  :recipe (:repo "emacs-evil/evil-collection"))
;; Nice package for redefining beginnings and ends of some buffers as the
;; IMPORTANT beginnings and ends.
(package! beginend)

;; Slack
;; (package! request)
;; (package! oauth2)
;; (package! circe)
;; (package! emacs-slack
;;   :recipe (:host github :repo "stonekyx/emacs-slack"))

;; Don't use Doom's snippets:
;; (package! doom-snippets :ignore t)
(package! yasnippet)

;; Open large files.
(package! vlf)

;; GraphQL
(package! graphql-mode)

(package! solaire-mode :disable t)
;; different font styles, same file
(package! mixed-pitch)

(package! org-caldav)

(package! pcmpl-args)

(package! svg-lib)

(package! code-review
  :pin "182c1cc6ebced5c161aa37d01142dfab7eb8e7b3"
    :recipe
    (:host github
     :repo "wandersoncferreira/code-review"))

(package! on-parens)

;; Racket
(package! ob-racket
  :recipe (:host github :repo "DEADB17/ob-racket"))

(package! racket-mode)

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

;; Font management
(package! fontaine)
