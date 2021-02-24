;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
                                        ;

;; adds browsing for links
(package! browse-at-remote)

;; internal ssh handling
(package! ssh-agency)

;; adds gcal integration
(package! org-gcal)

;; allows for automatic actions on todos
(package! org-edna)

;; decent emoji support
(package! emojify)

;; use a vim-like undo tree
(package! undo-tree)

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

(package! fira-code-mode)

(package! eradio)

(package! ox-reveal)

;;
;; automatic code reformatting
;;

;; prettier (web development)
(package! prettier)



(package! indium)
