(eval-after-load 'ox-html
  '(setq org-html-special-string-regexps nil))

;; org export settings
(after! org
  (setq org-html-postamble nil
        org-export-with-toc nil)
  (setq org-babel-default-header-args `((:results . "verbatim pp replace output")
                                        (:exports . "both")
                                        (:noweb . "strip-export")
                                        (:session . "none")
                                        (:eval . "no-export"))))

;; Quick and dirty CSS.
;; TODO add to it, maybe make it a global file or host on a CDN.
(setq org-html-head-extra
      "<style>.example::before {content: \"Results:\"; display: block; margin-bottom: 1em;}</style>")
