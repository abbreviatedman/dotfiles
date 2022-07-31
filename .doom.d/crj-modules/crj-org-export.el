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
                                        (:eval . "no-export")
                                        (:padline . "no"))))

(defun crj/prettify-js-org-src-block ()
  "Run prettier on source block at point."
  (interactive)
  (org-edit-src-code)
  (prettier-prettify)
  (org-edit-src-exit))

(map! :map org-mode-map
      :leader
      (:prefix "m"
       (:prefix "p"
        :desc "Prettify source block at point."
        :n "j" #'crj/prettify-js-org-src-block)))

;; Quick and dirty CSS.
;; TODO add to it, maybe make it a global file or host on a CDN.
(setq org-html-head-extra
      "<style>.example::before {content: \"Results:\"; display: block; margin-bottom: 1em;}</style>")
