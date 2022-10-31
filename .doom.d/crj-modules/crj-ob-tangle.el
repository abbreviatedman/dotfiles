(defun org-babel-tangle-single-block (block-counter &optional only-this-block)
  "Collect the tangled source for current block.
Return the list of block attributes needed by
`org-babel-tangle-collect-blocks'.  When ONLY-THIS-BLOCK is
non-nil, return the full association list to be used by
`org-babel-tangle' directly."
  (let* ((info (org-babel-get-src-block-info))
         (start-line
          (save-restriction (widen)
                            (+ 1 (line-number-at-pos (point)))))
         (file (buffer-file-name (buffer-base-buffer)))
         (src-lang (nth 0 info))
         (params (nth 2 info))
         (extra (nth 3 info))
         (coderef (nth 6 info))
         (cref-regexp (org-src-coderef-regexp coderef))
         (link (org-babel-tangle--unbracketed-link params))
         (source-name
          (or (nth 4 info)
              (format "%s:%d"
                      (or (ignore-errors (nth 4 (org-heading-components)))
                          "No heading")
                      block-counter)))
         (expand-cmd (intern (concat "org-babel-expand-body:" src-lang)))
         (assignments-cmd
          (intern (concat "org-babel-variable-assignments:" src-lang)))
         (body
          ;; Run the tangle-body-hook.
          (let ((body (if (org-babel-noweb-p params :tangle)
                          (if (string= "strip-tangle" (cdr (assq :noweb (nth 2
                                                                             info))))
                              (replace-regexp-in-string (org-babel-noweb-wrap)
                                                        "" (nth 1 info))
                            (org-babel-expand-noweb-references info))
                        (nth 1 info))))
            (with-temp-buffer
              (insert
               ;; Expand body in language specific manner.
               (cond ((assq :no-expand params) body)
                     ((fboundp expand-cmd) (funcall expand-cmd body params))
                     (t
                      (org-babel-expand-body:generic
                       body params (and (fboundp assignments-cmd)
                                        (funcall assignments-cmd params))))))
              (when (string-match "-r" extra)
                (goto-char (point-min))
                (while (re-search-forward cref-regexp nil t)
                  (replace-match "")))
              (run-hooks 'org-babel-tangle-body-hook)
              (buffer-string))))
         (comment
          (when (or (string= "both" (cdr (assq :comments params)))
                    (string= "org" (cdr (assq :comments params))))
            ;; From the previous heading or code-block end
            (funcall
             org-babel-process-comment-text
             (buffer-substring
              (max (condition-case nil
                       (save-excursion
                         (org-back-to-heading t) ; Sets match data
                         (match-end 0))
                     (error (point-min)))
                   (save-excursion
                     (if (re-search-backward
                          org-babel-src-block-regexp nil t)
                         (match-end 0)
                       (point-min))))
              (point)))))
         (src-tfile (cdr (assq :tangle params)))
         (result
          (list start-line
                (if org-babel-tangle-use-relative-file-links
                    (file-relative-name file)
                  file)
                link
                source-name
                params
                (if org-src-preserve-indentation
                    (org-trim body t)
                  (org-trim (org-remove-indentation body)))
                comment)))
    (if only-this-block
        (let* ((file-name (org-babel-effective-tangled-filename
                           (nth 1 result) src-lang src-tfile)))
          (list (cons file-name (list (cons src-lang result)))))
      result)))

(defun org-babel-noweb-p (params context)
  "Check if PARAMS require expansion in CONTEXT.
CONTEXT may be one of :tangle, :export or :eval."
  (let ((allowed-values (cl-case context
                          (:tangle '("yes" "tangle" "no-export" "strip-export"
                                     "strip-tangle"))
                          (:eval   '("yes" "no-export" "strip-export" "eval"
                                     "strip-tangle"))
                          (:export '("yes" "strip-tangle")))))
    (cl-some (lambda (v) (member v allowed-values))
             (split-string (or (cdr (assq :noweb params)) "")))))
