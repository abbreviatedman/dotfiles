;; crj/beespell-remove-word
;; TODO success/failure messages
;; TODO docstrings
;; TODO deal with non-hunspell dictionaries

(defvar crj/beespell-local-dictionary nil)
(defvar crj/beespell-use-superwords nil)

(defun crj/beespell-remove-word ()
  (interactive)
  (if (not crj/beespell-local-dictionary)
      (message "Variable crj/beespell-local-dictionary not set. Beespell needs a dictionary to remove this word from!")
    (let ((dictionary-buffer (get-file-buffer crj/beespell-local-dictionary))
          (previous-subword-mode-p subword-mode))
      (subword-mode)
      (let ((word (if crj/beespell-use-superwords (thing-at-point 'symbol t) (thing-at-point 'word t))))
        (cond ((not (file-readable-p crj/beespell-local-dictionary))
               (message "No dictionary was found to remove from. Check that the variable crj/beespell-local-dictionary was set to the file path of an existing file."))
              (dictionary-buffer ; The dictionary was a valid file and is currently open.
               (with-current-buffer dictionary-buffer
                 (let ((result-message (crj/beespell--find-and-delete-word-in-whole-buffer word)))
                   (message result-message))))
              ((not dictionary-buffer) ; The dictionary was a valid file and is /not/ currently open.
               (save-window-excursion
                 ;; the point dance below is because the point appears to move in a closed buffer using the commands in cr/beespell--find-and-delete-word-in-whole-buffer. Possibly there are commands that will maintain the point in a closed buffer? Let me know if you know of any!
                 (let ((old-point (point)))
                   (find-file crj/beespell-local-dictionary)
                   (goto-char (point-min))
                   (let (result-message (crj/beespell--find-and-delete-word-forward word))
                     (goto-char old-point)
                     (save-buffer)
                     (kill-buffer)
                     (message result-message)))))))
      (unless previous-subword-mode-p (subword-mode -1))
      (flyspell-buffer))))

(defun crj/beespell--find-and-delete-word-in-whole-buffer (word)
  (let* ((word-search (concat "^" word "$"))
         (match
          (or (re-search-forward word-search nil t)
              (re-search-backward word-search nil t))))
    (if (not match)
        "Beespell tried to remove that word, but it wasn't in the dictionary."
      (kill-whole-line)
      "Beespell removed the word from your dictionary.")))

(defun crj/beespell--find-and-delete-word-forward (word)
  (let* ((word-search (concat "^" word "$"))
         (match
          (re-search-forward word-search nil t)))
    (if (not match)
        "Beespell tried to remove that word, but it wasn't in the dictionary."
      (kill-whole-line)
      "Beespell removed the word from your dictionary.")))
