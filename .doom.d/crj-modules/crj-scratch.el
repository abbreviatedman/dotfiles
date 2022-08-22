(defun crj/open-scratch-buffer (&optional new-buffer-p project-p same-window-p)
  "Open a scratch buffer with the current buffer's mode applied.

Passes on arguments to `doom/open-scratch-buffer'. See that function's doc string for the arguments used.

Setting `doom-scratch-initial-major-mode' to `t' was supposed to handle the \"use the same mode\" part, but it does not appear to be working. So: hack."
  (interactive)
  (let ((previous-mode major-mode)
        (read-only-p buffer-read-only))
    (doom/open-scratch-buffer new-buffer-p project-p same-window-p)
    (unless read-only-p
      (funcall previous-mode))))

(defun crj/open-scratch-buffer-in-new-window ()
  "Open the scratch buffer in the same window with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer nil nil t))

;; Currently unused, but... here's how I'd do it if I brought it back.
(defun crj/open-new-scratch-buffer ()
  "Open a new scratch buffer with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer t))

(defun crj/open-project-scratch-buffer ()
  "Open a project-specific scratch buffer with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer nil t))

(defun crj/open-project-scratch-buffer-in-new-window ()
  "Open a project-specific scratch buffer in a new window with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer nil t t))

(map! :leader
      "x" #'crj/open-scratch-buffer
      "X" #'crj/open-scratch-buffer-in-new-window
      (:prefix "p"
       :n "x" #'crj/open-project-scratch-buffer
       :n "X" #'crj/open-project-scratch-buffer-in-new-window)
      (:prefix "o"
       :n "x" #'crj/open-scratch-buffer
       :n "X" #'crj/open-scratch-buffer-in-new-window))

(setq doom-scratch-dir "~/Documents/work/playground/")
