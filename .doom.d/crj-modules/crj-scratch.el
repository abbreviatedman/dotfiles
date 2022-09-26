(defun crj--get-new-scratch-buffer-mode (previous-mode read-only-p desired-mode)
  (cond (desired-mode desired-mode)
        ((not read-only-p) previous-mode)
        (t 'emacs-lisp-mode)))

(defun crj/open-scratch-buffer (&optional new-buffer-p project-p same-window-p desired-mode)
  "Open a scratch buffer with the current buffer's mode applied.

Passes on arguments to `doom/open-scratch-buffer'. See that function's doc string for the arguments used.

Setting `doom-scratch-initial-major-mode' to `t' was supposed to handle the \"use the same mode\" part, but it does not appear to be working. So: hack."
  (interactive)
  (let ((new-mode (crj--get-new-scratch-buffer-mode major-mode buffer-read-only desired-mode)))
    (doom/open-scratch-buffer new-buffer-p project-p same-window-p)
    (funcall new-mode)))

(defun crj/switch-to-scratch-buffer ()
  "Switch to the scratch buffer with the current mode applied."
  (interactive)
  (crj/open-scratch-buffer nil nil t))

;; Currently unused, but... here's how I'd do it if I brought it back.
(defun crj/open-new-scratch-buffer ()
  "View a new scratch buffer with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer t))

(defun crj/open-project-scratch-buffer ()
  "Open a project-specific scratch buffer with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer nil t))

(defun crj/open-project-scratch-buffer-in-new-window ()
  "View a project-specific scratch buffer with the current buffer's mode applied."
  (interactive)
  (crj/open-scratch-buffer nil t t))

(defun crj/open-scratch-buffer-in-emacs-lisp-mode ()
  "Open a scratch buffer in `emacs-lisp-mode.'"
  (interactive)
  (crj/open-scratch-buffer nil nil nil 'emacs-lisp-mode))

(defun crj/open-scratch-buffer-in-a-js-mode ()
  "Open a scratch buffer in `rjsx-mode"
  (interactive)
  (crj/open-scratch-buffer nil nil nil 'rjsx-mode))

(defun crj/switch-to-scratch-buffer-in-emacs-lisp-mode ()
  "Switch to a scratch buffer in `emacs-lisp-mode'."
  (interactive)
  (crj/open-scratch-buffer nil nil t 'emacs-lisp-mode))

(defun crj/switch-to-scratch-buffer-in-a-js-mode ()
  "Switch to a scratch buffer in a JavaScript Mode

Currently we're using `rjsx-mode'."
  (interactive)
  (crj/open-scratch-buffer nil nil t 'rjsx-mode))

;; Gotta remove doom's built-in ones first.
(define-key doom-leader-map "x" nil)
(define-key doom-leader-map "X" nil)

(map! :leader
      (:prefix ("x" . "open scratch")
       :desc "Open a scratch buffer in current mode." :n "x" #'crj/open-scratch-buffer
       :desc "Open a scratch buffer in Emacs Lisp mode." :n "e" #'crj/open-scratch-buffer-in-emacs-lisp-mode
       :desc "Open a scratch buffer in a JS mode." :n "j" #'crj/open-scratch-buffer-in-a-js-mode)
      (:prefix ("X" . "switch to scratch")
       :desc "Open a scratch buffer in current mode." :n "x" #'crj/switch-to-scratch-buffer
       :desc "Open a scratch buffer in Emacs Lisp mode." :n "e" #'crj/switch-to-scratch-buffer-in-emacs-lisp-mode
       :desc "Open a scratch buffer in a JS mode." :n "j" #'crj/switch-to-scratch-buffer-in-a-js-mode))

(setq doom-scratch-dir "~/Documents/work/playground/")
