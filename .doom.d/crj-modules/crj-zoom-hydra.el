(defun crj/zoom-in (arg)
  (interactive "p")
  (text-scale-increase arg)
  (crj/fix-font-wonkiness))

(defun crj/zoom-out (arg)
  (interactive "p")
  (text-scale-increase (* arg -1))
  (crj/fix-font-wonkiness))

(defun crj/zoom-reset (arg)
  (interactive "p")
  (text-scale-increase 0)
  (crj/fix-font-wonkiness))

(defun crj/zoom-in-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-in arg)))
  (crj/fix-font-wonkiness))

(defun crj/zoom-out-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-out arg)))
  (crj/fix-font-wonkiness))

(defun crj/zoom-reset-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-reset arg)))
  (crj/fix-font-wonkiness))

(defhydra crj/hydra/text-zoom (:hint nil :color red)
  "
      Buffer zoom: _j_:zoom in, _k_:zoom out, _b_:reset
      Global zoom: _J_:zoom in, _K_:zoom out, _B_:reset
"
  ("j" crj/zoom-in)
  ("k" crj/zoom-out)
  ("b" crj/zoom-reset)
  ("J" crj/zoom-in-all-buffers)
  ("K" crj/zoom-out-all-buffers)
  ("B" crj/zoom-reset-all-buffers))
