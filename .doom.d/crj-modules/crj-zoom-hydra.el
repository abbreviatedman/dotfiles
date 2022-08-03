(defun crj/zoom-in (arg)
  (interactive "p")
  (crj/change-modeline-height (* arg text-scale-mode-step))
  (text-scale-increase arg))

(defun crj/zoom-out (arg)
  (interactive "p")
  (text-scale-increase (* arg -1)))

(defun crj/zoom-reset (arg)
  (interactive "p")
  (crj/change-modeline-height 1 t)
  (text-scale-increase 0))

(defun crj/zoom-in-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-in arg))))

(defun crj/zoom-out-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-out arg))))

(defun crj/zoom-reset-all-buffers (arg)
  (interactive "p")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (crj/zoom-reset arg))))

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
