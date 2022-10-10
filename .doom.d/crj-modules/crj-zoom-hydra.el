(defun crj/zoom-in (arg)
  (interactive "p")
  ;; (crj/change-modeline-height (* arg text-scale-mode-step))
  (text-scale-increase arg))

(defun crj/zoom-out (arg)
  (interactive "p")
  (text-scale-increase (* arg -1)))

(defun crj/zoom-reset (arg)
  (interactive "p")
  ;; (crj/change-modeline-height 1 t)
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
  ("J" crj/zoom-in)
  ("K" crj/zoom-out)
  ("B" crj/zoom-reset)
  ("j" crj/zoom-in-all-buffers)
  ("k" crj/zoom-out-all-buffers)
  ("b" crj/zoom-reset-all-buffers))

(map! :leader
      (:prefix "z"
       :desc "zoom in" :n "J" #'crj/zoom-in
       :desc "zoom out" :n "K" #'crj/zoom-out
       :desc "reset zoom" :n "B" #'crj/zoom-reset
       :desc "zoom in buffer" :n "j" #'crj/zoom-in-all-buffers
       :desc "zoom out buffer" :n "k" #'crj/zoom-out-all-buffers
       :desc "zoom out buffer" :n "b" #'crj/zoom-reset-all-buffers
       :desc "zoom hydra" :n "z" #'crj/hydra/text-zoom/body
       :desc "toggle link display in org" :n "l" #'org-toggle-link-display
       :desc "toggle prettier globally" :n "p" #'global-prettier-mode
       :desc "toggle transparency" :n "t" #'toggle-transparency))
