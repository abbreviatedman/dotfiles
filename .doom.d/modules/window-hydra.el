(defun hydra-move-split-right-more (arg)
  "Move window split right by a hardcoded higher amount."
  (interactive "p")
  (let ((movement-amount (* arg 10)))
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'left))
        (shrink-window-horizontally movement-amount)
      (enlarge-window-horizontally movement-amount))))

(defun hydra-move-split-left-more (arg)
  "Move window split left by a hardcoded higher amount."
  (interactive "p")
  (let ((movement-amount (* arg 10)))
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally movement-amount)
      (enlarge-window-horizontally movement-amount))))

(defun hydra-move-split-up-more (arg)
  "Move window split up by a hardcoded higher amount."
  (interactive "p")
  (let ((movement-amount (* arg 5)))
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window movement-amount)
      (shrink-window movement-amount))))

(defun hydra-move-split-down-more (arg)
  "Move window split down by a hardcoded higher amount."
  (interactive "p")
  (let ((movement-amount (* arg 5)))
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window movement-amount)
      (enlarge-window movement-amount))))

(defhydra hydra/crj-window-nav (:hint nil)
  "
Split: _v_ert  _s_:horz
Delete: _c_lose  _o_nly
Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
Balance Windows: _=_
Quit: _q_
"

  ("s" +evil/window-split-and-follow)
  ("v" +evil/window-vsplit-and-follow)

  ("c" delete-window)
  ("o" delete-other-windows)

  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" hydra-move-split-left-more)
  ("J" hydra-move-split-down-more)
  ("K" hydra-move-split-up-more)
  ("L" hydra-move-split-right-more)

  ("=" balance-windows)
  ("q" nil))
