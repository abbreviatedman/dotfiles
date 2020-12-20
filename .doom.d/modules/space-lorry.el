;; Functions for jumping current line or visual selection up or down.

;; move line up by COUNT lines, bringing point
(defun space-yank-bring-line-up (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or (* n -1) -1))
    (yank)
    (forward-line -1)))

;; move line down by COUNT lines, bringing point
(defun space-yank-bring-line-down (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or n 1))
    (yank)
    (forward-line -1)))



;; visual mode versions

(defun space-jump-down-visual-mode (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or n 1))
    (yank))
  (evil-visual-line)
  (forward-line -1))




(map! :map evil-normal-state-map "SPC k" #'space-yank-bring-line-up)
(map! :map evil-normal-state-map "SPC j" #'space-yank-bring-line-down)
(map! :map evil-visual-state-map "SPC j" #'space-jump-down-visual-mode)
