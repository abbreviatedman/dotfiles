;; Space Liner
;; Functions for moving the current line or visual selection up or down.

;; Normal Mode Versions

;; move line up by COUNT lines, bringing point
(defun space-liner-bring-line-up (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or (* n -1) -1))
    (yank)
    (forward-line -1)))

;; move line down by COUNT lines, bringing point
(defun space-liner-bring-line-down (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or n 1))
    (yank)
    (forward-line -1)))



;; Visual Mode Versions

;; Moves visual selection down by count lines, bringing point.
;; Currently is slightly off-kilter for non-Visual-Block visual modes.
(defun space-liner-bring-line-down-visual-mode (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or n 1))
    (yank))
  (evil-visual-line)
  (forward-line -1))

;; Moves visual selection up by count lines, bringing point.
;; Currently is slightly off-kilter for non-Visual-Block visual modes.
(defun space-liner-bring-line-up-visual-mode (&optional n)
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or (* n -1) -1))
    (yank))
  (evil-visual-line)
  (forward-line -1)
  )





(map! :map evil-normal-state-map "SPC k" #'space-liner-bring-line-up)
(map! :map evil-normal-state-map "SPC j" #'space-liner-bring-line-down)
(map! :map evil-visual-state-map "SPC k" #'space-liner-bring-line-up-visual-mode)
(map! :map evil-visual-state-map "SPC j" #'space-liner-bring-line-down-visual-mode)
