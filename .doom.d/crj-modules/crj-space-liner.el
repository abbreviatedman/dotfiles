;; Space Liner
;; Functions for moving the current line or visual selection up or down.

;; Normal Mode Versions
(defun space-liner-bring-line-up (&optional n)
"Moves line up by COUNT lines, bringing point."
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or (* n -1) -1))
    (yank)
    (forward-line -1)))

(defun space-liner-bring-line-down (&optional n)
"Moves line down by COUNT lines, bringing point."
  (interactive "p")
  (evil-with-single-undo
    (kill-whole-line)
    (forward-line (or n 1))
    (yank)
    (forward-line -1)))

;; Visual Mode Versions
(defun space-liner-bring-line-down-visual-mode (&optional n)
  "Moves visual selection down by count lines, bringing point.
Currently is slightly off-kilter for non-Visual-Block visual modes."
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or n 1))
    (yank))
  (evil-visual-line)
  (forward-line -1))

(defun space-liner-bring-line-up-visual-mode (&optional n)
  "Moves visual selection up by count lines, bringing point.
Currently is slightly off-kilter for non-Visual-Block visual modes."
  (interactive "p")
  (evil-with-single-undo
    (kill-region evil-visual-beginning evil-visual-end)
    (forward-line (or (* n -1) -1))
    (yank))
  (evil-visual-line)
  (forward-line -1))

(map! :leader :n "k" #'space-liner-bring-line-up)
(map! :leader :n "j" #'space-liner-bring-line-down)
(map! :leader :v "k" #'space-liner-bring-line-up-visual-mode)
(map! :leader :v "j" #'space-liner-bring-line-down-visual-mode)
