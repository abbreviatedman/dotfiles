;; custom snipe-and-transpose functions
(defun evil-snipe-back-and-transpose (chars)
  (interactive "M")
  (evil-with-single-undo
    (evil-snipe-S 1 chars)
    (evil-forward-char)
    (transpose-chars 1)
    (evil-backward-char 2))
  (remove-overlays))

(defun evil-snipe-and-transpose (chars)
  (interactive "M")
  (evil-with-single-undo
    (evil-snipe-s 1 chars)
    (evil-forward-char)
    (transpose-chars 1)
    (evil-backward-char 2))
  (remove-overlays))

;; These mappings are backwards of the usual "lowercase goes forward, uppercase goes backwards" vim standards, but I almost always want to transpose backwards, having made a mistake and moved on in the text. You could always change them back!
(map! :map evil-normal-state-map "SPC r" #'evil-snipe-back-and-transpose)
(map! :map evil-normal-state-map "SPC R" #'evil-snipe-and-transpose)
