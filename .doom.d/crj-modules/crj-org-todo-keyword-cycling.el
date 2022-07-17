(setq crj/todo-cycling-keywords
      '("TODO" "NEXT" "DONE" none))

(defun crj/cycle-todo-keyword (&optional backwards)
  "Applies the next todo keyword to the heading at point by cycling through `crj/todo-cycling-keywords'."
  (let* ((index (crj/get-index-of-todo (crj/get-todo-keyword)))
         (operation (if backwards '- '+))
         (next-index (funcall operation index 1))
         (wrapped-index (crj/get-wrapped-index next-index crj/todo-cycling-keywords)))
    (org-todo (nth wrapped-index crj/todo-cycling-keywords))))

(defun crj/get-todo-keyword ()
  "Returns the todo keyword at point as a string.

TODO - Find a way (probably symbols) to navigate between org's symbols and my own list."
  (format "%s" (org-element-property :todo-keyword (org-element-context))))

(defun crj/cycle-todo-keyword-maybe ()
  "Calls `crj/cycle-todo-keyword' if on a heading. Otherwise, delegates to `+org/dwim-at-point'."
  (interactive)
    (if (org-at-heading-p)
        (crj/cycle-todo-keyword)
      (+org/dwim-at-point)))

(defun crj/cycle-todo-keyword-backwards ()
  "Runs `crj/cycle-todo-keyword', but with the optional argument to reverse the cycling direction."
  (interactive)
  (crj/cycle-todo-keyword t))

(defun crj/get-wrapped-index (index lst)
  "A helper function for wrapping an index within a list. Pass this function an index and a list, and it will adjust if necessary for the index having incremented by 1 past the end of the list or decremented by 1 to before the start of the list."
  (let ((len (length lst)))
    (cond ((>= index len) 0)
          ((< index 0) (- len 1))
          (t index))))

(defun crj/get-wrapped-index2 (index lst)
  "A helper function for wrapping an index within a list. Pass this function an index and a list, and it will adjust if necessary for the index having incremented past the end of the list or decremented to before the start of the list."
  (let ((len (length lst)))
    (if (< index 0) (+ len index)
      (% index len))))

(defun crj/get-index-of-todo (todo-keyword)
  "Returns the index of the current todo's keyword in `crj/todo-cycling-keywords'.
Hacks its way through some unideal code by assuming that if we can't find the keyword, there's no keyword at all, which is the last state in our list.

Ideally I'd learn how to use symbols properly."
  (or
   (cl-position todo-keyword crj/todo-cycling-keywords :test 'equal)
   (- (length crj/todo-cycling-keywords) 1)))

(map! :map evil-org-mode-map
      :n "<return>" #'crj/cycle-todo-keyword-maybe
      :n "RET" #'crj/cycle-todo-keyword-maybe
      :n "S-<return>" #'crj/cycle-todo-keyword-backwards
      :n "S-RET" #'crj/cycle-todo-keyword-backwards)
