;; org agenda setup
(setq! org-agenda-files '("~/Sync/org"))
(setq! org-agenda-file-regexp "\\`[^.].*\\.org\\'")
;; Stop indenting my code blocks. Seriously!
(setq org-edit-src-content-indentation 0)

(after! org
  (setq org-startup-folded 'showall)
  (setq +org-capture-notes-file "inbox.org")
  (setq +org-capture-todo-file "inbox.org")
  (setq org-capture-templates
    '(("t" "Personal todo" entry
      (file+headline +org-capture-todo-file "Todos")
      "* TODO %?\n%i\n%a" :prepend t)
    ("n" "Personal notes" entry
      (file+headline +org-capture-notes-file "Notes")
      "* %?\n%i\n%a" :prepend t)
    ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)
    ("p" "Templates for projects")
    ("pt" "Project-local todo" entry
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
    ("pn" "Project-local notes" entry
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
    ("pc" "Project-local changelog" entry
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)
    ("o" "Centralized templates for projects")
    ("ot" "Project todo" entry
      #'+org-capture-central-project-todo-file
      "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
    ("on" "Project notes" entry
      #'+org-capture-central-project-notes-file
      "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
    ("oc" "Project changelog" entry
      #'+org-capture-central-project-changelog-file
      "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
    (setq org-export-with-section-numbers nil)
    (add-to-list 'org-todo-keyword-faces '("NEXT" . +org-todo-project))
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DONE(d)") (sequence "|" "WAIT(w)" "HOLD(h)" "PROJ(p)" "CANCELED(c)"))))

(defun open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:org-create-source "Green"))))

(map! :leader "a" nil)
(map! :leader
  (:prefix ("a" . "agenda")
    :desc "view agenda" :n "a" #'org-agenda
    :desc "view todo-list" :n "t" #'(lambda () (interactive)  (org-todo-list 2))
    :desc "capture" :n "x" #'(lambda () (interactive) (find-file "~/Sync/org/capture.org"))
    :desc "view mobile file" :n "m" #'(lambda () (interactive) (find-file "~/Sync/org/phone.org"))
    :desc "task org file" :n "o" #'(lambda () (interactive) (find-file "~/Sync/org/tasks.org"))
    :desc "view projects" :n "p" #'(lambda () (interactive) (find-file "~/Sync/org/projects.org"))
    :desc "file" :n "f" #'org-refile
      (:prefix ("c" . "calendar")
      :desc "view" :n "v" #'open-calendar
      :desc "view org file" :n "o" #'(lambda () (interactive) (find-file "~/Sync/org/cal.org"))
      :desc "post" :n "p" #'org-gcal-post-at-point
      :desc "delete" :n "d" #'org-gcal-delete-at-point
      :desc "sync" :n "s" #'org-gcal-sync
      :desc "fetch" :n "f" #'org-gcal-fetch)))


;; sync which org file handles which gcal
(setq org-gcal-fetch-file-alist '(("colin@pursuit.org" . "~/Sync/org/cal.org")))

;; timestamp manipulation
(map! :map :n :leader :desc "Discover projects in directory" "J" #'org-timestamp-down)
(map! :map :n :leader :desc "Discover projects in directory" "K" #'org-timestamp-up)

(map! :map org-mode-map (:prefix "[" :n "H" #'outline-up-heading))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(defun crj/add-org-trello-mode-maybe ()
  (let ((filename (buffer-file-name (current-buffer))))
    (when (and filename (string= "trello" (file-name-extension filename)))
      (org-trello-mode))))

(add-hook 'org-mode-hook 'crj/add-org-trello-mode-maybe)

(eval-after-load 'ox-html
  '(setq org-html-special-string-regexps nil))

;; Mark current todo DONE and next todo NEXT
(map! :map org-mode-map :leader
      (:prefix "m"
       :desc "Next todo GTD-style" :n "m" #'(lambda ()
                                              (interactive)
                                              (org-todo 'done)
                                              (org-forward-heading-same-level 1)
                                              (org-todo 2))))

;; Emacs user Tecosaur's dwim for hitting return in org mode.
(defun tecosaur/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (tecosaur/org-element-descendant-of type parent))))

(defun tecosaur/org-return-dwim (&optional default)
  "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

Adapted from Tecosaur's version.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
    (org-return t)
    (cond
    ;; Act depending on context around point.

    ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
    ;; followed.

    ((eq 'link (car (org-element-context)))
     ;; Link: Open it.
     (org-open-at-point-global))

    ((org-at-heading-p)
    ;; Heading: Move to position after entry content.
    ;; NOTE: This is probably the most interesting feature of this function.
    (let ((heading-start (org-entry-beginning-position)))
      (goto-char (org-entry-end-position))
      (cond ((and (org-at-heading-p)
                  (= heading-start (org-entry-beginning-position)))
              ;; Entry ends on its heading; add newline after
              (end-of-line)
              (insert "\n\n"))
            (t
             (newline)
             (forward-line -1)))))

    ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

      ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                      (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (tecosaur/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

    ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return t))

      ((org-at-table-p)
      (cond ((save-excursion
                (beginning-of-line)
                ;; See `org-table-next-field'.
                (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
              ;; Empty row: end the table.
              (delete-region (line-beginning-position) (line-end-position))
              (org-return t))
            (t
              ;; Non-empty row: call `org-return-indent'.
              (org-return t))))
      (t
      ;; All other cases: call `org-return-indent'.
      (org-return t)))))

(map!
  :after evil-org
  :map evil-org-mode-map
  :i [return] #'tecosaur/org-return-dwim
  :i "C-k" nil)

(setq org-blank-before-new-entry
      '((heading . auto)
        (plain-list-item . nil)))

(map! :map emacs-everywhere-mode-map :n "ZZ" nil)
(map! :map emacs-everywhere-mode-map :n "ZZ" #'emacs-everywhere-finish-or-ctrl-c-ctrl-c)
(setq org-html-postamble nil
      org-export-with-toc nil
      org-treat-S-cursor-todo-selection-as-state-change)
(setq org-export-with-entities)
