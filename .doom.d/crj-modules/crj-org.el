;; Stop indenting my code blocks. Seriously!
(setq org-edit-src-content-indentation 0)
(map!
 :leader
 (:prefix "m"
  :desc "Repair list" :n "r" #'org-list-repair))

(use-package! org
  :config
  (add-to-list 'org-todo-keyword-faces '("NEXT" . +org-todo-project))
  (setq org-link-descriptive t
        org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
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
                                ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
        org-agenda-files '("~/org-stuff/trello/9-3-co-instructors.trello"
                           "~/org-stuff/trello/9-3-fellow-cards.trello"
                           "/home/abbreviatedman/org-stuff/readme.org"
                           "/home/abbreviatedman/org-stuff/personal.org"
                           "/home/abbreviatedman/org-stuff/archive.org")
        +org-capture-emails-file "~/org-stuff/readme.org"
        org-agenda-file-regexp "\\`[^.].*\\.org\\'"
        org-startup-folded 'showeverything
        org-todo-keywords '((sequence
                             "TODO(t)"
                             "NEXT(n)"
                             "IN-PROGRESS(i)"
                             "DONE(d)")
                            (sequence
                             "|"
                             "WAIT(w)"
                             "HOLD(h)"
                             "PROJ(p)"
                             "CANCELED(c)"))
        org-agenda-custom-commands '(("d" "Agenda and next todos"
                                      ((agenda "")
                                       (todo "IN-PROGRESS")
                                       (todo "NEXT"))))))


;; This works for removing Doom's bindings.
(after! evil-org
  (map! :map evil-org-mode-map
        :n "gj" nil
        :n "gk" nil
        :n "gj" #'evil-next-visual-line
        :n "gk" #'evil-previous-visual-line
        :n "zR" nil
        :n "zR" #'org-fold-show-all))


(defun crj/prettify-js-org-src-block ()
  "Run prettier on source block at point."
  (interactive)
  (org-edit-src-code)
  (prettier-prettify)
  (org-edit-src-exit))

(map! :map org-mode-map
      :leader
      (:prefix "m"
       (:prefix "p"
        :desc "Prettify source block at point."
        :n "j" #'crj/prettify-js-org-src-block)))


(defun crj/sort-entries-by-todo-state-at-current-level ()
  "Sorts headings at current level by order of todo-keywords.

See `org-todo-keywords' for what order `org-sort-entries' uses."
  (interactive)
  (let ((prev-point (point)))
    (outline-up-heading 1)
    (crj/sort-entries-by-todo-state-for-children)
    (goto-char prev-point)))

(defun crj/sort-entries-by-todo-state-for-children ()
  "Sorts children of current heading by order of todo keywords.

See `org-todo-keywords' for what order `org-sort-entries' uses."
  (interactive)
  (let ((prev-point (point)))
    (org-sort-entries nil ?o)
    (goto-char prev-point)))


;; There were problems with regular space liner in org mode. I'm going to try it again, though, so this can go away for now.
;; (evil-define-key 'normal org-mode-map
;;   (kbd "SPC j") #'org-metadown
;;   (kbd "SPC k") #'org-metaup)

(map!
 :map org-mode-map
 :leader
 (:prefix "m"
  :desc "Sort by todo keyword."
  :n "O" #'crj/sort-entries-by-todo-state-at-current-level
  (:prefix "u"
   :desc "Sort children by todo keyword."
   :n "O" #'crj/sort-entries-by-todo-state-for-children)))

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
(map! :leader
      (:prefix "m"
       (:prefix "c"
        :desc "Adjust timestamp down."
        :n "j" #'org-timestamp-down
        :desc "Adjust timestamp up."
        :n "k" #'org-timestamp-up)))

(map! :map org-mode-map
      :desc "Change todo state." :n "C-c t" #'org-todo
      (:prefix "[" :n "H" #'outline-up-heading))



;; (use-package! org-trello
;;   :after (dash s)
;;   :config
;;   ;; Hack to keep flycheck mode off in org-trello buffers
;;   (put 'org-trello-mode 'mode-class 'special)

;;   ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
;;   ;; Additionally, turn off some modes and settings that conflict with org-trello.
;;   (defun crj/add-org-trello-mode-maybe ()
;;     (let ((old-warning-suppress-types warning-suppress-types))
;;       (setq-local warning-suppress-types (append warning-suppress-types '((org-element-cache))))
;;       (let ((filename (buffer-file-name (current-buffer))))
;;         (if (and filename (string= "trello" (file-name-extension filename)))
;;             (progn
;;               (org-trello-mode)
;;               (sleep-for 2)
;;               (org-cycle-hide-drawers 'all)
;;               (flyspell-mode -1))
;;           (setq-local warning-suppress-types old-warning-suppress-types)))))

;;   (defun crj/save-and-clean-org-trello-buffer ()
;;     (interactive)
;;     (save-buffer)
;;     (org-cycle-hide-drawers 'all))

;;   (add-hook 'org-mode-hook 'crj/add-org-trello-mode-maybe)
;;   (map! :map org-trello-mode-map
;;         :leader
;;         (:prefix ("m" . "Markup")
;;          (:prefix ("t" . "Trello")
;;           :desc "Assign a user to card."
;;           :n "a" #'org-trello-toggle-assign-user
;;           :desc "Assign yourself to card."
;;           :n "A" #'org-trello-toggle-assign-me
;;           :desc "Browse trello board."
;;           :n "b" #'org-trello-jump-to-trello-board
;;           :desc "Browse trello card."
;;           :n "B" #'org-trello-jump-to-trello-card
;;           :desc "Remove current entity."
;;           :n "k" #'org-trello-kill-entity
;;           :desc "Sync buffer to Trello."
;;           :n "g" #'org-trello-sync-buffer
;;           :desc "Sync buffer from Trello."
;;           :n "G" #'(lambda () (interactive) (org-trello-sync-buffer t))
;;           :desc "Sync card to Trello."
;;           :n "c" #'org-trello-sync-card
;;           :desc "Sync card from Trello."
;;           :n "C" #'(lambda () (interactive) (org-trello-sync-card t))
;;           :desc "Add comment to card."
;;           :n "r" #'org-trello-add-card-comment
;;           :desc "Sync comment."
;;           :n "R" #'org-trello-sync-comment
;;           :desc "Save and clean buffer."
;;           :n "s" #'crj/save-and-clean-org-trello-buffer))))

;; Mark current todo DONE and next todo NEXT
(map! :map org-mode-map :leader
      (:prefix "m"
       :desc "Next todo GTD-style" :n "m" #'(lambda ()
                                              (interactive)
                                              (org-todo 'done)
                                              (org-forward-heading-same-level 1)
                                              (org-todo 1))))

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

(map! :leader
      (:prefix "i"
       :desc "Insert timestamp."
       :n "d" #'org-time-stamp))

(map! :map emacs-everywhere-mode-map :n "ZZ" nil)
(map! :map emacs-everywhere-mode-map :n "ZZ" #'emacs-everywhere-finish-or-ctrl-c-ctrl-c)

;; easier source block insertion keyboard shortcuts
(require 'org-tempo)

;; Show full link text by default.
(setq org-link-descriptive nil)

(defun JK-org-move-to-extreme (up)
  "Move current org subtree to the end of its parent.

With prefix arg move subtree to the start of its parent.

Source: https://emacs.stackexchange.com/a/43662."
  (interactive "P")
  (condition-case err
      (while t
        (funcall (if up
                     'org-move-subtree-up
                   'org-move-subtree-down)))
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg)))))))

(defun crj/org-move-item-to-end-of-list ()
  "Move current item to bottom of list."
  (interactive)
  (JK-org-move-to-extreme nil))

(defun crj/org-move-item-to-start-of-list ()
  "Move current item to top of list."
  (interactive)
  (JK-org-move-to-extreme t))

(map! :map org-mode-map :leader
      :n "J" #'crj/org-move-item-to-end-of-list
      :n "K" #'crj/org-move-item-to-start-of-list)

(map! :map org-mode-map
      (:prefix "["
       :n "[" #'org-backward-heading-same-level)
      (:prefix "]"
       :n "]" #'org-forward-heading-same-level))

;; Trying to figure out how to change org keyword order temporarily!
;; Just to play around with it.
;; However, org is very confusingly written!
(defun crj/sort-org-entries-by-custom-todo-state ()
  "Sort entries by a custom keyword order."
  (interactive)
  (let ((org-todo-keywords '((sequence "TODO(n)" "DONE(t)" "NEXT(d)")
                             (sequence "|" "WAIT(w)" "HOLD(h)" "PROJ(p)" "CANCELED(c)"))))
    (org-reload)
    (org-sort-entries nil ?o)))

(defun crj/open-tasks-file ()
  (interactive)
  (find-file "~/org-stuff/readme.org"))

(map! :leader
      (:prefix "o"
       :desc "Open tasks repo."
       :n "o" #'crj/open-tasks-file))

;; Make it pretty.
(setq org-fancy-priorities-list '("🔺" "👍" "⬇")
      org-pretty-entities t
      org-link-descriptive t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

(defun crj/org-toggle-character-markers ()
  (interactive)
  (crj/toggle-boolean-setting 'org-hide-emphasis-markers)
  (dolist (buffer (buffer-list (current-buffer)))
    (with-current-buffer buffer
      (when (eq 'org-mode major-mode)
        (org-mode-restart)))))

(add-hook 'elfeed-search-mode-hook #'elfeed-update)
