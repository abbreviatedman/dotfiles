;; org agenda setup
(setq! org-agenda-files '("~/Sync/org"))
(after! org
  (setq org-startup-folded 'content)
  (setq org-export-with-section-numbers nil)
  (add-to-list 'org-todo-keyword-faces '("NEXT" . +org-todo-project))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)"))))

(defun open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
   )))

(map! :map evil-normal-state-map :leader
      (:prefix-map ("a" . "agenda")
       :desc "view agenda" "a" #'org-agenda
       :desc "view todo-list" "t" (lambda () (interactive)  (org-todo-list 2))
       :desc "capture" "x" (lambda () (interactive) (find-file "~/Sync/org/capture.org"))
       :desc "view mobile file" "m" (lambda () (interactive) (find-file "~/Sync/org/phone.org"))
       :desc "view inbox" "i" (lambda () (interactive) (find-file "~/Sync/org/notes.org"))
       :desc "view projects" "p" (lambda () (interactive) (find-file "~/Sync/org/projects.org"))
       :desc "file" "f" #'org-refile
       (:prefix-map ("c" . "calendar")
        :desc "view" "v" #'open-calendar
        :desc "view org file" "o" (lambda () (interactive) (find-file "~/Sync/org/cal.org"))
        :desc "post" "p" #'org-gcal-post-at-point
        :desc "delete" "d" #'org-gcal-delete-at-point
        :desc "sync" "s" #'org-gcal-sync
        :desc "fetch" "f" #'org-gcal-fetch)))


;; sync which org file handles which gcal
(setq org-gcal-fetch-file-alist '(("colin@pursuit.org" . "~/Sync/org/cal.org")))

;; timestamp manipulation
(map! :map :n :leader :desc "Discover projects in directory" "J" #'org-timestamp-down)
(map! :map :n :leader :desc "Discover projects in directory" "K" #'org-timestamp-up)
