;; org agenda setup
(setq! org-agenda-files '("~/Sync/org"))
(after! org
  (setq org-startup-folded 'content)

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))))

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
        :desc "view" "v" (lambda () (interactive) (find-file "~/Sync/org/gcal/colin.jaffe.org"))
        :desc "post" "p" #'org-gcal-post-at-point
        :desc "delete" "d" #'org-gcal-delete-at-point
        :desc "sync" "s" #'org-gcal-sync
        :desc "fetch" "f" #'org-gcal-fetch)))

;; allow manual breaks in pomodoro
(setq org-pomodoro-manual-break t)

;; sync which org file handles which gcal
(setq org-gcal-fetch-file-alist '(("colin.jaffe@gmail.com" . "~/Sync/org/gcal/colin.jaffe.org")))
