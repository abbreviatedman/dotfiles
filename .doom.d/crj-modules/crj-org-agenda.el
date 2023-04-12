(use-package! org-agenda
  :init
  (defun crj/agenda () (interactive) (org-agenda t "g"))
  ;; this is for a work-only version I can display at work... not I haven't made this yet!
  (defun crj/work-agenda () (interactive) (org-agenda t "w"))
  (setq org-agenda-start-day nil
        org-agenda-custom-commands '(("g" "Daily agenda and top priority tasks"
                                      ((todo "WAIT"
                                             ((org-agenda-overriding-header "Tasks On Hold\n")
                                              (org-agenda-block-separator nil)))
                                       (todo "TODO"
                                             ((org-agenda-files '("~/org-stuff/invite.org"))
                                              (org-agenda-block-separator nil)
                                              (org-agenda-overriding-header "\n Invited Events\n")))
                                       (todo "TODO"
                                             ((org-agenda-files '("~/org-stuff/inbox.org"))
                                              (org-agenda-block-separator nil)
                                              (org-agenda-overriding-header "\nInbox Items\n")))
                                       (tags-todo "*"
                                                  ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                                                   (org-agenda-skip-function
                                                    `(org-agenda-skip-entry-if
                                                      'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                                                   (org-agenda-block-separator nil)
                                                   (org-agenda-overriding-header "\nImportant Without Deadline\n")))
                                       (agenda "" ((org-agenda-span 1)
                                                   (org-deadline-warning-days 0)
                                                   (org-agenda-block-separator nil)
                                                   (org-scheduled-past-days 0)
                                                   (org-agenda-day-face-function (lambda (_) 'org-agenda-date))
                                                   (org-agenda-format-date "%A %-e %B %Y")
                                                   (org-agenda-overriding-header "\nToday's Agenda\n")))
                                       (agenda "" ((org-agenda-start-on-weekday nil)
                                                   (org-agenda-start-day "+1d")
                                                   (org-agenda-span 3)
                                                   (org-deadline-warning-days 0)
                                                   (org-agenda-block-separator nil)
                                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                                   (org-agenda-overriding-header "\nNext Three Days After\n")))
                                       (agenda "" ((org-agenda-time-grid nil)
                                                   (org-agenda-start-on-weekday nil)
                                                   (org-agenda-start-day "+4d")
                                                   (org-agenda-span 14)
                                                   (org-agenda-show-all-dates nil)
                                                   (org-deadline-warning-days 0)
                                                   (org-agenda-block-separator nil)
                                                   (org-agenda-entry-types '(:deadline))
                                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                                   (org-agenda-overriding-header "\nUpcoming Deadlines (+14d After)\n")))))))
  :bind
  (("C-c a" . org-agenda)
   ("C-c g" . crj/agenda)))
