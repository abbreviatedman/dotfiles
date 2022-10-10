;; This is the expanded view of my code (which is further below):
;; (setq org-agenda-custom-commands
;;       `(("A" "Daily agenda and top priority tasks"
;;          ((tags-todo "*"
;;                      ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
;;                       (org-agenda-skip-function
;;                        `(org-agenda-skip-entry-if
;;                          'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-overriding-header "Important tasks without a date\n")))
;;           (agenda "" ((org-agenda-span 1)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-scheduled-past-days 0)
;;                       ;; We don't need the `org-agenda-date-today'
;;                       ;; highlight because that only has a practical
;;                       ;; utility in multi-day views.
;;                       (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
;;                       (org-agenda-format-date "%A %-e %B %Y")
;;                       (org-agenda-overriding-header "\nToday's agenda\n")))
;;           (agenda "" ((org-agenda-start-on-weekday nil)
;;                       (org-agenda-start-day "+1d")
;;                       (org-agenda-span 3)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                       (org-agenda-overriding-header "\nNext three days\n")))
;;           (agenda "" ((org-agenda-time-grid nil)
;;                       (org-agenda-start-on-weekday nil)
;;                       ;; We don't want to replicate the previous section's
;;                       ;; three days, so we start counting from the day after.
;;                       (org-agenda-start-day "+4d")
;;                       (org-agenda-span 14)
;;                       (org-agenda-show-all-dates nil)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-entry-types '(:deadline))
;;                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                       (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))
;;         ("P" "Plain text daily agenda and top priorities"
;;          ((tags-todo "*"
;;                      ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
;;                       (org-agenda-skip-function
;;                        `(org-agenda-skip-entry-if
;;                          'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-overriding-header "Important tasks without a date\n")))
;;           (agenda "" ((org-agenda-span 1)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-scheduled-past-days 0)
;;                       ;; We don't need the `org-agenda-date-today'
;;                       ;; highlight because that only has a practical
;;                       ;; utility in multi-day views.
;;                       (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
;;                       (org-agenda-format-date "%A %-e %B %Y")
;;                       (org-agenda-overriding-header "\nToday's agenda\n")))
;;           (agenda "" ((org-agenda-start-on-weekday nil)
;;                       (org-agenda-start-day "+1d")
;;                       (org-agenda-span 3)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                       (org-agenda-overriding-header "\nNext three days\n")))
;;           (agenda "" ((org-agenda-time-grid nil)
;;                       (org-agenda-start-on-weekday nil)
;;                       ;; We don't want to replicate the previous section's
;;                       ;; three days, so we start counting from the day after.
;;                       (org-agenda-start-day "+4d")
;;                       (org-agenda-span 14)
;;                       (org-agenda-show-all-dates nil)
;;                       (org-deadline-warning-days 0)
;;                       (org-agenda-block-separator nil)
;;                       (org-agenda-entry-types '(:deadline))
;;                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                       (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
;;          ((org-agenda-with-colors nil)
;;           (org-agenda-prefix-format "%t %s")
;;           (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
;;           (org-agenda-fontify-priorities nil)
;;           (org-agenda-remove-tags t))
;;          ("agenda.txt"))))
