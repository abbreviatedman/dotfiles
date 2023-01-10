;; This is the expanded view of my code (which is further below):
;; (setq org-agenda-custom-commands '(("d" "Agenda and next todos"
;;                                       ((agenda "")
;;                                        (todo "IN-PROGRESS")
;;                                        (todo "NEXT")))))
(use-package! org-agenda
  :init
  (defun crj/agenda () (interactive) (org-agenda t "g"))
  ;; (global-set-key (kbd "C-c g") #'(lambda () (interactive) (org-agenda t "g")))
  (setq org-agenda-start-day nil
        org-agenda-custom-commands '(("g" "Daily agenda and top priority tasks"
           ((todo "WAIT" ((org-agenda-overriding-header "Tasks On Hold\n")))
            (tags-todo "*"
                       ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                        (org-agenda-skip-function
                         `(org-agenda-skip-entry-if
                           'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                        (org-agenda-block-separator nil)
                        (org-agenda-overriding-header "Important Tasks\n")))
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
                        (org-agenda-overriding-header "\nNext Three Days\n")))
            (agenda "" ((org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-block-separator nil)
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "\nUpcoming Deadlines (+14d)\n")))))))
  :bind
  (("C-c a" . org-agenda)
   ("C-c g" . crj/agenda)))
