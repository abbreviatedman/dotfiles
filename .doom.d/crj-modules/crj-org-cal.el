(use-package! org-caldav
  :init
  (setq org-caldav-url "http://www.jaffe-cloud.com/remote.php/dav/calendars/abbreviatedman"
        org-caldav-calendars '((:calendar-id "personal" :inbox "~/org-stuff/calendars/personal-calendar.org")
                               (:calendar-id "calendargooglecom" :inbox "~/org-stuff/calendars/home-calendar.org")
                               ;; (:calendar-id "calendargooglecom-1" :inbox "~/org-stuff/calendars/9-3-calendar.org")
                               ;; (:calendar-id "calendargooglecom-2" :inbox "~/org-stuff/calendars/lectures-calendar.org")
                               ;; (:calendar-id "calendargooglecom-3" :inbox "~/org-stuff/calendars/work-calendar.org")
                               )

        org-caldav-sync-direction 'cal->org
        org-icalendar-timezone "America/New_York"))
