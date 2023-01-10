(use-package! org-caldav
  :init
  (setq org-caldav-url "http://www.jaffe-cloud.com/remote.php/dav/calendars/abbreviatedman"
        org-caldav-calendars '((:calendar-id "calendargooglecom" :inbox "~/org-stuff/calendars/home-calendar.org")
                               (:calendar-id "personal" :inbox "~/org-stuff/calendars/personal-calendar.org")
                               ;; (:calendar-id "calendargooglecom-1" :inbox "~/org-stuff/calendars/9-3-calendar.org")
                               ;; (:calendar-id "calendargooglecom-2" :inbox "~/org-stuff/calendars/lectures-calendar.org")
                               ;; (:calendar-id "calendargooglecom-3" :inbox "~/org-stuff/calendars/work-calendar.org")
                               )

        org-caldav-sync-direction 'cal->org
        org-icalendar-timezone "America/New_York"))

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    ;; (cfw:org-create-file-source "cal" "~/org-stuff/calendars/personal-calendar.org" "Blue")  ; other org source
    ;; (cfw:howm-create-source "Blue")  ; howm source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    (cfw:ical-create-source "Posteo" "https://posteo.de/calendars/ics/nfqs4gv2jjc2z2935mahw2lhewladzkr" "Orange")
    (cfw:ical-create-source "NextCloud" "https://www.jaffe-cloud.com/remote.php/dav/calendars/abbreviatedman/calendargooglecom/" "IndianRed")
   )))
