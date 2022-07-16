;; Kill buffer and window together.
(map! :leader
      (:prefix "b"
       :desc "Kill the current buffer and delete the selected window."
       :n "W" #'kill-buffer-and-window)
      (:prefix "w"
       :desc "Kill the current buffer and delete the selected window."
       :n "B" #'kill-buffer-and-window))
