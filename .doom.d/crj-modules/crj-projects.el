(setq projectile-track-known-projects-automatically nil)
(use-package! projectile
  :config
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".spec"))

;; Opens minibuffer to select a root folder from which to discover projects.
(map! :leader
      (:prefix ("p" . "project")
       :desc "Discover projects in directory"
       :n "h" #'projectile-discover-projects-in-directory
       :desc "Open a project vterm"
       :n "v" #'projectile-run-vterm
       :desc "Open a new project vterm"
       :n "V" #'(lambda () (interactive) (projectile-run-vterm 1))
       :desc "Open a project eshell"
       :n "e" 'projectile-run-eshell
       :desc "Open a new project eshell"
       :n "E" #'(lambda () (interactive) (projectile-run-eshell 1))
       :desc "Edit project dir-locals"
       :n "l" #'projectile-edit-dir-locals))

(defun rename-buffer-with-project-name-prefix ()
"Prompts the user to rename the buffer, supplying the project prefix."
  (interactive)
  (let* ((project-prefix (concat (projectile-default-project-name (projectile-project-name)) "-"))
         (prompt (concat "New Buffer Name: " project-prefix))
         (name (concat project-prefix (read-string prompt))))
    (rename-buffer name)))

;; Rename buffers.
(map! :leader
      (:prefix "b"
       :desc "Rename buffer"
       :n "R" #'rename-buffer-with-project-name-prefix))

(map! :leader
      (:prefix ("TAB" . "+workspace")
               :desc "Switch to other buffer." :n "TAB" #'evil-switch-to-windows-last-buffer))

(map! :leader
      :desc "Switch to workspace buffer." :n "<" #'+vertico/switch-workspace-buffer
      :desc "Switch to buffer." :n "," #'consult-buffer)

;; (map! :leader
;;       (:prefix ("p" . "project")
;;        :desc "Open a project vterm"
;;        :n "v" #'projectile-run-vterm
;;        :desc "Open a new project vterm"
;;        :n "V" #'(lambda () (interactive) (projectile-run-vterm 1))
;;        :desc "Open a project eshell"
;;        :n "e" 'projectile-run-eshell
;;        :desc "Open a new project eshell"
;;        :n "E" #'(lambda () (interactive) (projectile-run-eshell 1))
;;        :desc "Edit project dir-locals"
;;        :n "l" #'projectile-edit-dir-locals))
