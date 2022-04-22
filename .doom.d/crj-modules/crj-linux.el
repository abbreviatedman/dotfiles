;; Node Location
(setenv "NODE_PATH" nil)
(setenv "PATH" (concat (getenv "PATH") ":/usr/sbin/node"))
(setq exec-path (append exec-path '("/usr/sbin/node")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/sbin"))
(setq exec-path (append exec-path '("/usr/sbin")))

(map! :map 'emacs-everywhere-mode-initial-map "DEL" nil)
