;; node location
(setenv "NODE_PATH" nil)
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/local/node"))
(setq exec-path (append exec-path '("/usr/local/bin")))
