
;; Node Location
(setenv "PATH" (concat (getenv "PATH") ":/usr/sbin/node"))
(setq exec-path (append exec-path '("/usr/sbin/node")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/sbin"))
(setq exec-path (append exec-path '("/usr/sbin")))
