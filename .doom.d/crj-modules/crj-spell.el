; Basic dictionary setup.
(after! flyspell
  (setq flyspell-lazy-window-idle-seconds 2)
  (ispell-set-spellchecker-params)
  (setq ispell-program-name "hunspell")
  ;; Add many English words, as well as Spanish.
  (ispell-hunspell-add-multi-dic "en_US,en_CA,en_AU,en_GB,es_ES")
  (ispell-change-dictionary "en_US,en_CA,en_AU,en_GB,es_ES" t))

; beespell - hacky library of mine to get dictionary removal
;; set custom dictionary
(setq crj/beespell-local-dictionary "~/.hunspell_en_US")
;; set shortcut
(map! :n "zw" nil)
(map! :n "zw" #'crj/beespell-remove-word)

; case management
(map! :n "zc" nil)
(map! (:prefix "z"
       (:prefix ("c" . "Manage case")
        (:desc "Cycle Case" :n "z" #'string-inflection-all-cycle
         :desc "Switch to camelCase" :n "c" #'string-inflection-lower-camelcase
         :desc "Switch to PascalCase" :n "C" #'string-inflection-camelcase
         :desc "Switch to snake_case" :n "s" #'string-inflection-underscore
         :desc "Switch to kebab-case" :n "k" #'string-inflection-kebab-case))))

;; spellcheck settings
(map!
 :leader
 (:prefix "m"
  :desc "Spellcheck buffer" :n "s" #'ispell-buffer))

(setq flyspell-highlight-flag t
      ispell-silently-savep t
      ispell-quietly t
      flyspell-mark-duplications-flag nil)
