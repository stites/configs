(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )
