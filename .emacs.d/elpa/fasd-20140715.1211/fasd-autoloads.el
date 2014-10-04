;;; fasd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-fasd-mode fasd-add-file-to-db fasd-find-file)
;;;;;;  "fasd" "fasd.el" (21549 54254 0 0))
;;; Generated autoloads from fasd.el

(autoload 'fasd-find-file "fasd" "\
Use fasd to open a file, or a directory with dired.
If PREFIX is non-nil consider only directories.  QUERY can be
passed optionally to avoid the prompt.

\(fn PREFIX &optional QUERY)" t nil)

(autoload 'fasd-add-file-to-db "fasd" "\
Add current file or directory to the Fasd database.

\(fn)" nil nil)

(defvar global-fasd-mode nil "\
Non-nil if Global-Fasd mode is enabled.
See the command `global-fasd-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-fasd-mode'.")

(custom-autoload 'global-fasd-mode "fasd" nil)

(autoload 'global-fasd-mode "fasd" "\
Toggle fasd mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("fasd-pkg.el") (21549 54254 578824 0))

;;;***

(provide 'fasd-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fasd-autoloads.el ends here
