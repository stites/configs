;; quick switching buffers
(defun switch-to-previous-buffer ()
(interactive)
(switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<f11>")  'switch-to-previous-buffer)

;; ctrl-tab & ctrl-shift-tab to switch windows
;;(global-set-key (kbd "C-tab") 'other-window)
;;(global-set-key (kbd "C-S-tab"k) 
;;  (LAMBDAk ()
;;    (INTERACTIVE)
;;    (OTHER-WINDOW -1)))
