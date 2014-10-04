(setq js-indent-level 2)
(setq js-expr-indent-offset 2)
;; autocomplete for js2-mote - by Yegge
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
;; syntax highlighting to perform:
;;   * 0 or a negative value means none.
;;   * 1 adds basic syntax highlighting.
;;   * 2 adds highlighting of some Ecma built-in properties.
;;   * 3 adds highlighting of many Ecma built-in functions
(setq js2-highlight-level 3)

