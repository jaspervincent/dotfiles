;;;; 常规设置和常用自定义函数(j-simple.el)

(use-package j-simple
  :ensure nil
  :demand t
  :bind
  ( ("ESC ESC" . j-simple-keyboard-quit-dwim)
    ("C-g" . j-simple-keyboard-quit-dwim)
    ("C-M-SPC" . j-simple-mark-sexp)   ; 选中区域
    ))

(message "Load init-essentials done...")
(provide 'init-essentials)
