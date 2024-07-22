;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)
(require 'use-package)


;;; modeline上显示我的所有的按键和执行的命令
;;---(package-install 'keycast) ;包会安装在elpa目录中
;;----(keycast-mode-line-mode t)

(message "Load init-packages done...")
(provide 'init-packages)
