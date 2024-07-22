(global-display-line-numbers-mode 1) ;显示行号 29版本。29之前用(global-linum-mode 1)
(setq inhibit-startup-screen t) ; 尝试关掉启动界面
;;(toggle-frame-maximized)                     ; 全屏编辑器


;;(set-face-attribute 'default nil :height 150) ; 修改字号，大小为16pt
;;让鼠标滚动更好用。默认滚动很快
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(tool-bar-mode -1)                           ;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(scroll-bar-mode -1)                         ;; 关闭文件滑动控件
;; (menu-bar-mode -1)                        ;; 关闭菜单栏 (不关闭，使用插件时会用到)

(setq-default cursor-type 'bar)              ; 更改光标的样式，默认比较粗. 更多C-h v 查询帮助
;;(setq cursor-type 'bar)                    ;; 更改光标的样式。setq当前buffer生效，不能全局生效

(global-hl-line-mode t)                      ;; 高亮当前行

;;主题
(load-theme 'tango-dark)

(message "Load init-ui done...")
(provide 'init-ui)
