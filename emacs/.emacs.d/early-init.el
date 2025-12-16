;;; frame基础设置
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version))) ;; 自定义标题栏内容
(setq
      inhibit-splash-screen t             ; 禁用 Emacs 启动时的欢迎（Splash）屏幕。 启动时会直接进入默认的初始缓冲区。
      inhibit-startup-screen t            ; 禁用 Emacs 启动时的启动（Startup）屏幕（通常显示版本信息和帮助提示）。 
                                          ; 与上面类似，旨在加快启动速度和简洁界面。
 )
(setq frame-resize-pixelwise t            ; 框架大小可以按一个像素增加或减少。默认nil，按照字符单元（行和列）来调整
      frame-inhibit-implied-resize 'force ; 强制阻止 Emacs 在某些操作下（如隐藏或显示窗口的菜单栏、工具栏等）自动调整窗口大小。 
                                          ; force 是一个比较强的设置，确保窗口大小保持不变。
      use-dialog-box t                    ; 指示 Emacs 在需要用户交互（如通过鼠标操作）时，是否应该使用图形界面的对话框。 
                                          ; 虽然注释提到这主要用于鼠标事件，但设置为 t 会启用图形对话框。
      use-file-dialog nil                 ; 禁用图形界面的文件选择对话框。 
                                          ; 当执行如 C-x C-f（find-file）这样的文件操作时，Emacs 将在 Minibuffer
                                          ; 中进行提示和输入，而不是弹出单独的图形窗口。这对于习惯纯键盘操作的用户很常用。
      use-short-answers t                 ; 指示 Emacs 在 Minibuffer 提示需要 "yes or no" 回答时，
                                          ; 接受简短的回答，例如 y 或 n。 否则可能需要输入完整的 "yes" 或 "no"。
      inhibit-x-resources t               ; 禁用从 X 资源（X resources）文件中读取配置设置。 这确保 Emacs 只使用其
                                          ; 自身的配置文件（如 init.el），避免与系统或用户环境中的 X 配置冲突或被其覆盖。
      inhibit-startup-echo-area-message user-login-name ; 控制启动时 Echo Area（Emacs 窗口底部的消息区域）显示的消息。 
                                          ; 将其设置为 user-login-name（即当前用户的登录名）会禁止显示标准的启动
                                          ; 信息（如 "For more information..."）。
      inhibit-startup-buffer-menu t)      ; 禁止在启动时自动显示 *Buffer List* 缓冲区菜单。 Emacs 启动后会直接
                                          ; 显示初始缓冲区，而不是缓冲区列表。

;;; frame参数
(tool-bar-mode -1)                        ; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(scroll-bar-mode -1)                      ; 关闭文件滑动控件
;; (menu-bar-mode -1)                     ; 关闭菜单栏 (不关闭，使用插件时会用到)

;;(toggle-frame-maximized)                ; 全屏编辑器
;;(setq  initial-frame-alist (quote ((fullscreen . maximized))))

;; 设置字体
;; ;;(add-to-list 'default-frame-alist `(font . "Iosevka-20"))
;;(defun rc/get-default-font ()
;;  (cond
;;   ((eq system-type 'windows-nt) "Consolas-13")
;;   ((eq system-type 'gnu/linux) "Iosevka-20")))
;;
;;(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;; 一台机器有电池目录非空，台式机目录为空。不为空时为真（即有电源信息，是笔记本电脑）
(defvar j-laptop-p (null (directory-empty-p "/sys/class/power_supply/"))
  "When non-nil, we assume to be working on a laptop.") 

;; 如果是笔记本电脑，则激活一个设置：每当 Emacs 窗口被最大化时，标题栏将自动隐藏，从而最大化可用的编辑区域空间。
;;(when j-laptop-p
;;  (add-hook 'window-size-change-functions #'frame-hide-title-bar-when-maximized)) 

(defvar j-pgtk-p (string-match-p "PGTK" system-configuration-features)
  "When non-nil, this is a build --with-pgtk.
PGTK is the Wayland-specific build of Emacs.")

(setq initial-frame-alist `(
      (horizontal-scroll-bars . nil)      ; 禁用水平滚动条。设置后，窗口底部将不会显示用于水平滚动的条。
     ;; (menu-bar-lines . 0)                ; 隐藏菜单栏。将菜单栏的高度设置为 0 行。
      (tool-bar-lines . 0)                ; 隐藏工具栏。将工具栏的高度设置为 0 行（通常是图标）。
      (vertical-scroll-bars . nil)        ; 禁用垂直滚动条。设置后，窗口右侧将不会显示用于垂直滚动的条。
      (scroll-bar-width . ,(if j-pgtk-p 12 6))  ; 设置滚动条的宽度。真则宽度设为 12 像素；否则设为 6 像素。
                                                ; 前一项禁用了滚动条，这个设置可能不会生效
      (width . (text-pixels . 600))       ; 设置框架的宽度。框架的初始宽度设置为 800 像素。
                                          ; 使用 text-pixels 确保以像素为单位，而不是字符数。
      (height . (text-pixels . 700))      ; 设置框架的高度。框架的初始高度设置为 900 像素。
      ;;,@(unless j-pgtk-p
      ;;    (list '(undecorated . t)))      ; 条件设置：无边框。如果变量 j-pgtk-p为假，则添加 (undecorated . t)，
                                          ; 这会使得 Emacs 框架没有窗口管理器提供的装饰（如标题栏、边框、关闭/最小化按钮）。
      (border-width . 0)                  ; 设置边框宽度为 0。
      ;;,@(when j-laptop-p
      ;;    (list '(fullscreen . maximized)))    ; 条件设置：最大化。如果变量 j-laptop-p 为真，则添加(fullscreen . maximized)，
                                               ; 这使得 Emacs 框架以最大化状态启动。
      ))

;; 在 Emacs 完成初始化文件（如 init.el）的加载之后，会执行这个配置。
;; 将一个匿名函数（lambda）挂载到 'after-init-hook 上。
(add-hook 'after-init-hook (lambda ()
                            (setq default-frame-alist `((horizontal-scroll-bars . nil)
                                                        ;;(menu-bar-lines . 0)
                                                        (tool-bar-lines . 0)
                                                        (vertical-scroll-bars . nil)
                                                        (scroll-bar-width . (if j-pgtk-p 12 6))
                                                        (width . (text-pixels . 600))
                                                        (height . (text-pixels . 700))
                                                        ;;,@(unless j-pgtk-p
                                                        ;;    (list '(undecorated . t)))
                                                        (border-width . 0)
                                                        ;;,@(when j-laptop-p
                                                        ;;    (list '(fullscreen . maximized)))
))))

(setq gc-cons-threshold (* 50 1000 1000))
