;;; config

;;; 文件搜索
(global-set-key (kbd "C-c p f") 'project-find-file) ;;   查找文件，默认绑定在 C-x p f
(global-set-key (kbd "C-c p s") 'consult-ripgrep)  ;;  查找文件内容

(use-package general
  :init
  ;; global-definer 对应leader键为空格 SPC
  (global-definer
    "!" 'shell-command ;空格+! SPC ! 进入shell命令行
    "SPC" 'execute-extended-command ; 按2下空格SPC-SPC就可以实现 M-x 的效果
    "'" 'vertico-repeat
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "u" 'universal-argument    ; SPC u 相当于emacs的C-u 
    "hdf" 'describe-function   ; SPC hdf 相当于emacs的C-h f
    "hdv" 'describe-variable   ; SPC hdv 相当于emacs的C-h v
    "hdk" 'describe-key        ; SPC hdk 相当于emacs的C-h k
    ;; 查看自定义函数库j-highlight-global, j-expand-region
    "hh" 'my/highlight-dwim    ; SPC hh 高亮选中区域并标上颜色
    "hc" 'my/clearn-highlight  ; SPC hc 清除高亮颜色
    ;; 切换窗口
    "v" 'er/expand-region      ; SPC v 扩大区域
    "0" 'select-window-0       ; SPC 0 切0号窗口
    "1" 'select-window-1       ; SPC 1 切1号窗口
    "2" 'select-window-2
    "3" 'select-window-3
    "4" 'select-window-4
    "5" 'select-window-5
    ;; 函数定义、引用跳转
    "gg" 'xref-find-definitions
    "gr" 'xref-find-references
    )
  ;; 子菜单
  ;;(+general-global-menu! "<name>" "<keyword>" 
  ;; leader 键为SPC空格，name为描述，子leader键为keyword. 如 SPC b b查看buffer缓冲区

  ;; SPC s 针对函数列表跳转的快捷键
  (+general-global-menu! "search" "s"
    "j" 'my/imenu              ; SPC s j 函数列表跳转(常用)
    "p" 'consult-ripgrep       ; SPC s p 查找文件内容(常用)
    "k" 'consult-keep-lines    ; SPC s k 查找的内容覆盖整个文件
    "f" 'consult-focus-lines)  ; SPC s f 在选中区域，隐藏匹配字符的行. 再次执行恢复显示

  ;; SPC b 针对buffer缓冲区的快捷键
  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "b" '(consult-buffer :which-key "consult buffer") ; SPC b b 查看buffer缓冲区(常用)
    "B" 'switch-to-buffer
    "p" 'previous-buffer
    "R" 'rename-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*")) 
          :which-key "messages-buffer") ; SPC b M查看*Message*缓冲区(常用 配合SPC b b快速切换)
    "n" 'next-buffer
    "i" 'ibuffer
    "f" 'my-open-current-directory
    "k" 'kill-buffer
    "y" 'copy-buffer-name
    "K" 'kill-other-buffers)

  (+general-global-menu! "layout" "l"
    "l" 'tabspaces-switch-or-create-workspace           ; SPC l l 切换工作空间(1)
    "L" 'tabspaces-restore-session                      ; SPC l L 恢复工作空间(5)
    "p" 'tabspaces-open-or-create-project-and-workspace ; SPC l p 打开一个目录并创建workspace(3)
    "f" 'tabspaces-project-switch-project-open-file
    "s" 'tabspaces-save-session                         ; SPC l s 保存工作空间(4)
    "B" 'tabspaces-switch-buffer-and-tab
    "b" 'tabspaces-switch-to-buffer
    "R" 'tab-rename                                     ; SPC l R 重命名tab-bar
    "TAB" 'tab-bar-switch-to-recent-tab                 ; SPC l TAB 在两个挨着最近的空间进行切换(2)
    "r" 'tabspaces-remove-current-buffer
    "k" 'tabspaces-close-workspace)

  ;; SPC f 针对文件快捷键
  (+general-global-menu! "file" "f"
    "f" 'find-file
    "r" 'consult-recent-file     ; SPC f r 列出最近打开过的文件(常用)
    "L" 'consult-locate          ; SPC f L 文件查找
    "d" 'consult-dir
    "ed" 'open-my-init-file
    "s" 'save-buffer
    "w" 'sudo-edit
    "S" 'save-some-buffers
    "j"  'dired-jump
    "y" 'copy-file-name
    "R" 'my/rename-current-buffer-file
    "k" 'my/delete-file-and-buffer
    "!" 'my/exec-shell-on-buffer)

  ;; SPC w 针对窗口的快捷键
  (+general-global-menu! "window" "w"
    "/" 'split-window-right    ; SPC w / 左右分屏
    "-" 'split-window-below    ; SPC w - 上下分屏
    "m" 'delete-other-windows  ; SPC w m 仅保留当前窗口
    "u" 'winner-undo           ; SPC w u 返回布局
    "z" 'winner-redo           ; SPC w z 撤销返回布局。 和返回布局结合实现2个布局间切换
    "w" 'esw/select-window     ; SPC w w 选择分屏方式 >向右分，<向左分，^向上分，v向下分
    "s" 'esw/swap-two-windows  ; SPC w s 交换窗口 
    "d" 'esw/delete-window     ; SPC w d 删除指定窗口
    "=" 'balance-windows-area  ; SPC w = 等宽布局
    "r" 'esw/move-window
    "x" 'resize-window         ; SPC w x 调整窗口大小。如f水平变大；b缩小；n向下扩；p向上扩；
    "H" 'buf-move-left         ; SPC w H 右边窗口左移
    "L" 'buf-move-right        ; SPC w L 窗口右移
    "J" 'buf-move-down         ; SPC w J 窗口下移
    "K" 'buf-move-up)          ; SPC w K 窗口上移

  ;; SPC p 针对项目的快捷键
  (+general-global-menu! "project" "p"
    "f" 'project-find-file    ; SPC p f 在项目中快速找到文件(常用)
    "r" 'consult-recent-file
    "s" 'project-find-regexp
    "d" 'project-dired
    "b" 'consult-project-buffer
    "e" 'project-eshell
    "m" 'my/project-run-makefile-target
    "c" 'project-compile
    "t" 'my/project-citre
    "p" 'project-switch-project
    "i" 'my/project-info
    "a" 'project-remember-projects-under
    "x" 'project-forget-project)
  )

(message "Load init-keybindings done...")
(provide 'init-keybindings)
