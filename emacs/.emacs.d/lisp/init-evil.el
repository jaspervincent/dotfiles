(message "Load init-evil done...")
(provide 'init-evil)

;;; evil 
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil) ;不使用自带的键位绑定。默认加载不同模式下键位绑定
  (setq evil-want-C-u-scroll t) ;C-u 实现向上滚动。默认C-u 是emacs中的功能
  (evil-mode)

  ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)) ;如果有链接，按回车能访问这个链接

  ;;; 自定义设置
  ;; 在进行插入模式后，希望使用emacs的快捷键，而又不希望进入 Emacs State
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state) ;按 [Esc] 回到normal state. 下面会介绍更方便的evil-escape插件

  ;; 加空行, 默认 o + Esc 需要2次操作. 下面的设置直接在normal 模式中操作
  (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line))) ; 向上加空行
  (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1))) ; 向上加空行

  ;; 在normal state中切换不同的buffer
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer) ;切到前一个buffer
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-motion-state-map (kbd "] b") 'next-buffer)

  ;; 在dired模式下，添加快捷键
  (evil-define-key 'normal dired-mode-map ; 只修改dired模式下normal模式的快捷键绑定
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "C-k") 'dired-up-directory ;往上一级目录
    "`" 'dired-open-term
    "q" 'quit-window ; q 退出。默认没有。 可以查看emacs state下的q绑定的函数 =C-x d= 进入dired， =C-z= 进入emacs state， =C-h k= 按 q 可查到对应的函数为 quit-window
    "o" 'dired-find-file-other-window ; 在另外一个窗口打开文件
    "z" 'dired-get-size
    ")" 'dired-omit-mode)
  )





;;; evil 使用undo-tree来管理undo redo
(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree))

(use-package general
  :init
  ;; global-definer 对应leader键为空格
  (global-definer
    "!" 'shell-command ;空格+! SPC ! 进入shell命令行
    "SPC" 'execute-extended-command ; 按2下空格SPC-SPC就可以实现 M-x 的效果
    "'" 'vertico-repeat
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "u" 'universal-argument ;SPC u 相当于emacs的C-u 
    "hdf" 'describe-function ;SPC hdf 相当于emacs的C-h f
    "hdv" 'describe-variable ;SPC hdv 相当于emacs的C-h v
    "hdk" 'describe-key ;SPC hdk 相当于emacs的C-h k
    )
  ;; leader 键为SPC空格，子leader键为b. 如 SPC b b查看buffer缓冲区
  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "b" '(consult-buffer :which-key "consult buffer") ; 查看buffer缓冲区
    "B" 'switch-to-buffer
    "p" 'previous-buffer
    "R" 'rename-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*")) 
          :which-key "messages-buffer") ; SPC b M查看*Message*缓冲区
    "n" 'next-buffer
    "i" 'ibuffer
    "f" 'my-open-current-directory
    "k" 'kill-buffer

    "y" 'copy-buffer-name
    "K" 'kill-other-buffers)
  )

;; kj 退回到普通模式
(use-package evil-escape
  :ensure t
  :init
  ;; {{ https://github.com/syl20bnr/evil-escape
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (setq-default evil-escape-key-sequence "kj")
  ;; disable evil-escape when input method is on
  (evil-escape-mode 1)
  ;; }}
)

;;下方状态栏显示单词重复出现位置
(use-package evil-anzu
  :ensure t
  :after evil
  :diminish
  :demand t
  :init
  (global-anzu-mode t))

;;; 使用社区稳定的按键绑定。 并设置不同模式中默认的evil状态
(use-package evil-collection
  :ensure t
  :demand t
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list)) ; 移除社区中插件的改键对你键位影响
  (evil-collection-init)

  ;; 设置进入mode时对应的vim 模式
  (cl-loop for (mode . state) in
           '((org-agenda-mode . normal) ;进入agenda时默认Normal State状态
             (Custom-mode . emacs)
             (eshell-mode . emacs) ;进入eshell模式时默认Emacs State状态
             (makey-key-mode . motion))
           do (evil-set-initial-state mode state)))

;;; S用法，选中一个单词后加引号。viw S " 相当于原生vim text object用法 viw Shift i " 
(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

;;; 添加不同语言的注释
(use-package evil-nerd-commenter
  :init
  ;; normal 或 visual 模式下按 ,/ 可加注释
  ;; (define-key evil-normal-state-map (kbd ",ci") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",") 'evilnc-comment-or-uncomment-lines)
  )

(use-package evil-snipe
  :ensure t
  :diminish
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;;; 按 % 可以在函数、括号之间跳转
(use-package evil-matchit
  :ensure
  :init
  (global-evil-matchit-mode 1))

(message "Load init-evil done...")
(provide 'init-evil)
