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
  (define-key evil-insert-state-map [escape] 'evil-normal-state) ;按 [Esc] 回到normal state
  ;;(define-key evil-insert-state-map (kbd "kj") 'evil-normal-state) ;按  回到normal state

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
  :diminish
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree))

(message "Load init-evil done...")
(provide 'init-evil)
