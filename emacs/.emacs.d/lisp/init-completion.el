;; 内置补全功能n
;;(setq tab-always-indent 'complete)           ;; 使用 TAB 来列出当前补全选项

(use-package company
  :ensure t
  :init
  (global-company-mode t)                    ;; 全局开启 company 补全
  :config
  (setq company-idle-delay 0)                ;; 补全时间快些
  (setq company-minimum-prefix-length 1)     ;; 最少输入1个字符开启
  (setq company-show-numbers t)              ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择)
  (setq company-dabbrev-other-buffers 'all)  ;; 从所有缓冲区收集补全信息
  (setq company-tooltip-align-annotations t) ;; 右侧附加注释
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)) ; 根据选择的频率进行排序，如果不喜欢可以去掉
  :bind (:map company-active-map
              ("C-n" . 'company-select-next)
              ("C-p" . 'company-select-previous)))  ;; 使用 `C-n` 与 `C-p` 来选择补全项，默认选择上一条和下一条候选项命令 M-n M-p

;;; minibuffer增强，模糊搜索 (orderless)
(use-package orderless
  :ensure t
  :after minibuffer
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;; 增强文件内搜索和跳转函数定义(consult.el and j-consult.el)
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;;replace swiper 可以替代老的 ivy mode 的 swiper 功能
  :bind
  ( :map global-map
    ("M-g M-g" . consult-goto-line)
    ("C-s" . consult-line) ;; replace swipe
    ("M-s i" . consult-imenu)  ;跳转函数定义
    ;;("C-x b" .  consult-buffer) ;; 多显示最近打开文件。替换默认 C-x b buffer
    ;;("C-c p s" . consult-ripgrep) ;;  查找文件内容，需要安装 ripgrep 命令
    )
  :config
  (setq
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2         ;; 搜索 2 个字符显示输出。默认 3
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1
   consult-line-start-from-top t)
  )

(use-package j-consult
  :ensure nil
  ;;:after (consult orderless)
  ;;:demand t
  :config
  
  (setq
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 5         ;; 搜索 2 个字符显示输出。默认 3
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1
   consult-line-start-from-top t)

  ;; 使用拼音进行搜索文件 (pyim)
  (require 'pyim)
  
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))
  
  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
	(advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))
  
  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
	(advice-remove 'orderless-regexp #'eh-orderless-regexp)))
  
  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
  (add-hook 'minibuffer-exit-hook 'disable-py-search) ;退出minibuffer时自动退出拼音搜索
  
  (global-set-key (kbd "s-p") 'toggle-chinese-search) ;需要时打开拼音搜索。因为拼音搜索性能不稳定
  )

;;; minibuffer操作扩展 （embark.el and j-embark.el)
(use-package embark
  :ensure t
  :defer 1
  :init
  (setq which-key-use-C-h-commands nil
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command)
  :bind
  ( :map minibuffer-local-map
    ("C-;" . embark-act)         ;; pick some comfortable binding
   )) ;; alternative for `describe-bindings'

;;; 批量修改文件内容
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode) ;hook 第一个参数执行前，先执行第2个参数的功能
  )

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package j-embark
  :ensure nil
  :after embark
  :bind
  ( :map minibuffer-local-map
    ("C-c C-e" . my/embark-export-write) ; 打开写，配合query-replace-regexp实现文本替换
    :map embark-file-map ; 打开pc文件管理器
    ("E" . my/consult-directory-externally)
    ))

;;; Minubuffer 详细的注释信息 (marginalia.el)
(use-package marginalia
  :ensure t
  :defer 1
  :config
  (setq marginalia-max-relative-age 0) ; absolute time
  (marginalia-mode 1))

;;; Minibuffer 垂直补全布局(vertico)
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  ;; 适合vim用户习惯，上下移动 C-k, C-j 
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "s-SPC") #'+vertico/embark-preview)
  )

(message "Load init-completion done...")
(provide 'init-completion)
