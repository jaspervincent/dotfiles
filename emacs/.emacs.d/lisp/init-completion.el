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

;;; 增强 minibuffer 补全：vertico 和 Orderless

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :after vertico
  :init (setq completion-styles '(orderless)))

;;; 配置 Marginalia 增强 minubuffer 的 annotation
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
)

;;; minibuffer action 和自适应的 context menu：Embark
(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   )) ;; alternative for `describe-bindings'

;;; 增强文件内搜索和跳转函数定义：Consult
(use-package consult
  :ensure t
  ;;replace swiper 可以替代老的 ivy mode 的 swiper 功能
  :bind (
         ("C-s" . consult-line) ;; replace swipe
         ("M-s i" . consult-imenu)  ;跳转函数定义
         ;;("C-x b" .  consult-buffer) ;; 多显示最近打开文件。替换默认 C-x b buffer
         ;;("C-c p s" . consult-ripgrep) ;;  查找文件内容，需要安装 ripgrep 命令
         )
  )

;;; 批量修改文件内容
(use-package embark-consult
  :ensure t)

(use-package wgrep
  :ensure t)

(setq wgrep-auto-save-buffer t)

(eval-after-load 'consult
  '(eval-after-load 'embark
     '(progn
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))) ;hook 第一个参数执行前，先执行第2个参数的功能

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;;; 配置搜索中文文件
;; PC提前安装 everyting 及其客户端ES. 利用M-x consult-locate搜索
(progn
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
  )
(eval-after-load 'consult
  (progn
      (setq
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2         ;; 搜索 2 个字符显示输出。默认 3
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
      ))

;; make c-j/c-k work in vertico selection 添加minibuffer中上下移动键位绑定
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)

;; make consult-ripgrep work 有时consult-ripgrep搜索不了
(cond
 ;; macOS
 ((eq system-type 'darwin)
  "afplay")
 ;; Windows
 ((eq system-type 'windows-nt)
  (add-to-list 'process-coding-system-alist 
               '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
  )
 (t
  "Nothing"))

(message "Load init-completion done...")
(provide 'init-completion)
