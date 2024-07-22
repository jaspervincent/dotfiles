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
(package-install 'vertico)
(vertico-mode t)

(package-install 'orderless)
(setq completion-styles '(orderless))

;;; 配置 Marginalia 增强 minubuffer 的 annotation
(package-install 'marginalia)
(marginalia-mode t)

;;; minibuffer action 和自适应的 context menu：Embark
(package-install 'embark)
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

;;; 增强文件内搜索和跳转函数定义：Consult
(package-install 'consult)
;;replace swiper 可以替代老的 ivy mode 的 swiper 功能
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-s i") 'consult-imenu)  ;跳转函数定义

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

;;; 批量修改文件内容
(package-install 'embark-consult)
(package-install 'wgrep)
(setq wgrep-auto-save-buffer t)

(eval-after-load 'consult
  '(eval-after-load 'embark
     '(progn
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))) ;hook 第一个参数执行前，先执行第2个参数的功能

(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

(message "Load init-completion done...")
(provide 'init-completion)
