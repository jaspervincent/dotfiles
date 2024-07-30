;;; config

(with-eval-after-load 'org  ; 启动org文件时加载
  ;; 使用旧版快捷键<s +Tab
  (require 'org-tempo)
  
  ;; 其他
  (require 'org-protocol) ; 加载内置包, 在摘取网页内容时会用到
  (setq org-image-actual-width nil) ; 内嵌图片是否显示实际宽度, 默认为t
  (setq org-return-follows-link t)  ; 链接位置回车，使用外部程序打开
  
  ;; 禁用左尖括号
  (setq electric-pair-inhibit-predicate
	`(lambda (c)
           (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
  
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))))
)

;;; 安装 org，这个配置一定要配置在 use-package 的初始化之前，否则无法正常安装
;; (assq-delete-all 'org package--builtins)
;; (assq-delete-all 'org package--builtin-versions)
;; (use-package org
;;   :pin gnu-elpa
;;   :ensure t)

(use-package org-contrib  ;非org的官方贡献的插件
  :ensure t
  :pin nongnu)

;;; Org-mode (personal information manager)
(use-package org
  :ensure nil
  :init
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  ( :map global-map
    ("C-c l" . org-store-link) ; 存储当前连接
    ("C-c o" . org-open-at-point-global)
    :map org-mode-map
    ("C-c M-l" . org-insert-last-stored-link) ; 插入最近存储的连接
    ("C-c C-M-l" . org-toggle-link-display) ; 显示连接内容
    :map ctl-x-x-map
    ("i" . j-org-id-headlines) ; 为标题创建属性CUSTOM_ID
    ("h" . j-org-ox-html)      ; org导出为html
    ))
;;;; links
(use-package org
  :ensure nil
  :config
  (require 'j-org) ; for the above commands

  (setq org-link-context-for-files t)
  (setq org-link-keep-stored-after-insertion nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;;;; code blocks
(use-package org
  :ensure nil
  :config
  ;;(setq org-confirm-babel-evaluate nil)             ; 使用Babel执行代码块时, 是否弹出确认对话框. nil 禁止弹出, 默认t弹出
  ;;(setq org-src-window-setup 'current-window)       ; 源代码窗口的布局位置. 默认 'reorganize-frame 侧边。 'current-window 当前窗口 
  ;;(setq org-edit-src-persistent-message nil)        ; 编辑源代码时显示持久的消息。nil 不显示, 默认 t
  ;;(setq org-src-fontify-natively t)                 ; 源代码本地高亮。默认t
  (setq org-src-preserve-indentation t)             ; 编辑源代码时保留原有的缩进，t代表启用该功能。默认nil不保留. 
  (setq org-src-tab-acts-natively t)                ; 源代码编辑模式下，Tab键的行为与原生代码编辑器一致，t代表启用该功能,  默认t
  (setq org-edit-src-content-indentation 0))          ; 代码块里缩进，0不缩进，默认是2(重要)

(with-eval-after-load 'org  ; 启动org文件时加载
  ;; 自定义org todo  C-c C-t 
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
		(sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
)

(with-eval-after-load 'org
  ;; 引入checklist, 自动更新状态
  ;; org-set-property 添加 RESET_CHECK_BOXS, 设置为t
  (require 'org-checklist)
  ;; need repeat task and properties
  (setq org-log-done t)
  (setq org-log-into-drawer t)

)

;;; 日历
;; C-c C-s schedule
;; C-c C-d deadline
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/gtd.org")) ;; 定义 agenda 文件，可以是多个
(setq org-agenda-span 'day) ;; 按天观察

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/gtd.org" "Workspace")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
	;; 用来摘取网页内容
        ("x" "Web Collections" entry
         (file+headline org-agenda-file-note "Web")
         "* %U %:annotation\n\n%:initial\n\n%?")
	))

(setq org-agenda-file-note (expand-file-name "~/notes.org")) ; 保存网页内容

(global-set-key (kbd "C-c r") 'org-capture)

(setq org-agenda-custom-commands
      '(("c" "重要且紧急的事"
         ((tags-todo "+PRIORITY=\"A\"")))
        ;; ...other commands here
        ))

;;; 截图
(use-package j-org-download
  :ensure nil
  :demand t
  :bind
  ("<f2>" . my/org-insert-clipboard-image))

(use-package flyspell-correct
  :ensure t
  :init)

;; ispell 为内置包，需要使用外部程序来做语法检查
(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell")
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (setq ispell-personal-dictionary "d:/msys64/mingw64/lib/aspell-0.60/en_GB")
  )

(define-key evil-insert-state-map (kbd "C-;") 'flyspell-correct-previous)

;;; fanyi翻译
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     ;; fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     ;; fanyi-longman-provider)
                   )))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org"))  ; 默认笔记目录，提前手动创建好
  :bind (("C-c n l" . org-roam-buffer-toggle)  ; 显示反向链接
         ("C-c n f" . org-roam-node-find)      ; 查找
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)    ; 插入
         ("C-c n c" . org-roam-capture)        ; 捕获
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)) ; 记日志功能
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; 可以按标题搜索、tags搜索
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)    ; 刷新数据 sqlite3 库 ~/.emacs.d/org-roam.db
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :after org-roam)

(message "Load init-org done...")
(provide 'init-org)
