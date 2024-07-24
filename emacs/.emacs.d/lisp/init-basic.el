;;; 一些默认的配置

(setq w32-apps-modifier 'super)       ;PC 通过SharpKeys改成了 Application

;;; 统一配置
(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应Windows上面的Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应Windows上面的Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应Windows上面的Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank) ;对应Windows上面的Ctrl-v 粘贴
(global-set-key (kbd "s-z") 'undo) ;对应Windows上面的Ctrol-z 撤销

;; 查询函数、变量、key 定义的文件位置
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 设置系统的编码，避免各处的乱码
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; 开启服务
;;(server-mode 1)

(electric-pair-mode t)                       ; 括号补全
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑。默认选中放后面

(global-auto-revert-mode t)                  ; 自动加载外部修改过的文件
(setq auto-save-default nil)                 ; 关闭自动保存文件，#为后缀的文件
(setq ring-bell-function 'ignore)            ; 关闭提示声音
(fset 'yes-or-no-p 'y-or-n-p)                ;; 某个命令时需要输入 (yes or no)

;;; 最近编辑过的文件
(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;;; Save and restore editor sessions between restarts
;; 记住使用过的命令
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300)
  )

;; 记住光标所在文件的位置
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)) ;在emacs配置完全加载好以后，执行save-place-mode

;;; modeline上显示文件大小、 列号
(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))

(use-package general
  :ensure t
  :init
  ;; 针对evil插件。设置了一些类似leader键的函数. 如 leader 为空格对应函数global-definer , leader 为逗号对应函数global-leader
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))


  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  ;; 定义一个宏。这个宏的作用是继承global-dfiner函数的leader键SPC空格，接收一个inix-key变量的值为子leader
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))))

(message "Load init-basic done...")
(provide 'init-basic)
