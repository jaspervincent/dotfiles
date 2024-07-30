  ;;; 多光标操作 iedit & evil-multiedit
  (use-package iedit
    :ensure t
    :init
    (setq iedit-toggle-key-default nil)
    :config
    (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
    (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))

  (use-package evil-multiedit
    :ensure t
    :commands (evil-multiedit-default-keybinds)
    :init
    (evil-multiedit-default-keybinds))

  (use-package expand-region
    :ensure t
    :config
    ;; 定义子菜单
    (defadvice er/prepare-for-more-expansions-internal
        (around helm-ag/prepare-for-more-expansions-internal activate)
      ad-do-it
      (let ((new-msg (concat (car ad-return-value)
                             ", H to highlight in buffers"
                             ", / to search in project, "
                             "e iedit mode in functions"
                             "f to search in files, "
                             "b to search in opened buffers"))
            (new-bindings (cdr ad-return-value)))
        (cl-pushnew  ; 高亮
         '("H" (lambda ()
                 (interactive)
                 (call-interactively
                  'my/highlight-dwim))) ;自定库j-highlight-global
         new-bindings)
        (cl-pushnew ; 查找当前项目(git项目)目录中匹配选中字符串的文件
         '("/" (lambda ()
                 (interactive)
                 (call-interactively
                  'my/search-project-for-symbol-at-point))) ;自定库j-expand-region
         new-bindings)
        (cl-pushnew ; 等同于evil multiedit 中 R 操
         '("e" (lambda ()
                 (interactive)
                 (call-interactively
                  'evil-multiedit-match-all)))
         new-bindings)
        (cl-pushnew
         '("f" (lambda ()
                 (interactive)
                 (call-interactively
                  'find-file)))
         new-bindings)
        (cl-pushnew  ; 搜索选中的字符
         '("b" (lambda ()
                 (interactive)
                 (call-interactively
                  'consult-line)))
         new-bindings)
        (setq ad-return-value (cons new-msg new-bindings)))))


  (use-package j-expand-region
    :ensure nil)

  (use-package highlight-global
    :ensure nil
    :commands (highlight-frame-toggle)
    :quelpa (highlight-global :fetcher github :repo "glen-dai/highlight-global" :upgrade nil)
    :config
    ;; 高亮默认随机红、粉、蓝、青、紫等颜色循环
    (progn
      (setq-default highlight-faces
                    '(('hi-red-b . 0)
                      ('hi-aquamarine . 0)
                      ('hi-pink . 0)
                      ('hi-blue-b . 0)))))

  (use-package symbol-overlay
    :ensure t
    :config
    (define-key symbol-overlay-map (kbd "h") 'nil))

  (use-package j-highlight-global
    :ensure nil)

  (message "Load init-tools done...")
  (provide 'init-tools)
