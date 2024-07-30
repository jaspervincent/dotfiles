  ;;; c++ eglot
  (require 'eglot)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)

  ;; 运行任意单文件程序 快捷键F5
  (use-package quickrun
    :ensure t
    :commands (quickrun)
    :init
    (quickrun-add-command "c++/c1z"
      '((:command . "g++")
        (:exec . ("%c -std=c++1z %o -o %e %s"
                  "%e %a"))
        (:remove . ("%e")))
      :default "c++"))
  (global-set-key (kbd "<f5>") 'quickrun)

  ;;; treesit-auto
  ;; 语法高亮, *ts-mode , M-x treesit-auto-install-all
  (use-package treesit-auto
    :ensure t
    :demand t
    :config
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode))

  (use-package yasnippet
    :ensure t
    :hook ((prog-mode . yas-minor-mode)
           (org-mode . yas-minor-mode))
    :init
    :config
    (progn
      (setq hippie-expand-try-functions-list
            '(yas/hippie-try-expand
              try-complete-file-name-partially
              try-expand-all-abbrevs
              try-expand-dabbrev
              try-expand-dabbrev-all-buffers
              try-expand-dabbrev-from-kill
              try-complete-lisp-symbol-partially
              try-complete-lisp-symbol))))

  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet)

  (message "Load init-programming done...")
  (provide 'init-programming)
