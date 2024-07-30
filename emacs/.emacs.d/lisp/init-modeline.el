  ;;; config

  (use-package keycast
    :ensure t
    :commands (+toggle-keycast)
    :config
    (defun +toggle-keycast()
      (interactive)
      (if (member '("" keycast-mode-line " ") global-mode-string)
          (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
                 (remove-hook 'pre-command-hook 'keycast--update)
                 (message "Keycast OFF"))
        (add-to-list 'global-mode-string '("" keycast-mode-line " "))
        (add-hook 'pre-command-hook 'keycast--update t)
        (message "Keycast ON"))))
    ;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后

  (message "Load init-modeline done...")
  (provide 'init-modeline)
