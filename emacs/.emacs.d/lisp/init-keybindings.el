;;; config

;;; 文件搜索
(global-set-key (kbd "C-c p f") 'project-find-file) ;;   查找文件，默认绑定在 C-x p f
(eval-after-load 'consult (global-set-key (kbd "C-c p s") 'consult-ripgrep))  ;;  查找文件内容

(message "Load init-keybindings done...")
(provide 'init-keybindings)
