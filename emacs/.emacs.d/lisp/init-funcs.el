;;(defun open-init-file()
;;  (interactive)
;;  (find-file "~/.emacs.d/init.el"))
;;
;;;; 这一行代码，将函数 open-init-file 绑定到 <f4> 键上
;;(global-set-key (kbd "<f4>") 'open-init-file)


;;; 快捷使用emacs
(use-package j-emacs-fast
  :ensure nil
  :demand t
  :diminish
  :bind 
  ( :map global-map
    ("<f4>" . fast/open-my-init-file) ; 快速打开配置文件
    ))

(message "Load init-funcs done...")
(provide 'init-funcs)
