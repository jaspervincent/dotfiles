(message "Load init-evil done...")
(provide 'init-evil)

;;; evil 
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil) ;不使用自带的键位绑定
  (setq evil-want-C-u-scroll t) ;C-u 实现向上滚动。默认C-u 是emacs中的
  (evil-mode)

  ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)) ;如果有链接，按回车能访问这个链接
  )

;;; evil 使用undo-tree来管理undo redo
(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree))

(message "Load init-evil done...")
(provide 'init-evil)
