;;; 窗口标签
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tab/"))
;;(require 'awesome-tab)
;;(awesome-tab-mode t)

(defun awesome-tab-buffer-groups ()
"`awesome-tab-buffer-groups' control buffers' group rules.
Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
(list
(cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
        (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode)))
    "Emacs")
    ((derived-mode-p 'eshell-mode)
    "EShell")
    ((derived-mode-p 'dired-mode)
    "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
    "OrgMode")
    ((derived-mode-p 'eaf-mode)
    "EAF")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

;;; 使用拼音进行搜索
;;ivy
;; Encoding
;; UTF-8 as the default coding system
;;(when (fboundp 'set-charset-priority)
;;  (set-charset-priority 'unicode))

;;(set-language-environment 'chinese-gbk)
;;(prefer-coding-system 'utf-8-auto)


(package-install 'pyim)

(defun eh-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
    (pyim-cregexp-build result)))


(defun toggle-chinese-search ()
  (interactive)
  (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
      (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
    (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

(defun disable-py-search (&optional args)
  (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

;; (advice-add 'exit-minibuffer :after #'disable-py-search)
(add-hook 'minibuffer-exit-hook 'disable-py-search) ;退出minibuffer时自动退出拼音搜索

(global-set-key (kbd "s-p") 'toggle-chinese-search) ;需要时打开拼音搜索。因为拼音搜索性能不稳定

;;; modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :ensure t
  :init (keycast-mode-line-mode 1)) ; 在标题显示

(message "Load init-tools done...")
(provide 'init-tools)
