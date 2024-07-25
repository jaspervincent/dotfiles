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


(use-package pyim
  :ensure t)

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
  :config
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
      (cl-pushnew
       '("H" (lambda ()
               (interactive)
               (call-interactively
                'zilongshanren/highlight-dwim)))
       new-bindings)
      (cl-pushnew
       '("/" (lambda ()
               (interactive)
               (call-interactively
                'my/search-project-for-symbol-at-point)))
       new-bindings)
      (cl-pushnew
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
      (cl-pushnew
       '("b" (lambda ()
               (interactive)
               (call-interactively
                'consult-line)))
       new-bindings)
      (setq ad-return-value (cons new-msg new-bindings)))))

(use-package highlight-global
  :ensure nil
  :commands (highlight-frame-toggle)
  :quelpa (highlight-global :fetcher github :repo "glen-dai/highlight-global")
  :config
  (progn
    (setq-default highlight-faces
                  '(('hi-red-b . 0)
                    ('hi-aquamarine . 0)
                    ('hi-pink . 0)
                    ('hi-blue-b . 0)))))


(use-package symbol-overlay
  :config
  (define-key symbol-overlay-map (kbd "h") 'nil))

(message "Load init-tools done...")
(provide 'init-tools)
