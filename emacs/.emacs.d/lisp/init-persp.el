  (use-package tab-bar
    :ensure nil
    :init
    (tab-bar-mode t)
    (setq tab-bar-new-tab-choice "*scratch*") ;; buffer to show in new tabs
    (setq tab-bar-close-button-show nil)      ;; hide tab close / X button
    (setq tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

    (custom-set-faces
     '(tab-bar ((t (:inherit mode-line))))
     '(tab-bar-tab ((t (:inherit mode-line :foreground "#993644"))))
     '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :foreground "black")))))

    (defvar ct/circle-numbers-alist
      '((0 . "⓪")
        (1 . "①")
        (2 . "②")
        (3 . "③")
        (4 . "④")
        (5 . "⑤")
        (6 . "⑥")
        (7 . "⑦")
        (8 . "⑧")
        (9 . "⑨"))
      "Alist of integers to strings of circled unicode numbers.")

    (defun ct/tab-bar-tab-name-format-default (tab i)
      (let ((current-p (eq (car tab) 'current-tab))
            (tab-num (if (and tab-bar-tab-hints (< i 10))
                         (alist-get i ct/circle-numbers-alist) "")))
        (propertize
         (concat tab-num
                 " "
                 (alist-get 'name tab)
                 (or (and tab-bar-close-button-show
                          (not (eq tab-bar-close-button-show
                                   (if current-p 'non-selected 'selected)))
                          tab-bar-close-button)
                     "")
                 " ")
         'face (funcall tab-bar-tab-face-function tab))))
    (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)
    (setq tab-bar-tab-hints t))

  (use-package tabspaces
    ;; use this next line only if you also use straight, otherwise ignore it.
    :ensure t
    :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
    :defer t
    :commands (tabspaces-switch-or-create-workspace
               tabspaces-open-or-create-project-and-workspace)
    :custom
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-default-tab "Default")
    (tabspaces-remove-to-default t)
    (tabspaces-include-buffers '("*scratch*"))
    ;; maybe slow 关闭emacs后自动恢复工作空间。怕影响性能可以注释掉
    ;;(tabspaces-session t)
    ;;(tabspaces-session-auto-restore t)
    :config
    ;; Filter Buffers for Consult-Buffer

    (with-eval-after-load 'consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult--source-buffer :hidden nil :default nil)
      ;; set consult-workspace buffer list
      (defvar consult--source-workspace
        (list :name "Workspace Buffers"
              :narrow ?w
              :history 'buffer-name-history
              :category 'buffer
              :state #'consult--buffer-state
              :default t
              :items (lambda () (consult--buffer-query
                                 :predicate #'tabspaces--local-buffer-p
                                 :sort 'visibility
                                 :as #'buffer-name)))

        "Set workspace buffer list for consult-buffer.")
      (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

  (message "Load init-persp done...")
  (provide 'init-persp)
