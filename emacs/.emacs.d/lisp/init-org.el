;;; config

;;; 使用旧版快捷键<s +Tab
(with-eval-after-load 'org
  (require 'org-tempo))

;; 禁用左尖括号
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))))

;;; 安装 org，这个配置一定要配置在 use-package 的初始化之前，否则无法正常安装
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)
(use-package org
  :pin gnu-elpa
  :ensure t)

(use-package org-contrib  ;非org的官方贡献的插件
  :ensure t
  :pin nongnu)

;;; 自定义org todo  C-c C-t 
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

(require 'org-checklist)
;; need repeat task and properties
(setq org-log-done t)
(setq org-log-into-drawer t)











(message "Load init-org done...")
(provide 'init-org)
