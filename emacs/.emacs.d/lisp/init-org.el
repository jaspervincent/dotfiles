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

(message "Load init-org done...")
(provide 'init-org)
