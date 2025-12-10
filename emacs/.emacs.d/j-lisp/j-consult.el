;;; j-consult.el --- Extensions for Consult -*- lexical-binding: t -*-

;; Copyright (C) 2024  Jasper Hsu

;; Author: Jasper Hsu <xcwhome@163.com>
;; URL: https://xuchangwei.com/lisp/jasper-emacs.html
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I do not recommend that
;; you copy any of this if you are not certain of what it does.

;;; Code:

;;;; 配置搜索中文文件

(require 'consult)

;; PC提前安装 everyting 及其客户端ES. 利用M-x consult-locate搜索
;; 如 #学习#.txt
;; (progn
;;   (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
;;   (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
;;   )
  (if sys/win32p
      (progn
        (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
        (add-to-list 'process-coding-system-alist '("explorer" gbk . gbk))
        (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))))

;; make consult-ripgrep work 有时consult-ripgrep搜索不了
(cond
 ;; macOS
 ((eq system-type 'darwin)
  "afplay")
 ;; Windows
 ((eq system-type 'windows-nt)
  (add-to-list 'process-coding-system-alist 
               '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
  )
 (t
  "Nothing"))

;;;; 函数定义跳转
(defun my/imenu ()
  "跳转函数列表，当模式为org-mode跳转标题列表"
  (interactive)
  (if (eq major-mode #'org-mode)
      (call-interactively #'consult-org-heading)
    (call-interactively #'consult-imenu)))

(provide 'j-consult)
;;; j-consult.el ends here
