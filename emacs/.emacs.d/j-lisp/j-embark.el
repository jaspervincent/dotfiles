;;; j-embark.el --- Extensions for Embark -*- lexical-binding: t -*-

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

(require 'embark)


;;;; 批量改写; 配合query-replace-regexp实现文本替换
;; 范例：批量替换操作
;; - ~M-x consult-ripgrep~  默认会在以 git 为根目录搜索。如 =#hello= ，搜索包含hello字符的文件。 ~C-n/C-p~ 下上搜索同时可以预览。
;; - ~C-c C-e~ 打开写
;; - ~M-x query-replace-regexp~ 输入hello 回车， 替换为hello 回车。 按 y 同意当前行替换，n 不同意修改
;; - ~C-c C-c~ 执行替换，按 q 退出
;; 
;; 范例：当前buffer替换操作
;; - =C-s= 搜索， 输入hello
;; - =C-c C-e= 编辑
;; - ~M-x query-replace-regexp~ 输入要替换的内容
;; - ~C-c C-c~ 执行替换，按 q 退出
(defun my/embark-export-write()
 "Export the current vertico results to a writable buffer if
possible.Supports exporting
consult-grep to wgrep, file to
wdeired, and consult-location to occur-edit."
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))



;;;; 使用 Emacs 打开windows文件管理器
;; 函数在embark-file-map 映射为E
;; (define-key embark-file-map (kbd "E") #'consult-directory-externally)
;; 范例: 快速打开文件所在目录
;; - =M-x my/consult-directory-externally= 输入文件路径，如 =~/.emacs.d/auto-save-list/.saves-2000-JASPER~=
;; - =C-x C-f= 选择文件时，可以用embark来打开， =C;= 选择E，用文件管理器打开对应目录的
(defun my/consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\\\\\"
      ;;(shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
            (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (file-name-directory (expand-file-name file)))))

;;打开当前文件的目录
(defun my/open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

(provide 'j-embark)
;;; j-embark.el ends here
