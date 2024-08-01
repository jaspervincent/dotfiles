;;; j-emacs-fast.el --- Extensions for org -*- lexical-binding: t -*-

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

;;;; 放置一些快捷使用emacs函数

;;; Dos2Unix/Unix2Dos
(defun fast/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun fast/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


;;; 文件操作
(defun fast/open-my-init-file()
  "快速打开主配置文件"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory )))

(defun fast/copy-file-name ()
  "复制文件绝对路径，使用 =C-y= 粘贴
Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (warn "Current buffer is not attached to a file!")))

;;; buffer缓冲区
(defun fast/copy-buffer-name ()
  "复制缓冲区名
Copy name of the current buffer."
  (interactive)
  (kill-new (buffer-name)))

(defun fast/kill-other-buffers ()
  "杀掉其他缓冲区
Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

;;; 返回上一个buffer
(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window.
If `spacemacs-layouts-restrict-spc-tab' is `t' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(provide 'j-emacs-fast)
;;; j-emacs-fast.el ends here
