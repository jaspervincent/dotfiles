;;; j-simple.el --- Extensions for Consult -*- lexical-binding: t -*-

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

;;; Commands

;;;; General commands

(defun j-simple--mark (bounds)
  "Mark between BOUNDS as a cons cell of beginning and end positions."
  (push-mark (car bounds))
  (goto-char (cdr bounds))
  (activate-mark))

;;; autoload这是一个文档字符串的前缀，表示这个函数可以通过 Emacs 的自动加载机制被加载。当用户在 Emacs 命令行中输入函数名时，Emacs 会尝试自动加载这个函数的定义。
;;;###autoload
(defun j-simple-mark-sexp ()
  "Mark symbolic expression at or near point.
Repeat to extend the region forward to the next symbolic
expression. 选中区域"
  (interactive)
  (if (and (region-active-p)
           (eq last-command this-command))
      (ignore-errors (forward-sexp 1))
    (when-let ((thing (cond
                       ((thing-at-point 'url) 'url)
                       ((thing-at-point 'sexp) 'sexp)
                       ((thing-at-point 'string) 'string)
                       ((thing-at-point 'word) 'word))))
      (j-simple--mark (bounds-of-thing-at-point thing)))))

;;;###autoload
(defun j-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(provide 'j-simple)
;;; j-simple.el ends here
