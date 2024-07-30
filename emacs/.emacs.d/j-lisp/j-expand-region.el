  ;;; j-expand-region.el --- Extensions for expand-region -*- lexical-binding: t -*-

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

  (require 'expand-region)

  ;;;###autoload
  (defun my/search-project-for-symbol-at-point ()
    "查找当前项目(git项目)目录中匹配选中字符串的文件"
    (interactive)
    (if (use-region-p)
        (progn
          (consult-ripgrep (project-root (project-current))
                           (buffer-substring (region-beginning) (region-end))))))

  (provide 'j-expand-region)
  ;;; j-expand-region.el ends here
