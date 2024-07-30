  ;;; j-evil.el --- Extensions for evil -*- lexical-binding: t -*-

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

  ;;;###autoload
  (defun my/evil-quick-replace (beg end )
    "交互式替换文本"
    (interactive "r")
    (when (evil-visual-state-p)
      (evil-exit-visual-state)
      (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
        (setq command-string (format "%%s /%s//g" selection))
        (minibuffer-with-setup-hook
            (lambda () (backward-char 2))
          (evil-ex command-string)))))

  ;;(define-key evil-visual-state-map (kbd "C-r") 'my/evil-quick-replace)

  (provide 'j-evil)
  ;;; j-evil.el ends here
