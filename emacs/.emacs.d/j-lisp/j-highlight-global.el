;;; j-highlight-global.el --- Extensions for highlight-global -*- lexical-binding: t -*-

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

;;; 选中文本高亮，general插件中做了键位绑定

;;;###autoload
(defun my/highlight-dwim ()
  "do what i mean 如果选中区域调用highlight-frame-toggle高亮,
没有则调用symbol-overlay高亮"
  (interactive)
  (if (use-region-p)
      (progn
        (highlight-frame-toggle)
        (deactivate-mark))
    (symbol-overlay-put)))

;;;###autoload
(defun my/clearn-highlight ()
  (interactive)
  (clear-highlight-frame)
  (symbol-overlay-remove-all))

(provide 'j-highlight-global)
;;; j-highlight-global.el ends here
