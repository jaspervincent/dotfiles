  ;;; j-org.el --- Extensions for org -*- lexical-binding: t -*-

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

  ;;;; org-export

  (declare-function org-html-export-as-html "org")
  (declare-function org-texinfo-export-to-info "org")

  ;;;###autoload
  (defun j-org-ox-html ()
    "导出为html
  Streamline HTML export."
    (interactive)
    (org-html-export-as-html nil nil nil t nil))

  ;;;###autoload
  (defun j-org-ox-texinfo ()
    "导出Info
  Streamline Info export."
    (interactive)
    (org-texinfo-export-to-info))

  ;;;; org-id

  (declare-function org-id-add-location "org")
  (declare-function org-with-point-at "org")
  (declare-function org-entry-get "org")
  (declare-function org-id-new "org")
  (declare-function org-entry-put "org")

  ;; Original idea:
  ;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
  (defun j-org--id-get ()
    "获取当前的CUSTOM_ID
  Get the CUSTOM_ID of the current entry.
  If the entry already has a CUSTOM_ID, return it as-is, else
  create a new one."
    (let* ((pos (point))
           (id (org-entry-get pos "CUSTOM_ID")))
      (if (and id (stringp id) (string-match-p "\\S-" id))
          id
        (setq id (org-id-new "h"))
        (org-entry-put pos "CUSTOM_ID" id)
        id)))

  (declare-function org-map-entries "org")

  ;;;###autoload
  (defun j-org-id-headlines ()
    "添加CUSTOM_ID
  Add missing CUSTOM_ID to all headlines in current file."
    (interactive)
    (org-map-entries
     (lambda () (j-org--id-get))))

  ;;;###autoload
  (defun j-org-id-headline ()
    "Add missing CUSTOM_ID to headline at point."
    (interactive)
    (j-org--id-get))

  (provide 'j-org)
  ;;; j-org.el ends here
