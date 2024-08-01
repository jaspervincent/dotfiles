;;; j-org-download.el --- Extensions for org -*- lexical-binding: t -*-

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

;;;; 截图
;; 将剪切板中内容粘贴到org文件中

;;;###autoload
(defun my/org-insert-clipboard-image (width)
  "保存目录 `foldername'
图片名称 `imgName'
图片显示与关闭 `org-redisplay-inline-images'
图片的质量跟是否采用压缩有关"
  (interactive (list
                (read-string (format "Input image width, default is 800: ")
                             nil nil "800")))
  ;; 设置图片存放的文件夹位置为 `当前Org文件同名.assets'
  (setq foldername "./images/")
  (if (not (file-exists-p foldername))
      (mkdir foldername))
  ;; 设置图片的文件名，格式为 `img_年月日_时分秒.png'
  (setq srcImgName (concat "img_" (format-time-string "%Y%m%d_%H%M%S") "_src.png"))
  (setq imgName (concat "img_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
  ;; 图片文件的相对路径
  (setq srcRelativeFilename (concat "./images/" srcImgName))
  (setq relativeFilename (concat "./images/" imgName))
  ;; 根据不同的操作系统设置不同的命令行工具
  (cond ((string-equal system-type "gnu/linux")
         (shell-command (concat "xclip -selection clipboard -t image/png -o > " relativeFilename)))
        ((string-equal system-type "darwin")
         (shell-command (concat "pngpaste " relativeFilename)))
        ((string-equal system-type "windows-nt")
         ;; 原 (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"relativeFilename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))))
         ;; 简单压缩 (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; if ($([System.Windows.Forms.Clipboard]::ContainsImage())) { $image = [System.Windows.Forms.Clipboard]::GetImage(); $image.Save('"srcRelativeFilename "', [System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'Clipboard content saved as file'; & ffmpeg -i "srcRelativeFilename " -q:v 4 -loglevel quiet -y "relativeFilename " } else { Write-Output 'Clipboard does not contain image data' }\""))))
         ;; (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; if ($([System.Windows.Forms.Clipboard]::ContainsImage())) { $image = [System.Windows.Forms.Clipboard]::GetImage(); $image.Save('"srcRelativeFilename "', [System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'Clipboard content saved as file'; & ffmpeg -i "srcRelativeFilename " -q:v 4 -loglevel quiet -y "relativeFilename " ;& Remove-Item -Path "srcRelativeFilename" -Force} else { Write-Output 'Clipboard does not contain image data' }\""))))
         ;;调色板压缩 (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; if ($([System.Windows.Forms.Clipboard]::ContainsImage())) { $image = [System.Windows.Forms.Clipboard]::GetImage(); $image.Save('"srcRelativeFilename "', [System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'Clipboard content saved as file'; ffmpeg -i "srcRelativeFilename " -vf 'palettegen=max_colors=256:stats_mode=single' -loglevel quiet -y  out_3.png ; ffmpeg -i "srcRelativeFilename " -i out_3.png -lavfi '[0][1:v] paletteuse' -pix_fmt pal8 -loglevel quiet -y "relativeFilename "} else { Write-Output 'Clipboard does not contain image data' }\""))))
         (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; if ($([System.Windows.Forms.Clipboard]::ContainsImage())) { $image = [System.Windows.Forms.Clipboard]::GetImage(); $image.Save('"srcRelativeFilename "', [System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'Clipboard content saved as file'; ffmpeg -i "srcRelativeFilename " -vf 'palettegen=max_colors=256:stats_mode=single' -loglevel quiet -y  out_3.png ; ffmpeg -i "srcRelativeFilename " -i out_3.png -lavfi '[0][1:v] paletteuse' -pix_fmt pal8 -loglevel quiet -y "relativeFilename "; Remove-Item -Path "srcRelativeFilename" -Force ; Remove-Item -Path out_3.png  -Force} else { Write-Output 'Clipboard does not contain image data' }\""))))
  ;; 给粘贴好的图片链接加上宽度属性，方便导出
  (insert (concat "\n#+DOWNLOADED: screenshot @ "
                  (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time))
                  "\n#+CAPTION: \n#+ATTR_ORG: :width "
                  width
                  "\n#+ATTR_LATEX: :width "
                  (if (>= (/ (string-to-number width) 800.0) 1.0)
                      "1.0"
                    (number-to-string (/ (string-to-number width) 800.0)))
                  "\\linewidth :float nil\n"
                  "#+ATTR_HTML: :width "
                  width
                  "\n[[file:" relativeFilename "]]\n"))
  ;; 重新显示一下图片
  (org-redisplay-inline-images)
  (message "OKKKKKK")
  )

;;(global-set-key (kbd "<f2>") 'my/org-insert-clipboard-image)


(provide 'j-org-download)
;;; j-org-download.el ends here
