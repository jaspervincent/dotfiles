;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;; 判断操作系统， 定义一个新变量
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )

;;; 禁用备份和锁定文件
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;;; 指定自己义配置, 通过使它变成一次性的来禁用它
(setq custom-file (make-temp-file "emacs-custom-")) ;没有则自动创建emacs-custom-开头随机文件
;;(load custom-file 'no-error 'no-message)

;;; 始终从 *scratch* 缓冲区开始
(setq initial-buffer-choice t)

;;; 加载目录
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp" "j-lisp"))

;;; 软件包
(setq package-vc-register-as-project nil) ; Emacs 30

;; 设置插件源优先级
(require 'package)
(setq package-check-signature nil 
      load-prefer-newer t) ;; 个别时候会出现签名校验失败
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; 最大的数字优先（未提及的优先级为 0）
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(unless (bound-and-true-p package--initialized)
  (package-initialize)) ;; 刷新软件源索引

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar j-emacs-my-packages
  '(vertico tmr)
  "List of symbols representing the packages I develop/maintain.")

;; 指定某些软件使用特点源安装 
(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           j-emacs-my-packages)))

;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
;; (setq use-package-always-ensure t           ; 自动安装
;;      use-package-always-defer t            ; 软件包延迟加载 
;;      use-package-enable-imenu-support t
;;      use-package-expand-minimally t)

(defmacro prot-emacs-install (package &rest vc-args)
  "Prepare to install PACKAGE.
PACKAGE is an unquoted symbol, referring to the name of the package.  If
VC-ARGS are nil, then install PACKAGE using `package-install'.

If VC-ARGS is non-nil, then check if their `car' is a directory.  If it
is, apply `package-vc-install-from-checkout' on VC-ARGS, else apply
`package-vc-install'.

At all times, do nothing if PACKAGE is already installled."
  (declare (indent 0))
  (unless (symbolp package)
    (error "The package `%s' is not a symbol" package))
  (cond
   ((and package vc-args)
    (let ((fn (if-let* ((first (car vc-args))
                        (_ (and (stringp first) (file-directory-p first))))
                  'package-vc-install-from-checkout
                'package-vc-install)))
      `(unless (package-installed-p ',package)
         (condition-case-unless-debug err
             (apply #',fn ,vc-args)
           (error (message "Failed `%s' with `%S': `%S'" ',fn ,vc-args (cdr err)))))))
   (package
    `(progn
       (unless (package-installed-p ',package)
         (unless package-archive-contents
           (package-refresh-contents))
         (condition-case-unless-debug nil
             (package-install ',package)
           (error (message "Cannot install `%s'; try `M-x package-refresh-contents' first" ',package))))))))

(defmacro j-emacs-configure (&rest body)
  "Evaluate BODY and catch any errors."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     ((error user-error quit)
      (message "Failed to configure package starting with `%S' because of `%S'" (car ',body) (cdr err)))))

;; 在加载任何模块之前设置首选项
(load (locate-user-emacs-file "jasper-emacs-pre-custom.el") :no-error :no-message)

;; Packages
(require 'init-packages)
(use-package benchmark-init
  :ensure t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Core
(require 'init-ui)
(require 'init-essentials)
(require 'init-basic)
(require 'init-modeline)
(require 'init-completion)
(require 'init-evil)
(require 'init-tools)
;; uis
(require 'init-window)
;; Tools
(require 'init-org)
;; Frameworks
(require 'init-persp)
;; Languages
(require 'init-programming)
;; personal
(require 'init-keybindings)
(require 'init-funcs)

;; 加载完所有配置后的代码
(load (locate-user-emacs-file "jasper-emacs-post-custom.el") :no-error :no-message)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(setq gc-cons-threshold (* 2 1000 1000))
