  ;;; 切换窗口
  (use-package window-numbering
    :ensure t
    :init
    :hook (after-init . window-numbering-mode))

  ;;; 可以交换窗口、删除窗口、分屏
  (use-package es-windows
    :ensure t)

  ;;; config
  (use-package buffer-move
    :ensure t)

  ;;; 调整窗口大小
  (use-package resize-window
    :ensure t
    :init
    (defvar resize-window-dispatch-alist
      '((?n resize-window--enlarge-down " 向下Resize - Expand down " t)
        (?p resize-window--enlarge-up " 向上Resize - Expand up" t)
        (?f resize-window--enlarge-horizontally " 向右Resize - horizontally" t)
        (?b resize-window--shrink-horizontally " 向左Resize - shrink horizontally" t)
        (?r resize-window--reset-windows " 重置Resize - reset window layout" nil)
        (?w resize-window--cycle-window-positive "  选择分屏Resize - cycle window" nil)
        (?W resize-window--cycle-window-negative " Resize - cycle window" nil)
        (?2 split-window-below " 水平Split window horizontally" nil)
        (?3 split-window-right " 垂直Slit window vertically" nil)
        (?0 resize-window--delete-window " 删除Delete window" nil)
        (?K resize-window--kill-other-windows " Kill other windows (save state)" nil)
        (?y resize-window--restore-windows " 恢复(when state) Restore window configuration" nil)
        (?? resize-window--display-menu " Resize - display menu" nil))
      "List of actions for `resize-window-dispatch-default.
  Main data structure of the dispatcher with the form:
  \(char function documentation match-capitals\)"))

  ;;; 内置插件。用来切换2个不同窗口布局
  (use-package winner
    :ensure nil
    :commands (winner-undo winner-redo)
    :hook (after-init . winner-mode)
    :init (setq winner-boring-buffers '("*Completions*"
                                        "*Compile-Log*"
                                        "*inferior-lisp*"
                                        "*Fuzzy Completions*"
                                        "*Apropos*"
                                        "*Help*"
                                        "*cvs*"
                                        "*Buffer List*"
                                        "*Ibuffer*"
                                        "*esh command on file*")))

  ;; Enforce rules for popups
  (use-package popper
    :ensure t
    :defines popper-echo-dispatch-actions
    :commands popper-group-by-directory
    :bind (:map popper-mode-map
                ("s-`" . popper-toggle-latest)
                ("s-o"   . popper-cycle)
                ("M-`" . popper-toggle-type))
    :hook (emacs-startup . popper-mode)
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "\\*Compile-Log\\*"
            "\\*Completions\\*"
            "\\*Warnings\\*"
            "\\*Flymake diagnostics.*\\*"
            "\\*Async Shell Command\\*"
            "\\*Apropos\\*"
            "\\*Backtrace\\*"
            "\\*prodigy\\*"
            "\\*Calendar\\*"
            "\\*Embark Actions\\*"
            "\\*Finder\\*"
            "\\*Kill Ring\\*"
            "\\*Embark Export:.*\\*"
            "\\*Edit Annotation.*\\*"
            "\\*Flutter\\*"
            bookmark-bmenu-mode
            comint-mode
            compilation-mode
            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode
            occur-mode
            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            ivy-occur-mode ivy-occur-grep-mode
            process-menu-mode list-environment-mode cargo-process-mode
            youdao-dictionary-mode osx-dictionary-mode fanyi-mode

            "^\\*eshell.*\\*.*$" eshell-mode
            "^\\*shell.*\\*.*$"  shell-mode
            "^\\*terminal.*\\*.*$" term-mode
            "^\\*vterm.*\\*.*$"  vterm-mode

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\*$"
            "\\*eldoc\\*"
            "^\\*elfeed-entry\\*$"
            "^\\*macro expansion\\**"

            "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode))

    (when (display-grayscale-p)
      (setq popper-mode-line
            '(:eval
              (concat
               (propertize " " 'face 'mode-line-emphasis)
               (propertize " " 'face 'mode-line-emphasis)))))

    (setq popper-echo-dispatch-actions t)
    (setq popper-group-function nil)
    :config
    (popper-echo-mode 1)

    (with-no-warnings
      (defun my-popper-fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))
      (setq popper-window-height #'my-popper-fit-window-height)

      (defun popper-close-window-hack (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

  (message "Load init-window done...")
  (provide 'init-window)
