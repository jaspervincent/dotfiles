  ;;; config

  ;;(use-package quelpa :ensure t)
  (use-package quelpa
    :ensure t
    :commands quelpa
    :config
    :custom
    (quelpa-git-clone-depth 1)
    (quelpa-update-melpa-p nil)
    (quelpa-self-upgrade-p nil)
    (quelpa-checkout-melpa-p nil))
  (use-package quelpa-use-package
    :ensure t
    :init
    (setq quelpa-use-package-inhibit-loading-quelpa t)
    :demand t)

  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git")))

  (message "Load init-packages done...")
  (provide 'init-packages)
