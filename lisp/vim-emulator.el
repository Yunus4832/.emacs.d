;;; vim-emulator.el --- vim 模拟

;;; Commentary:

;;; Code:

(require 'use-package)

;; 安装 evil 模拟软件包
(use-package evil
    :init
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1))

;; Evil 补充
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'vim-emulator)

;;; vim-emulator.el ends here

