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

;; evil-escape 用于设置退出 insert 模式的按键映射
(use-package evil-escape
  :after evil-collection
  :config
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.5)
  (evil-escape-mode 1))

(provide 'vim-emulator)

;;; vim-emulator.el ends here

