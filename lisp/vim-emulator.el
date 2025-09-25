;;; vim-emulator.el --- vim 模拟  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 安装 evil 模拟软件包
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; treemacs 的 evil 扩展
(use-package treemacs-evil
  :defer 1
  :after (treemacs evil-collection)
  :config
  (evil-set-initial-state 'treemacs-mode 'normal)
  )

;; Evil 补充
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil-escape 用于设置退出 insert 模式的按键映射
(use-package evil-escape
  :load-path "third-single-package"
  :after evil-collection
  :config
  (setq evil-escape-key-sequence (list (kbd "jj") (kbd "kk")))
  (setq evil-escape-delay 0.5)
  (setq evil-escape-excluded-major-modes (list 'magit-status-mode 'magit-refs-mode 'magit-log-mode 'treemacs-mode))
  (push 'visual evil-escape-excluded-states)
  (evil-escape-mode 1))

(provide 'vim-emulator)

;;; vim-emulator.el ends here
