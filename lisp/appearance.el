;;; theme-config.el --- 外观配置

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 Monokai 主题
(use-package monokai-theme
    :config
    (load-theme 'monokai t))

;; 使用 dashboard 启动页
(use-package dashboard
    :config
    (dashboard-setup-startup-hook))

;; 使用 Powerline 状态栏
(use-package powerline
    :config
    (powerline-default-theme))

;; 设置字体
(set-face-attribute 'default nil :family "Consolas NF" :height 120)
(set-fontset-font t 'han (font-spec :family "新宋体" :height 120))

;; 代码模式显示相对行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(provide 'theme-config)

;;; theme-config ends here 

