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

;; 关闭菜单栏
(menu-bar-mode -1)

;; 关闭滚动条
(scroll-bar-mode -1)

;; 关闭工具栏
(tool-bar-mode -1)

;; 设置字体
(set-face-attribute 'default nil :family "Consolas NF" :height 120)
(set-fontset-font t 'han (font-spec :family "新宋体" :height 120))

;; 设置默认的窗口列宽 100 列
(add-hook 'window-setup-hook
     (lambda ()
       (set-frame-width (selected-frame) 100)))

(provide 'theme-config)

;;; theme-config ends here 

