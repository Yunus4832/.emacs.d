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

(provide 'theme-config)

;;; theme-config ends here 

