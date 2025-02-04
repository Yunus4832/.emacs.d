;;; appearance.el --- 外观配置

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

;; 使用 awesome-tab
(require 'awesome-tab)
(awesome-tab-mode t)
(setq awesome-tab-show-tab-index t)
(setq awesome-tab-height 120)
(setq awesome-tab-display-icon nil)
(defun awesome-tab-hide-tab (buffer-name)
  "Which buffer will be hide, BUFFER-NAME is the tab buffer name to show."
  (let ((name (format "%s" buffer-name)))
    (or
     (string-match "Treemacs" name)
     (string-match "dashboard" name)
     (string-match "message" name)
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (and (string-prefix-p "magit" name)
               (not (file-name-extension name)))
     )))

;; 设置字体
(set-face-attribute 'default nil :family "Consolas NF" :height 120)
(set-fontset-font t 'han (font-spec :family "新宋体" :height 120))

;; 代码模式和文本模式显示相对行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defvar display-line-numbers-type 'relative)

(provide 'appearance)

;;; appearance.el ends here

