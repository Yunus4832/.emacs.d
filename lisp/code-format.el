;;; code-format.el --- 代码格式化

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 format-all 格式化代码
(use-package format-all)

;; 启用全局模式（可选）
(add-hook 'prog-mode-hook 'format-all-mode)

;;; code-format.el ends here
