;;; terminal.el --- 终端配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 终端工具
(use-package multi-term
  :ensure t
  :custom
  (multi-term-dedicated-select-after-open-p t)
  )

(provide 'terminal)

;;; project-management.el ends here
