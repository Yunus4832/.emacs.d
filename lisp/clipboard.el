;;; clipboard.el --- 剪切板配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 终端模式下，复制内容同步到系统剪切板
(use-package clipetty
  :if (not window-system)
  :ensure t
  :hook (after-init . global-clipetty-mode))

;;; clipboard.el ends here
