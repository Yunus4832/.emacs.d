;;; file-check.el --- 文件检查

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 flycheck
(use-package flycheck
  :defer t
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

(provide 'file-check)

;;; file-check.el ends here
