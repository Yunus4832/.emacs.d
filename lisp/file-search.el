;;;; file-search.el --- 文件搜索配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 helm 搜索文件
(use-package helm
  :config
  (setq helm-echo-input-in-header-line t)
  (setq helm-display-function 'helm-display-buffer-in-own-frame)
  (helm-mode 1)
  )

;; 使用 helm-projectile
(use-package helm-projectile
  :if (functionp 'helm)
  :config
  (helm-projectile-on))

(provide 'file-search)

;;; file-search.el ends here
