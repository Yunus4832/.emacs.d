;;;; file-search.el --- 文件搜索配置

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 helm 搜索文件
(use-package helm
  :config
  (helm-mode 1)
)

;; 使用 helm-projectile
(use-package helm-projectile
  :if (functionp 'helm)
  :config
  (helm-projectile-on))

(provide 'file-search)

;;; file-search.el ends here
