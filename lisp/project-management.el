;;; project-management.el --- 项目管理

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 projectile 做项目管理
(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory))
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

(provide 'project-management)

;;; project-management.el ends here
