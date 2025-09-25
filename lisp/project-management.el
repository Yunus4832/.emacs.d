;;; project-management.el --- 项目管理  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 projectile 做项目管理
(use-package projectile
  :config
  (projectile-mode 1))

(provide 'project-management)

;;; project-management.el ends here
