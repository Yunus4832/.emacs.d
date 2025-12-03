;;;; file-search.el --- 文件搜索配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; ivy 搜索补全框架
(use-package ivy
  :ensure t
  :config
  (ivy-mode))

;; 内容搜索
(use-package counsel
  :ensure t
  :config
  (counsel-mode))

;; 使用 ivy 渐进式搜索
(use-package swiper)

(provide 'file-search)

;;; file-search.el ends here
