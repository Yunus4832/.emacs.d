;;; init.el --- Emacs 启动配置文件  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

;; 初始化包管理器
(package-initialize)

;; 设置软件源
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable-melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; 关闭软件包签名验证
(setq package-check-signature nil)

;; 如果 use-package 没有安装，则安装 use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package 配置
(require 'use-package)
(setq use-package-always-ensure t)

;; 自定义配置文件
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

;; 加载 lisp 目录下的所有 el 文件
(let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path lisp-dir)
  (mapc 'load (directory-files lisp-dir t "\\.el$")))

;; 加载键映射
(defvar key-mapping-file "~/.emacs.d/key-mapping.el")
(unless (file-exists-p key-mapping-file)
  (write-region "" nil key-mapping-file))

(load key-mapping-file)

(provide 'init)

;;; init.el ends here
