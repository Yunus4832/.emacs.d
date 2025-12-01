;;; lite-eamcs.el --- Emacs 最小启动配置文件，不依赖第三方包  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

;; 关闭菜单栏
(menu-bar-mode 0)

;; 关闭滚动条
(scroll-bar-mode -1)

;; 关闭工具栏
(tool-bar-mode -1)

;; 启用 Emacs 内置补全 ido
(ido-mode 1)
(ido-everywhere 1)

;; 初始化包管理器
(package-initialize)

;; 设置软件源
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable-melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; 关闭软件包签名验证
(setq package-check-signature nil)

;; 关闭自动保存
(setq auto-save-default nil)

;; 关闭备份文件
(setq make-backup-files nil)

;; 关闭版本控制备份
(setq version-control nil)

;; 开启全局本地文件自动保存
(auto-save-visited-mode t)

;; 将 .gitignore 视为代码文件
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . prog-mode))

;; 帮助窗口自动获取焦点
(setq help-window-select t)

;; 默认退出后台进程
(setq confirm-kill-processes nil)

;; 默认开启成对符号匹配
(setq-default electric-pair-mode t)
(electric-pair-mode 1)

;; 默认关闭软折行
(setq-default truncate-lines 1)

(setq default-file-name-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; viper mode Vi 模拟器
(setq viper-mode 't)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)
(require 'viper)
(viper-mode)

;; 代码模式和文本模式显示相对行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(defvar display-line-numbers-type 'relative)

;; 设置括号匹配的高亮显示
(setq show-paren-highlight-openparen nil)
(set-face-attribute 'show-paren-match nil :background "green" :foreground (face-attribute 'default :background nil))

;; 取消开始页
(setq inhibit-startup-screen t)

;; 按键映射
(eval-after-load 'viper
  '(progn
     (define-key viper-vi-global-user-map (kbd "g g") 'beginning-of-buffer)
     (define-key viper-vi-global-user-map (kbd "SPC f") 'dired)
     (define-key viper-vi-global-user-map (kbd "SPC c o") (lambda () (interactive)
                                (let ((buf (get-buffer-create "*compilation*")))
                                  (with-current-buffer buf
                                (unless (eq major-mode 'compilation-mode)
                                  (compilation-mode)))
                                  (display-buffer buf 'display-buffer-at-bottom))))
     (define-key viper-vi-global-user-map (kbd "SPC cc") (lambda () (interactive)
                               (let ((win (get-buffer-window "*compilation*")))
                                 (when win
                                   (quit-window nil win)))))
     (define-key viper-vi-global-user-map (kbd "SPC c n") 'next-error)
     (define-key viper-vi-global-user-map (kbd "SPC c p") 'previous-error)
     (define-key viper-vi-global-user-map (kbd "SPC c m") 'compile)
     (define-key viper-emacs-global-user-map (kbd "C-h") 'help-command)
     (define-key viper-insert-global-user-map (kbd "C-h") 'help-command)
     (define-key viper-vi-global-user-map (kbd "C-h") 'help-command)))

;;; lite-emacs.el ends here
