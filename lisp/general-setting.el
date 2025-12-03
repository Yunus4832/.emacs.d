;;; general-setting --- 通用设置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; 关闭自动保存
(setq auto-save-default nil)

;; 关闭备份文件
(setq make-backup-files nil)

;; 关闭版本控制备份
(setq version-control nil)

;; 开启全局本地文件自动保存
(auto-save-visited-mode t)

;; 帮助窗口自动获取焦点
(setq help-window-select t)

;; 默认退出后台进程
(setq confirm-kill-processes nil)

;; 默认开启成对符号匹配
(setq-default electric-pair-mode t)
(electric-pair-mode 1)

;; tab 显示的空格数量
(setq-default tab-width 4)

;; 默认关闭软折行
(setq-default truncate-lines t)

;; 文件自定义钩子
(dolist (file-custom-hook '(prog-mode-hook
                            text-mode-hook
                            conf-mode-hook
                            lisp-interaction-mode-hook))
  (add-hook file-custom-hook
            (lambda ()
              ;; 使用空格缩进代码
              (setq indent-tabs-mode nil))))

(setq default-file-name-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(provide 'general-setting)

;;; general-setting.el ends here
