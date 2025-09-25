;;; early-init.el --- GUI 启动前配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; 关闭菜单栏
(menu-bar-mode -1)

;; 关闭滚动条
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; 关闭工具栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; 设置默认的窗口列宽 100 列, 背景黑色，前景白色
(setq default-frame-alist '((width . 100)
                            (background-color . "#272822")
                            (foreground-color . "white")))

;;; early-init.el ends here
