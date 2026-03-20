;;; early-init.el --- GUI 启动前配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; 关闭菜单栏、工具栏和滚动条
(dolist (mode '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;; 设置默认的窗口属性: 列宽, 背景颜色, 前景颜色, 字体
(setq default-frame-alist
      '((width . 100)
        (background-color . "#272822")
        (foreground-color . "white")
        (font . "Consolas NF")))

;;; early-init.el ends here
