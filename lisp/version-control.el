;;; version-control.el --- 版本控制设置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; magit
(use-package magit)

;; treemacs 的 magit 扩展
(use-package treemacs-magit
  :after (treemacs magit))

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t)                                           ;; 全局启用 git-gutter
  (setq git-gutter:ask-p nil)                                          ;; 不需要询问
  (setq git-gutter:modified-sign "▋")                                  ;; 修改的行
  (setq git-gutter:added-sign "▋")                                     ;; 新增的行
  (setq git-gutter:deleted-sign "▋")                                   ;; 删除的行
  (setq git-gutter:update-interval 1)                                  ;; 更新间隔
  (set-face-foreground 'git-gutter:modified "blue")                    ;; 修改的行颜色
  (set-face-foreground 'git-gutter:added "green")                      ;; 新增的行颜色
  (set-face-foreground 'git-gutter:deleted "gray")                     ;; 删除的行颜色
  (set-face-background 'git-gutter:modified "blue")                    ;; 修改的行颜色
  (set-face-background 'git-gutter:added "green")                      ;; 新增的行颜色
  (set-face-background 'git-gutter:deleted "gray")                     ;; 删除的行颜色
  )

(provide 'version-control)

;;; version-control.el ends here
