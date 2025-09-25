;;; jump.el --- 代码内跳转  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 avy 跳转
(use-package avy
  :config
  (setq avy-background t              ;; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t             ;; 搜索所有 window，即所有「可视范围」
        avy-timeout-seconds 0.3))     ;; 「关键字输入完毕」信号的触发时间

;;; jump.el ends here

