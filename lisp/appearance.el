;;; appearance.el --- 外观配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 Monokai 主题
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; 使用 dashboard 启动页
(use-package dashboard
  :if (< (length command-line-args) 2)
  :config
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((projects . 5)         ;; 最近文件显示10条
                          (recents . 10)         ;; 书签显示5条
                          (bookmarks . 5)))      ;; 项目显示5条
  (dashboard-setup-startup-hook))

;; powerline 简化主题
(defun powerline-simplify-theme ()
  "Setup the simplify mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list ;; (powerline-raw "%*" face0 'l)
                                ;; (when powerline-display-buffer-size
                                ;;   (powerline-buffer-size face0 'l))
                                ;; (when powerline-display-mule-info
                                ;;   (powerline-raw mode-line-mule-info face0 'l))
                                (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                (when (and (boundp 'which-func-mode) which-func-mode)
                                  (powerline-raw which-func-format face0 'l))
                                (powerline-raw " " face0)
                                (funcall separator-left face0 face1)
                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (powerline-major-mode face1 'l)
                                ;; (powerline-process face1)
                                ;; (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)
                                (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                (powerline-vc face2 'r)
                                (when (bound-and-true-p nyan-mode)
                                  (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face0 face2))
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

;; 使用 Powerline 状态栏
(use-package powerline
  :config
  (powerline-simplify-theme))

;; 使用 awesome-tab
(use-package awesome-tab
  :load-path "third-single-package"
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-show-tab-index t)
  (setq awesome-tab-height 120)
  (setq awesome-tab-display-icon nil)
  (defun awesome-tab-hide-tab (buffer-name)
    "Which buffer will be hide, BUFFER-NAME is the tab buffer name to show."
    (let ((name (format "%s" buffer-name)))
      (or
       (string-match "^\\*.*\\*$" name)
       (string-match "Treemacs" name)
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "magit" name)))))

;; 设置字体
(when (window-system)
  (set-face-attribute 'default nil :family "Consolas NF" :height 120)
  (set-fontset-font t 'han (font-spec :family "新宋体" :height 120)))

;; 代码模式和文本模式显示行号
(dolist (line-number-hook '(prog-mode-hook
                            text-mode-hook
                            conf-mode-hook
                            lisp-interaction-mode-hook))
  (add-hook line-number-hook 'display-line-numbers-mode))

;; 所有打开的文件都显示行号
(defun my-line-numbers-for-files ()
  (when buffer-file-name
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook #'my-line-numbers-for-files)

;; 使用相对行号
(defvar display-line-numbers-type 'relative)

;; 设置空白符的显示样式
(setq whitespace-style
      '(face
        spaces
        tabs
        newline
        trailing
        leading
        space-mark
        tab-mark
        ))

;; 全局启用空白字符展示
(global-whitespace-mode 1)

;; 设置括号匹配的高亮显示
(setq show-paren-highlight-openparen nil)
(set-face-attribute 'show-paren-match nil :background "green" :foreground (face-attribute 'default :background nil))

;; 帮助窗口显示在右侧
(setq display-buffer-alist
      (cons '("\\*Help\\*"
              (display-buffer-in-side-window)
              (side . right)
              (slot . 0)
              (window-width . 80))
            display-buffer-alist))

(provide 'appearance)

;;; appearance.el ends here
