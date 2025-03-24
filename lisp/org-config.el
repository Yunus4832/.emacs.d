;;; org-config.el --- org-mode 的配置

;;; Commentary:

;;; Code:

(require 'use-package)

;; 设置 Agenda 目录
(setq org-agenda-files '("~/.emacs.d/org/"))

;; 表格中文对齐
;; (use-package cnfonts
;;   :config
;;   (cnfonts-mode 1))
(use-package valign
  :config
  (add-hook 'org-mode-hook #'valign-mode)
  (setq valign-fancy-bar 1))

;; 打开文件链接独占框架
(setq org-link-frame-setup
      `((file . (lambda (path)
		  (find-file path)
		  (delete-other-windows)))))


;;; org-config.el ends here
