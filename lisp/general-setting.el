;;; general-setting --- 通用设置

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

;; 将 .gitignore 视为代码文件
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . prog-mode))

(provide 'general-setting)

;;; general-setting.el ends here

