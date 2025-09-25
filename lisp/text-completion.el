;;; text-completion.el --- 文本补全

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 company 补全文本
(use-package company
  ;; 等价于 (add-hook 'after-init-hook #'global-company-mode)
  :hook (after-init . global-company-mode)
  :config
  ;; setq 可以像这样连着设置多个变量的值
  (setq company-tooltip-align-annotations t ; 注释贴右侧对齐
        company-tooltip-limit 20            ; 菜单里可选项数量
        company-show-quick-access t         ; 显示编号（然后可以用 M-数字 快速选定某一项）
        company-idle-delay .2               ; 延时多少秒后弹出
        company-minimum-prefix-length 1     ; 至少几个字符后开始补全
        )
  (defun company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	backends
      (append (if (consp backends)
		  backends
		(list backends))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-backend-with-yas company-backends))
  )

;; 代码片段引擎
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; 预定义的代码片段
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'text-completion)

;;; text-completion.el ends here
