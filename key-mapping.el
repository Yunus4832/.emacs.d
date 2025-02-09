;;; key-mapping.el --- 按键映射配置

;;; Commentary:

;;; Code:

(require 'use-package)

;; 使用 general 进行按键映射
(use-package general
  :config
  (general-create-definer my-leader-def :prefix "SPC") ;; 设置 Leader 键为空格
  ;; leader 按键绑定
  ;; 文件浏览器
  (my-leader-def
    :states 'normal
    "f" 'treemacs-select-window)  ;; 绑定 SPC f 到 treemacs-select-window
  ;; IDE 功能
  (my-leader-def
    :states 'normal
    ";e" (lambda () (interactive) (delete-other-windows) (dashboard-open))) ;; 打开最近访问文件列表
  (my-leader-def
    :states 'normal
    ";s" 'helm-projectile) ;; 搜索项目文件
  (my-leader-def
    :states 'normal
    ";f" 'format-all-buffer) ;; 格式化代码
  ;; 版本控制
  (my-leader-def
    :states 'normal
    "vn" 'git-gutter:next-hunk) ;; 跳转到下一个 Hunk
  (my-leader-def
    :states 'normal
    "vp" 'git-gutter:previous-hunk) ;; 跳转到上一个 Hunk
  (my-leader-def
    :states 'normal
    "vr" 'git-gutter:revert-hunk) ;; 回滚当前 Hunk
  ;; Buffer 切换
  (my-leader-def
    :states 'normal
    "bn" 'awesome-tab-forward-tab) ;; 下一个 Buffer
  (my-leader-def
    :states 'normal
    "bp" 'awesome-tab-backward-tab) ;; 上一个 Buffer
  (my-leader-def
    :states 'normal
    "bb" 'evil-switch-to-windows-last-buffer) ;; 最近访问的 Buffer
  (my-leader-def
    :states 'normal
    "bd" 'evil-delete-buffer) ;; 删除当前 Buffer
  (my-leader-def
    :states 'normal
    "bl" 'helm-buffers-list) ;; buffer 列表
)

;; 使用 helm-M-x 替代 M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;;; key-mapping.el ends here
