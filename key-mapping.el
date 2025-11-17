;;; key-mapping.el --- 按键映射配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'treemacs)

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
    ";e" 'helm-recentf) ;; 打开最近访问文件列表
  (my-leader-def
    :states 'normal
    ";a" 'magit-blame-addition) ;; 打开 Git blame
  (my-leader-def
    :states 'normal
    ";s" 'helm-projectile) ;; 搜索项目文件
  (my-leader-def
    :states 'normal
    ";f" 'format-all-buffer) ;; 格式化代码
  (my-leader-def
    :states 'normal
    ";c" 'comment-line) ;; 注释代码
  (my-leader-def
    :states 'visual
    ";c" 'comment-dwim) ;; 注释代码
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
  (my-leader-def
    :states 'normal
    "vv" 'magit-status) ;; 打开 magit 工具窗口
  (my-leader-def
    :states 'normal
    "vc" 'magit-status) ;; 打开 magit 工具窗口
  (my-leader-def
    :states 'normal
    "vs" 'magit-push-to-remote) ;; 推送代码
  (my-leader-def
    :states 'normal
    "vu" 'magit-pull-branch) ;; 拉取和更新代码
  (my-leader-def
    :states 'normal
    "vl" 'magit-log-all) ;; 提交记录
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
  (my-leader-def
    :states 'normal
    "SPC s" 'avy-goto-char-timer) ;; buffer 内跳转
  ;; 临时开关
  (my-leader-def
    :states 'normal
    ",w" 'toggle-truncate-lines) ;; 开关软折行
  (my-leader-def
    :states 'normal
    ",," '(lambda () (interactive) (find-file user-init-file))) ;; 打开配置文件
  ;; 切换终端
  (general-def
    "M-=" 'multi-term-dedicated-toggle)  ;; 切换终端
  ;; 使用 helm-M-x 替代 M-x
  (general-def
    "M-x" 'helm-M-x)
  ;; org mode TAB 和 RET 切换标题折叠
  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   :predicate (lambda () (not window-system))
   "RET" #'org-cycle
   "TAB" #'org-cycle)
  ;; 在 treemacs-mode 下定义快捷键
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states 'normal
   "RET" #'treemacs-RET-action
   "C-m" #'treemacs-RET-action
   "ma" '(treemacs-create-file :which-key "Create File")
   "md" '(treemacs-create-dir :which-key "Create Directory")
   "mr" '(treemacs-rename :which-key "Rename")
   "mm" '(treemacs-move :which-key "Move")
   "mD" '(treemacs-delete :which-key "Delete")
   "f" #'treemacs-hide-gitignored-files-mode)
  )

;;; key-mapping.el ends here
