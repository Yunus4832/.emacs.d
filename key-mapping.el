;;; key-mapping.el --- 按键映射配置  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'treemacs)
(require 'custom-command)

;; 使用 general 进行按键映射
(use-package general
  :config
  ;; 设置 Leader 键为空格
  (general-create-definer my-leader-def :prefix "SPC")
  ;; leader 按键绑定
  ;; 文件浏览器
  ;; 绑定 SPC f 到 treemacs-select-window
  (my-leader-def
    :states 'normal
    "f" 'treemacs-select-window)
  ;; IDE 功能
  ;; 打开最近访问文件列表
  (my-leader-def
    :states 'normal
    ";e" 'helm-recentf)
  ;; 打开 Git blame
  (my-leader-def
    :states 'normal
    ";a" 'magit-blame-addition)
  ;; 搜索项目文件
  (my-leader-def
    :states 'normal
    ";s" 'helm-projectile)
  ;; 搜索项目文件内容
  (my-leader-def
    :states 'visual
    ";s" 'helm-projectile-ag)
  ;; 搜索项目文件内容
  (my-leader-def
    :states 'normal
    ";S" 'helm-projectile-ag)
  ;; 搜索项目文件内容
  (my-leader-def
    :states 'visual
    ";S" 'helm-projectile-ag)
  ;; 格式化代码
  (my-leader-def
    :states 'normal
    ";f" 'format-all-buffer)
  ;; 注释代码
  (my-leader-def
    :states 'normal
    ";c" 'comment-line)
  ;; 注释代码
  (my-leader-def
    :states 'visual
    ";c" 'comment-dwim)
  ;; 版本控制
  ;; 跳转到下一个 Hunk
  (my-leader-def
    :states 'normal
    "vn" 'git-gutter:next-hunk)
  ;; 跳转到上一个 Hunk
  (my-leader-def
    :states 'normal
    "vp" 'git-gutter:previous-hunk)
  ;; 回滚当前 Hunk
  (my-leader-def
    :states 'normal
    "vr" 'git-gutter:revert-hunk)
  ;; 打开 magit 工具窗口
  (my-leader-def
    :states 'normal
    "vv" 'magit-status)
  ;; 打开 magit 工具窗口
  (my-leader-def
    :states 'normal
    "vc" 'magit-status)
  ;; 推送代码
  (my-leader-def
    :states 'normal
    "vs" 'magit-push-to-remote)
  ;; 拉取和更新代码
  (my-leader-def
    :states 'normal
    "vu" 'magit-pull-branch)
  ;; 提交记录
  (my-leader-def
    :states 'normal
    "vl" 'magit-log-all)
  ;; Buffer 切换
  ;; 下一个 Buffer
  (my-leader-def
    :states 'normal
    "bn" 'awesome-tab-forward-tab)
  ;; 上一个 Buffer
  (my-leader-def
    :states 'normal
    "bp" 'awesome-tab-backward-tab)
  ;; 最近访问的 Buffer
  (my-leader-def
    :states 'normal
    "bb" 'evil-switch-to-windows-last-buffer)
  ;; 删除当前 Buffer
  (my-leader-def
    :states 'normal
    "bd" 'evil-delete-buffer)
  ;; buffer 列表
  (my-leader-def
    :states 'normal
    "bl" 'helm-buffers-list)
  ;; buffer 内跳转
  (my-leader-def
    :states 'normal
    "SPC s" 'avy-goto-char-timer)
  ;; 临时开关
  ;; 开关软折行
  (my-leader-def
    :states 'normal
    ",w" 'toggle-truncate-lines)
  ;; 打开配置文件
  (my-leader-def
    :states 'normal
    ",," '(lambda () (interactive) (find-file user-init-file)))
  ;; compile 命令相关
  ;; 打开 compilation 缓冲区
  (my-leader-def
    :states 'normal
    "co" (lambda () (interactive)
	       (let ((buf (get-buffer-create "*compilation*")))
	         (with-current-buffer buf
	           (unless (eq major-mode 'compilation-mode)
		         (compilation-mode)))
	         (display-buffer buf 'display-buffer-at-bottom))))
  ;; 关闭 compilation 缓冲区
  (my-leader-def
    :states 'normal
    "cc" (lambda () (interactive)
	       (let ((win (get-buffer-window "*compilation*")))
	         (when win
	           (quit-window nil win)))))
  ;; 下一个错误
  (my-leader-def
    :states 'normal
    "cn" 'next-error)
  ;; 上一个错误
  (my-leader-def
    :states 'normal
    "cp" 'previous-error)
  ;; 编译生成 compilation 缓冲区
  (my-leader-def
    :states 'normal
    "cm" 'compile)
  ;; 切换终端
  (general-def
    "M-=" 'multi-term-dedicated-toggle)  ;; 切换终端
  ;; 使用 helm-M-x 替代 M-x
  (general-def
    "M-x" 'helm-M-x)
  ;; 退出 helm 使用 ESC 或者再次按下 M-x
  (general-def helm-map
    "<escape>" #'helm-keyboard-quit)
  (general-def helm-map
    "M-x" #'helm-keyboard-quit)
  ;; 如果没有打开文件，dashboard 使用 q 直接退出
  (general-def
    :keymaps 'dashboard-mode-map
    :states 'normal
    "q" (lambda ()
	      (interactive)
	      (if (my/has-file-buffers-p)
	          (quit-window)
	        (save-buffers-kill-terminal))))
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
