;;; custom-command.el --- 自定义命令  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; 是否存在打开的文件 buffer
(defun my/has-file-buffers-p ()
  "Return non-nil if there's any buffer visiting a file (excluding special buffers)."
  (cl-find-if (lambda (buf)
                (and (buffer-file-name buf)
                     (not (string-prefix-p "*" (buffer-name buf)))
                     (buffer-live-p buf)))
              (buffer-list)))

(provide 'custom-command)

;;; custom-command.el ends here
