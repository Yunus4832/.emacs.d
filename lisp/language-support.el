;;; language-support.el --- 编程语言支持  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

;; yaml 支持
(use-package yaml-mode
    :ensure t
    :mode ("\\.yml\\'" . yaml-mode)
          ("\\.yaml\\'" . yaml-mode))

;; json 支持
(use-package json-mode :ensure t)

(provide 'language-support)

;;; language-support.el ends here
