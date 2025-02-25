;;; evil-escape.el --- Escape from anything with customizable key sequences  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil
;; Created: 22 Oct 2014
;; Package-Version: 20241212.1318
;; Package-Revision: aebd1a78a6bd
;; Package-Requires: ((emacs "26") (evil "1.14.0") (cl-lib "0.5"))
;; URL: https://github.com/emacsorphanage/evil-escape

;; This file is not part of GNU Emacs.

;;; Code:

(require 'evil)
(require 'cl-lib)

(eval-when-compile
  (declare-function evil-iedit-state/quit-iedit-mode "evil-iedit-state.el"))

(defgroup evil-escape nil
  "Key sequences to escape insert state and everything else."
  :prefix "evil-escape-"
  :group 'evil)

(defcustom evil-escape-key-sequence (list (kbd "fd"))
  "List of key sequences to escape from insert state."
  :type '(repeat key-sequence)
  :group 'evil-escape)

(defcustom evil-escape-lighter '(concat " " evil-escape-key-sequence)
  "The lighter for the evil escape mode."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-delay 0.1
  "Max time delay between two key presses."
  :type 'number
  :group 'evil-escape)

(defcustom evil-escape-unordered-key-sequence nil
  "Whether the key sequence can also be entered with the second key first."
  :type 'boolean
  :group 'evil-escape)

(defcustom evil-escape-case-insensitive-key-sequence nil
  "Whether the key sequence is case-insensitive.
This makes it possible use any of \"df\", \"DF\", \"Df\" or \"dF\"
to escape."
  :type 'boolean
  :group 'evil-escape)

(defcustom evil-escape-excluded-major-modes nil
  "Excluded major modes where escape sequences have no effect."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-excluded-states nil
  "Excluded states where escape sequences have no effect."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-enable-only-for-major-modes nil
  "List of major modes where evil-escape is enabled."
  :type 'sexp
  :group 'evil-escape)

(defcustom evil-escape-inhibit-functions nil
  "List of zero argument predicate functions disabling evil-escape.
If any of these functions return non nil, evil escape will be inhibited."
  :type 'sexp
  :group 'evil-escape)

(defvar evil-escape-inhibit nil
  "When non nil evil-escape is inhibited.")

;;;###autoload
(define-minor-mode evil-escape-mode
  "Minor mode to escape insert state and everything else with a key sequence."
  :lighter (:eval evil-escape-lighter)
  :group 'evil
  :global t
  (if evil-escape-mode
      (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
    (remove-hook 'pre-command-hook 'evil-escape-pre-command-hook)))

(defun evil-escape ()
  "Escape from everything... well almost everything."
  (interactive)
  (call-interactively (evil-escape-func)))

(defun evil-escape-func ()
  "Return the function to escape from everything."
  (pcase evil-state
    (`normal (evil-escape--escape-normal-state))
    (`motion (evil-escape--escape-motion-state))
    (`insert 'evil-normal-state)
    (`emacs (evil-escape--escape-emacs-state))
    (`hybrid (evil-escape--escape-emacs-state))
    (`evilified (evil-escape--escape-emacs-state))
    (`visual 'evil-exit-visual-state)
    (`replace 'evil-normal-state)
    (`lisp 'evil-lisp-state/quit)
    (`iedit 'evil-iedit-state/quit-iedit-mode)
    (`iedit-insert 'evil-iedit-state/quit-iedit-mode)
    (`multiedit 'evil-multiedit-abort)
    (`multiedit-insert 'evil-multiedit-state)
    (_ (evil-escape--escape-normal-state))))

(defun evil-escape-command-keys ()
  (if (and evil-escape-case-insensitive-key-sequence
           (char-or-string-p (this-command-keys)))
      (downcase (this-command-keys))
    (this-command-keys)))

(defun evil-escape-pre-command-hook ()
  "evil-escape pre-command hook."
  (with-demoted-errors "evil-escape: Error %S"
      (when (evil-escape-p)
        ;; Don't inhibit redisplay, else visual mode j key will not be updated.
        (let* ((inhibit-redisplay nil)
               (fontification-functions nil)
               (modified (buffer-modified-p))
               (inserted (evil-escape--insert))
               (current-key (this-command-keys))
               (all-sequences (evil-escape--all-sequences))
               (matching-sequences (cl-loop for seq in all-sequences
                                            when (equal current-key (substring seq 0 1))
                                            collect seq))
               (expected-second-keys (mapcar (lambda (s) (elt s 1)) matching-sequences))
               (evt (read-event nil nil evil-escape-delay)))
          (when inserted (evil-escape--delete))
          (set-buffer-modified-p modified)
          (cond
           ((and (characterp evt)
                 (member evt expected-second-keys))
            (evil-repeat-stop)
            (let ((esc-fun (evil-escape-func)))
              (when esc-fun
                (setq this-command esc-fun)
                (setq this-original-command esc-fun))))
           ((null evt))
           (t (setq unread-command-events
                    (append unread-command-events (list evt)))))))))

(defun evil-escape--evil-repeat (fn &rest args)
  "Bind `evil-escape-inhibit' to t."
  (let ((evil-escape-inhibit t))
    (apply fn args)))
(advice-add 'evil-repeat :around #'evil-escape--evil-repeat)

(defun evil-escape--all-sequences ()
  "Return all possible key sequences considering unordered option."
  (cl-loop for seq in evil-escape-key-sequence
	   when (= (length seq) 2)
	   nconc (if evil-escape-unordered-key-sequence
		     (list seq (reverse seq))
		   (list seq))))

(defun evil-escape-p ()
  "Return non-nil if evil-escape can run."
  (and evil-escape-key-sequence
       (not evil-escape-inhibit)
       (or (window-minibuffer-p)
           (bound-and-true-p isearch-mode)
           (memq major-mode '(ibuffer-mode
                              image-mode))
           (evil-escape--is-magit-buffer)
           (and (fboundp 'helm-alive-p) (helm-alive-p))
           (or (not (eq 'normal evil-state))
               (not (eq 'evil-force-normal-state
                        (lookup-key evil-normal-state-map [escape])))))
       (not (memq major-mode evil-escape-excluded-major-modes))
       (not (memq evil-state evil-escape-excluded-states))
       (or (not evil-escape-enable-only-for-major-modes)
           (memq major-mode evil-escape-enable-only-for-major-modes))
       (cl-loop for seq in (evil-escape--all-sequences)
           thereis (equal (this-command-keys) (substring seq 0 1)))
       (not (cl-reduce (lambda (x y) (or x y))
                       (mapcar 'funcall evil-escape-inhibit-functions)
                       :initial-value nil))))

(defun evil-escape--escape-normal-state ()
  "Return the function to escape from normal state."
  (cond
   ((and (fboundp 'helm-alive-p) (helm-alive-p)) 'helm-keyboard-quit)
   ((eq 'ibuffer-mode major-mode) 'ibuffer-quit)
   ((eq 'image-mode major-mode) 'quit-window)
   ((evil-escape--is-magit-buffer) 'evil-escape--escape-with-q)
   ((bound-and-true-p isearch-mode) 'isearch-abort)
   ((window-minibuffer-p) 'abort-recursive-edit)
   ((eq 'treemacs-mode major-mode) 'treemacs-quit)
   (t (lookup-key evil-normal-state-map [escape]))))

(defun evil-escape--escape-motion-state ()
  "Return the function to escape from motion state."
  (cond
   ((or (memq major-mode '(apropos-mode
                           help-mode
                           ert-results-mode
                           ert-simple-view-mode
                           compilation-mode
                           image-mode)))
    'quit-window)
   ((eq 'undo-tree-visualizer-mode major-mode) 'undo-tree-visualizer-quit)
   ((and (fboundp 'helm-ag--edit-abort)
         (string-equal "*helm-ag-edit*" (buffer-name)))
    'helm-ag--edit-abort)
   ((eq 'neotree-mode major-mode) 'neotree-hide)
   (t 'evil-normal-state)))

(defun evil-escape--escape-emacs-state ()
  "Return the function to escape from emacs state."
  (cond
   ((bound-and-true-p isearch-mode) 'isearch-abort)
   ((window-minibuffer-p) 'abort-recursive-edit)
   ((evil-escape--is-magit-buffer) 'evil-escape--escape-with-q)
   ((eq 'ibuffer-mode major-mode) 'ibuffer-quit)
   ((eq 'emoji-cheat-sheet-plus-buffer-mode major-mode) 'kill-this-buffer)
   ((eq 'paradox-menu-mode major-mode) 'evil-escape--escape-with-q)
   ((memq major-mode '(gist-list-menu-mode
                       image-mode))
    'quit-window)
   (t 'evil-normal-state)))

(defun evil-escape--first-key ()
  "Return the first key string in the key sequence."
  (let* ((first-key (elt evil-escape-key-sequence 0))
         (fkeystr (char-to-string first-key)))
    fkeystr))

(defun evil-escape--second-key ()
  "Return the second key string in the key sequence."
  (let* ((sec-key (elt evil-escape-key-sequence 1))
         (fkeystr (char-to-string sec-key)))
    fkeystr))

(defun evil-escape--insert-func ()
  "Default insert function."
  (when (not buffer-read-only) (self-insert-command 1)))

(defun evil-escape--delete-func ()
  "Delete char in current buffer if not read only."
  (when (not buffer-read-only) (delete-char -1)))

(defun evil-escape--insert ()
  "Insert the first key of the sequence."
  (condition-case nil
      (pcase evil-state
        (`insert (evil-escape--insert-2) t)
        (`emacs (evil-escape--insert-2) t)
        (`hybrid (evil-escape--insert-2) t)
        (`normal
         (when (window-minibuffer-p) (evil-escape--insert-func) t))
        (`iedit-insert (evil-escape--insert-func) t))
    (error nil)))

(defun evil-escape--insert-2 ()
  "Insert character while taking into account mode specificites."
  (pcase major-mode
    (`term-mode (call-interactively 'term-send-raw))
    (_ (cond
        ((bound-and-true-p isearch-mode) (isearch-printing-char))
        (t (evil-escape--insert-func))))))

(defun evil-escape--delete ()
  "Revert the insertion of the first key of the sequence."
  (pcase evil-state
    (`insert (evil-escape--delete-2))
    (`emacs (evil-escape--delete-2))
    (`hybrid (evil-escape--delete-2))
    (`normal
     (when (minibuffer-window-active-p (current-buffer))
       (evil-escape--delete-func)))
    (`iedit-insert (evil-escape--delete-func))))

(defun evil-escape--delete-2 ()
  "Delete character while taking into account mode specifities."
  (pcase major-mode
    (`term-mode (call-interactively 'term-send-backspace))
    (_ (cond
        ((bound-and-true-p isearch-mode) (isearch-delete-char))
        (t (evil-escape--delete-func))))))

(defun evil-escape--escape-with-q ()
  "Send `q' key press event to exit from a buffer."
  (interactive)
  (setq unread-command-events (listify-key-sequence "q")))

(defun evil-escape--is-magit-buffer ()
  "Return non nil if the current buffer is a Magit buffer."
  (string-match-p "magit" (symbol-name major-mode)))

(provide 'evil-escape)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-escape.el ends here
