;;; First of all...
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-; g") 'goto-line)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(keyboard-translate ?\C-h ?\C-?)
(recentf-mode)
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)

;; Package management
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(el-get-bundle auto-complete)
(el-get-bundle jedi)
(el-get-bundle rope)
(el-get-bundle flymake-cursor)
(el-get-bundle elpy)
(el-get-bundle flycheck)
(el-get-bundle js3-mode)
(el-get-bundle yasnippet)

;;; Screen size specific configuration
(setq initial-frame-alist
      (append '((width . 85)
		(height . 50)) initial-frame-alist))

(setq custom-safe-themes t)
(load-theme 'wheatgrass)

;; Platform specific settings
(cond 
  ((string-match "apple-darwin" system-configuration)
   (define-key global-map [?¥] [?\\]))
  ((string-match "linux" system-configuration)
   (require 'mozc)
   (setq default-input-method "japanese-mozc")))

;;; Template
(require 'autoinsert)
(setq auto-insert-directory "~/.emacs.d/template")
(setq auto-insert-alist
      (append '(("\\.cpp$" . ["template.cpp" my-template])
                ("\\.h$" . ["template.h" my-template]))))
(defvar template-replacements-alists
  '(("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-noext%" .
     (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%" .
     (lambda () (format "%s_H_" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
	    (progn
	      (goto-char (point-min))
	      (replace-string (car c) (funcall (cdr c)) nil)))
	template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;; Yasnippet
(yas-global-mode t)


;;; Auto complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;; C-mode

(setq common-function-for-c-hook
         (lambda ()
           (local-set-key (kbd "C-; c") 'compile)
           (setq compile-command "./waf")))
(add-hook 'c-mode-hook common-function-for-c-hook)
(add-hook 'c++-mode-hook common-function-for-c-hook)
(add-hook 'cuda-mode-hook common-function-for-c-hook)

;;; Haskell-mode
;(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;; elpy
(elpy-enable)
(setq elpy-rpc-virtualenv-path "~/.pyenv/versions/elpy")
(add-hook 'elpy-mode-hook
          '(lambda ()
             (when (eq major-mode 'python-mode)
               (prog1
                   (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))))

(require 'tramp-cmds)

;;; js3 mode
(add-hook 'js3-mode-hook
          (lambda ()
            (setq js3-auto-indent-p t)
            (setq js3-curly-indent-offset 0)
            (setq js3-enter-indents-newline t)
            (setq js3-expr-indent-offset 2)
            (setq js3-indent-on-enter-key t)
            (setq js3-lazy-commas t)
            (setq js3-lazy-dots t)
            (setq js3-lazy-operators t)
            (setq js3-paren-indent-offset 2)
            (setq js3-square-indent-offset 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" default)))
 '(package-selected-packages
   (quote
    (yasnippet js3-mode py-autopep8 flycheck-pyflakes flymake-cursor auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
