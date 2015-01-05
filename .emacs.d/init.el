;;; First of all...
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-; g") 'goto-line)
(keyboard-translate ?\C-h ?\C-?)
(recentf-mode)
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)

;; installed packages:
;; auto-complete color-theme flymake-cursor flycheck-pyflakes py-autopep8 js3-mode ess-mode yasnippet
;;; Should also install pyflakes via pip

;;; Screen size specific configuration
(setq initial-frame-alist
      (append '((width . 85)
		(height . 50)) initial-frame-alist))

;; Platform specific settings
(cond 
  ((string-match "apple-darwin" system-configuration)
   (define-key global-map [?¥] [?\\]))
  ((string-match "linux" system-configuration)
   (require 'mozc)
   (setq default-input-method "japanese-mozc")))

  
;; Packages
(require 'package)
(setq package-archives
      (append '(("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa" . "http://melpa.milkbox.net/packages/"))
              package-archives))
(package-initialize)

;; color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-euphoria)))


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

;;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

;;; py-autopep8
(require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)

;;; flymake
(require 'tramp-cmds)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)))


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

;; ess-mode
(load "ess-site")
