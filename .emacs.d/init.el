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
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(package-initialize)

;(use-package init-loader)


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
(use-package yasnippet
  :straight t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (use-package yasnippet-snippets))
    


;;; C-mode
(setq common-function-for-c-hook
         (lambda ()
           (local-set-key (kbd "C-; c") 'compile)
           (setq compile-command "./waf")))
(add-hook 'c-mode-hook common-function-for-c-hook)
(add-hook 'c++-mode-hook common-function-for-c-hook)
(add-hook 'cuda-mode-hook common-function-for-c-hook)

;;; Haskell-mode
(use-package haskell-mode
  :straight t
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (autoload 'ghc-init "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))


;;; elpy
(use-package pyvenv
  :straight t)

(use-package elpy
  :straight t
  :init
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-flymake
                       elpy-module-pyvenv
                       elpy-module-yasnippet
                       elpy-module-sane-defaults))
  (add-hook 'elpy-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))
  (setq elpy-rpc-virtualenv-path "~/.pyenv/versions/elpy")
  (elpy-enable))

;; latex
(setq tex-default-mode 'latex-mode)
(setq tex-start-commands "")
(setq tex-print-file-extension ".pdf")
(setq latex-run-command "lualatex")
(setq tex-dvi-view-command "open")

(use-package jedi)
(use-package flymake-cursor)
(use-package elpy)
(use-package flycheck)
(use-package js2-mode)
(use-package cider)
(use-package tramp)

