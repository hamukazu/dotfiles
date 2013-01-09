;;; First of all...
(global-font-lock-mode t)
(global-set-key (kbd "C-h") 'delete-backward-char)
(recentf-mode)
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq-default indent-tabs-mode nil)
(set-language-environment "Japanese")
(require 'ruby-mode)
(defun ruby-mode-set-encoding () ())


;; set default coding system
(set-default-coding-systems 'utf-8)

;; for anthy
;(load-library "anthy")
;(setq default-input-method "japanese-anthy")
;(global-set-key [zenkaku-hankaku] 'toggle-input-method)
;; for mozc
(require 'mozc)
(setq default-input-method "japanese-mozc");;; 
(setq load-path (cons "~/.emacs.d/plugins" load-path))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

;; for ghc
(setq load-path (cons "~/cabal/ghc-mod-1.10.12" load-path))
(setq exec-path (cons "~/cabal/bin" exec-path))


;; color theme
(setq load-path (cons "~/.emacs.d/plugins/color-theme-6.6.0" load-path))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-euphoria)))

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;; Additional mode
;(load "cuda-mode.el")
(autoload 'js2-mode "js2" nil t)
(setq inhibit-startup-message t)
(setq auto-mode-alist (append '(("\\.ml" . tuareg-mode) 
                                ("\\.html" . html-helper-mode)
                                ("\\.hs" . haskell-mode)
                                ("wscript" . python-mode)
                                ("\\.tex$" . yatex-mode)
                                ("js" . js2-mode))

                              auto-mode-alist))

;(autoload 'ghc-init "ghc" nil t)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

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

;;; yasnippet

(add-hook 'html-helper-timestamp-hook (function (lambda ()
	   (insert "Last modified: " (format-time-string "%Y-%m-%d %T\n")))))

;;; C-mode

(setq common-function-for-c-hook
         (lambda () 
           (local-set-key (kbd "C-; c") 'compile)
           (setq compile-command "./waf")))

(add-hook 'c-mode-hook common-function-for-c-hook)
(add-hook 'c++-mode-hook common-function-for-c-hook)
(add-hook 'cuda-mode-hook common-function-for-c-hook)

(setq load-path (cons "~/.emacs.d/plugins/yasnippet-0.6.1c" load-path))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;;; Platform specific configuration

 (cond 
  ((string-match "apple-darwin" system-configuration)
   (autoload 'tuareg-mode "tuareg-mode")
   (load "~/.emacs.d/plugins/html-helper-mode/html-helper-mode")
   (setq load-path (cons "~/.emacs.d/plugins/haskell-mode-2.8.0" load-path))
   )
  ((string-match "mingw" system-configuration)
   (require 'tramp)
   (cons "/bin/sh" tramp-remote-path)
   (setq load-path (cons "~/.emacs.d/plugins/haskell-mode-2.8.0" load-path))
   (load "~/.emacs.d/plugins/html-helper-mode/html-helper-mode")
   (modify-coding-system-alist 'process "plink" 'utf-8-unix)
   (setq tramp-default-method "plink")))

;;; tramp
(setq tramp-default-user "kkato")

;;; Haskell-mode
;(require 'inf-haskell)
;(setq haskell-program-name "ghci")
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'font-lock-mode)
;(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; Screen size specific configuration

(setq initial-frame-alist 
      (append '((width . 85)
		(height . 50)
		(top . 0)
		(left . 0)) initial-frame-alist))
;(add-hook 'after-init-hook
;	  (lambda () (split-window-horizontally) 
;	    (other-window 0)))


(setq c-default-style
      '((java-mode . "gnu") ))

;;; yatex
(setq load-path (cons "~/.emacs.d/plugins/yatex1.75" load-path))

;;; aspell
(setq-default ispell-program-name "aspell")

;;; ejacs
(setq load-path (cons "~/.emacs.d/plugins/ejacs" load-path))
(autoload 'js-console "js-console" nil t)

;;; auto-install
;(require 'auto-install)
;(setq auto-install-directory "~/.emacs.d/auto-install/")
;(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
