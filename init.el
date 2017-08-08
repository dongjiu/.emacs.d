;; run as server
;; (require 'server)
;; (unless (server-running-p) (server-start))

;; environment variables
;; (setenv "PATH" (concat "/Users/dzhu/bin:" (getenv "PATH")))
(if (eq system-type 'windows-nt)
	(progn
	  (setenv "PATH" (concat "C:\\Users\\donzhu\\bin;" (getenv "PATH")))
	  (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))
;	  (setenv "PATH" (concat "C:\\Program Files\\Git\\mingw64\\bin;" (getenv "PATH")))
	  (setenv "PATH" (concat "C:\\Program Files (x86)\\MSBuild\\14.0\\Bin;" (getenv "PATH")))

	  ;; exec-path
	  (setq exec-path (append exec-path '("C:\\Program Files\\Git\\mingw64\\bin")))
	  (set-frame-font "Consolas 10")
	  )
  (setenv "PATH" (concat "/Users/dzhu/bin:/usr/local/bin:/Library/TeX/texbin:/sw/bin:"
						 "/Users/dzhu/tools/apache-maven-3.3.9/bin:"
						 "/usr/local/share/dotnet:"
						 (getenv "PATH")))
  (set-frame-font "Monaco 10")
  ;;(set-frame-font "-apple-Songti_SC-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1")
  (when (display-graphic-p)
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
						charset
						"-*-PingFang SC-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1")))


  (global-set-key (kbd "C-; h")
				  (lambda ()
					(interactive)
					(find-file "/Users/dzhu/Documents/notes/desktop.org")
					(read-only-mode 1)
					))
  )

;; transparent frame
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;(set-frame-parameter (selected-frame) 'alpha '(95 95))

;(add-to-list 'default-frame-alist '(alpha 95 95))
;(add-to-list 'default-frame-alist '(top . 10))
;(add-to-list 'default-frame-alist '(left . 0))
;(add-to-list 'default-frame-alist '(height . 65))
;(add-to-list 'default-frame-alist '(width . 240))

;; theme
;;(load-theme 'wombat t)
;;(load-theme 'deeper-blue t)
(load-theme 'tango-dark t)
(set-background-color "gray11")

;; appearance
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-fringe-mode 0))

;; mode-line
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line nil :foreground "gray")
(set-face-attribute 'mode-line nil :background "purple")
(set-face-attribute 'mode-line-inactive nil :box nil)

;; vertical bar
;;(set-face-attribute 'vertical-border nil :foreground "purple")
(set-face-attribute 'vertical-border nil :foreground "gray")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
;; web-mode
;;(require 'web-mode)
;; (setq web-mode-markup-indent-offset 4)
;; (setq web-mode-css-indent-offset 4)
;; (setq web-mode-code-indent-offset 4)
;; (setq web-mode-script-padding 4)
;; (setq web-mode-style-padding 4)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
;; (add-hook 'web-mode-hook
;; 		  (lambda ()
;; 			(setq indent-tabs-mode nil)))

;; clojure
;; (require 'clojure-mode)
;;(require 'cider-mode)

;; rainbow-mode
;; (require 'rainbow-mode)

(package-initialize)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;(setq ediff-window-setup-function 'ediff-setup-windows-multiframe)
(setq ediff-split-window-function 'split-window-horizontally) ;; vertical!!

;; vc-diff
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))

;; enable disabled functions
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)

;; tab
;;(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(add-hook 'java-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(c-set-offset 'substatement-open 0)
			(if (assoc 'inexpr-class c-offsets-alist)
				(c-set-offset 'inexpr-class 0))))

(add-hook 'csharp-mode-hook 'hs-minor-mode)

(add-hook 'csharp-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(c-set-offset 'substatement-open 0)
			(c-set-offset 'inline-open 0)
			(c-set-offset 'case-label '+)
			(if (assoc 'inexpr-class c-offsets-alist)
				(c-set-offset 'inexpr-class 0))
			(hs-minor-mode)
			(local-set-key (kbd "C-; C-; C-h") 'z-csharp-hide-methods)
			))

;; perl
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t?\\'" . cperl-mode))

;; c
(setq c-default-style "linux"
      c-basic-offset 4)
;; open .h in c++ mode
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-add-style "my-style"
             '(
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))
;;(defun my-c++-mode-hook ()
;;  (c-set-style "my-style")
;;  (auto-fill-mode)
;;  (c-toggle-auto-hungry-state 0))
;;(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; macros
;(fset 'comment-c [?\C-a ?\C-x ?\C-x ?\C-a ?\C-x ?r ?t ?/ ?/ return])
;(put 'comment-c 'kmacro t)


;;auto-complete
;;(require 'auto-complete)
;;(global-auto-complete-mode t)

;; dired
(setq dired-listing-switches "-alh")
(setq dired-dwimq-target t)


;;
;; key bindings
;;
;; run shell
(defun z-run-eshell ()
  "Run eshell"
  (interactive)
  (if (string= major-mode "eshell-mode")
	  (message "already in eshell mode")
	(let ((buf-name (concat "esh-" (buffer-name))))
	  (if (get-buffer buf-name)
		  (switch-to-buffer buf-name)
		(progn (eshell "dummy")
			   (rename-buffer buf-name))))))

(global-set-key (kbd "C-x C-h") 'z-run-eshell)

;; ffap
(global-set-key (kbd "C-x f") 'find-file-at-point)

;; buffer-menu
;;(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; line number
;;(global-linum-mode t)

;; org-mode
;;(setq org-export-with-sub-superscripts nil)
;;(setq org-default-notes-file "/Users/dzhu/Documents/notes/notes.org")

;; mysql
;; (setq sql-mysql-program "/usr/local/bin/mysql")
;; (setq sql-mysql-login-params
;;       '((server :default "localhost")
;;         (port :default 3306)
;;         (database :default "test")
;;         (user :default "root")
;;         (password)))
;; (add-hook 'sql-interactive-mode-hook
;; 		  (lambda ()
;; 			(toggle-truncate-lines t)))
;; ;;(setq sql-user "fc")
;; ;;(setq sql-password "Fc654321")
;; ;;(setq sql-server "192.168.8.8")
;; ;;(setq sql-mysql-options "optional command line options")
;; (setq sql-connection-alist
;;       '((local (sql-product 'mysql)
;;                (sql-server "localhost")
;;                (sql-port 3306)
;;                (sql-database "fcgyldb_dot3")
;;                (sql-user "root")
;;                (sql-password "mysql"))
;;         (dev (sql-product 'mysql)
;;               (sql-server "192.168.8.6")
;;               (sql-port 3306)
;;               (sql-database "fcgyldb_docker4")
;;               (sql-user "fc")
;;               (sql-password "Fc654321"))
;; 		(prod (sql-product 'mysql)
;;               (sql-server "192.168.8.4")
;;               (sql-port 3309)
;;               (sql-database "zunpindb")
;;               (sql-user "readonly_user")
;;               (sql-password "Fengchao4006221999"))
;; 		))

(load-file "~/.emacs.d/mylisp/z-string.el")
(load-file "~/.emacs.d/mylisp/z-util.el")
(load-file "~/.emacs.d/mylisp/z-csharp.el")

(global-set-key (kbd "C-,") 'set-mark-command)

(global-set-key (kbd "C-; i") 'imenu)
(global-set-key (kbd "C-; p") 'z-goto-match-paren)
(global-set-key (kbd "C-; g") 'rgrep)
(global-set-key (kbd "C-; f") 'find-dired)
(ffap-bindings)
(global-set-key (kbd "C-; w") 'ffap-copy-string-as-kill)
(global-set-key (kbd "C-; C-; y") 'z-dup-line)
(global-set-key (kbd "C-; C-; i") 'z-inc-num)
(global-set-key (kbd "C-; d d") 'z-word-definition)
(global-set-key (kbd "C-; d r") 'z-word-definition-region)
(global-set-key (kbd "C-; o c") 'org-capture)

;; reminder
(load-file "~/.emacs.d/mylisp/z-reminder.el")
(z-reminder-start t)
(global-set-key (kbd "C-; r s") 'z-reminder-start)
(global-set-key (kbd "C-; r e") 'z-reminder-stop)
(global-set-key (kbd "C-; r r") 'z-reminder-report)

;; git
(load-file "~/.emacs.d/mylisp/z-git.el")

;; minor mode
(define-minor-mode z-shell-mode
  "Better shell interaction"
  :lighter " z-sh")
(add-hook 'shell-mode-hook 'z-shell-mode)
(add-hook 'eshell-mode-hook 'z-shell-mode)

(define-minor-mode z-code-mode
  "Accelerate coding"
  :lighter " z-code")
(add-hook 'c-mode-hook 'z-code-mode)
(add-hook 'c++-mode-hook 'z-code-mode)
(add-hook 'java-mode-hook 'z-code-mode)
(add-hook 'csharp-mode-hook 'z-code-mode)
(add-hook 'typescript-mode-hook 'z-code-mode)

(define-minor-mode z-web-mode
  "Accelerate web programming"
  :lighter " z-web")
(add-hook 'html-mode-hook 'z-web-mode)
(add-hook 'web-mode-hook 'z-web-mode)

(define-minor-mode z-msbuild-mode
  "Accelerate msbuild"
  :lighter " z-msb")

;; abbreviations
(load-file "~/.emacs.d/mylisp/z-abbrevs.el")
(setq-default abbrev-mode t)

;; yasnippet
;;(add-to-list 'load-path "~/.emacs.d/snippets")
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(global-set-key (kbd "C-; TAB") 'yas-expand)

(setq initial-buffer-choice (lambda ()
							  (setq default-directory "C:/Users/donzhu")
							  (eshell)))

;; AutoHotKey
(load-file "~/.emacs.d/mylisp/z-ahk.el")
(global-set-key (kbd "C-; C-o C-v") 'z-open-file-at-point-in-vs)
