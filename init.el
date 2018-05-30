(require 'package)
(require 'org)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; bootstrap call
;; real config goes in config.org
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
