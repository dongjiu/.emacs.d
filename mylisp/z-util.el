(defun z-word-definition ()
  "Search word definition in a dictionary"
  (interactive)
;  (browse-url (concat "http://cn.bing.com/dict/search?q=" (thing-at-point 'word))))
  (browse-url (concat "http://cn.bing.com/dict/search?q=" (current-word))))

(defun z-word-definition-region ()
  "Search word (in region) definition in a dictionary"
  (interactive)
  (browse-url (concat "http://cn.bing.com/dict/search?q="
					  (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun z-search-region ()
  "Search text (in region) in search engine."
  (interactive)
  (browse-url (concat "https://www.bing.com/search?q="
					  (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun z-goto-close-paren (open-paren close-paren)
  (let ((parens-to-match 1)
        (stop nil)
        (start-point (point))
        (char))
    (push-mark)
    (forward-char)
    
    (while (and (> parens-to-match 0) (not stop))
      (setq char (char-after))
      
      (if (char-equal char open-paren)
          (setq parens-to-match (1+ parens-to-match))
        (if (char-equal char close-paren)
            (setq parens-to-match (- parens-to-match 1))))

      (if (or (= parens-to-match 0)
              (= (point) (- (point-max) 1)))
          (setq stop t)
        (forward-char)))

    (if (> parens-to-match 0)
        (progn (goto-char start-point)
               (message "No matching close paren.")))))


(defun z-goto-open-paren (close-paren open-paren)
  (let ((parens-to-match 1)
        (stop nil)
        (start-point (point))
        (char))
    (push-mark)
    (backward-char)
    
    (while (and (> parens-to-match 0) (not stop))
      (setq char (char-after))
      
      (if (char-equal char close-paren)
          (setq parens-to-match (1+ parens-to-match))
        (if (char-equal char open-paren)
            (setq parens-to-match (- parens-to-match 1))))

      (if (or (= parens-to-match 0)
              (= (point) (point-min)))
          (setq stop t)
        (backward-char)))

    (if (> parens-to-match 0)
        (progn (goto-char start-point)
               (message "No matching open paren.")))))


(defun z-goto-match-paren ()
  (interactive)
  (let ((char))
    (setq char (char-after))
    (cond ((char-equal char ?\() (z-goto-close-paren ?\( ?\)))
          ((char-equal char ?\[) (z-goto-close-paren ?\[ ?\]))
          ((char-equal char ?\{) (z-goto-close-paren ?\{ ?\}))
          ((char-equal char ?\<) (z-goto-close-paren ?\< ?\>))

          ((char-equal char ?\)) (z-goto-open-paren ?\) ?\())
          ((char-equal char ?\]) (z-goto-open-paren ?\] ?\[))
          ((char-equal char ?\}) (z-goto-open-paren ?\} ?\{))
          ((char-equal char ?\>) (z-goto-open-paren ?\> ?\<))

          (t (if (string-match "\\.java\\'" (buffer-name))
                 (z-goto-open-paren ?\} ?\{)
               (z-goto-open-paren ?\) ?\())))))

(defun z-dup-line (&optional N)
  "Duplicate current line and insert it after the current line.
If N is positive, insert N lines.
Otherwise, do nothing."
  (interactive "p")
  (let ((start) (end) (i 0))
	(save-excursion
	  (beginning-of-line)
	  (setq start (point))
	  (end-of-line)
	  (setq end (point))
	  (while (< i N)
		(newline)
		(insert-buffer-substring (buffer-name) start end)
		(setq i (1+ i))))))

(defun z-inc-num (&optional n)
  "Increment the number at point. If N is not nil, increment the number at point by n."
  (interactive "p")
  (let* ((num (+ (thing-at-point 'number) (if n n 1)))
		 (bounds (bounds-of-thing-at-point 'symbol))
		 (beg (car bounds))
		 (end (cdr bounds)))
	(save-excursion
	  (delete-region beg end)
	  (insert (format "%g" num)))))

(defun z-java-declaration-p (p)
  ""
  (let ((is-decl))
	(save-excursion
	  (goto-char p)
	  (while (or  (equal (char-before) ?\s)
				  (equal (char-before) ?\t))
		(backward-char 1))
	  (if (or (equal (char-before) ?\n)
			  (equal (char-before) ?\=))
		  (setq is-decl nil)
		(setq is-decl t)))
	is-decl
	))

(defun z-goto-variable-declaration (&optional no-mark)
  "Go to the declaration of the variable at current point."
  (interactive)
  (let ((word (thing-at-point 'word))
		(end)
		(dest)
		(case-fold-search nil))
	(forward-sexp)
	(setq end (point))
	(save-excursion
	  (c-beginning-of-defun)
	  (search-forward-regexp (concat "[ \\t]" word "\\b") end)
	  (backward-sexp)
	  (setq dest (point)))
	(save-excursion
	  (when (not (z-java-declaration-p dest))
		(c-beginning-of-defun)
		(backward-up-list)
		(search-forward-regexp (concat "[ \\t]" word "\\b") end)
		(backward-sexp)
		(setq dest (point))))
	(when (not no-mark)
	  (push-mark (point)))
	(goto-char dest)))

(defun z-variable-type (p)
  "Get the type name of the variable."
  (let ((beg) (mid) (end))
	(save-excursion
	  (goto-char p)
	  (while (not (or (equal (char-before) ?\s)
					  (equal (char-before) ?\t)))
		(backward-char))
	  (backward-char)
	  (while (or (char-equal (char-after) ?\s)
				 (char-equal (char-after) ?\t))
		(backward-char))
	  (setq end (1+ (point)))
	  (if (char-equal (char-after) ?\>)
		  (progn (z-goto-open-paren ?\> ?\<)
				 (setq mid (point)))
		(setq mid end))
	  (when (char-equal (char-after) ?\])
		(search-backward "[" nil t))
	  (backward-sexp)
	  (setq beg (point)))
	(list (buffer-substring-no-properties beg mid)
		  (buffer-substring-no-properties beg end))))

(defun z-show-variable-type ()
  "Show the type name of the variable"
  (interactive)
  (let ((type))
	(z-goto-variable-declaration)
	(setq type (z-variable-type (point)))
	(message "Type: %s, Declartion: %s" (car type) (nth 1 type))))

(defun z-java-find-class-file-in-tag (class)
  (let* ((proj-base (z-java-proj-base))
		 (tag-file (concat proj-base "/TAGS"))
		 (class-file)
		 (file-name (lambda ()
					  (let ((beg (line-beginning-position)) (end))
						(skip-chars-forward "^,")
						(setq end (point))
						(concat proj-base "/" (buffer-substring-no-properties beg end))))))
	(if (file-exists-p tag-file)
		(with-temp-buffer
		  (insert-file-contents tag-file)
		  (if (search-forward (concat "/" class ".java") nil t)
			  (setq class-file (funcall file-name))
			(when (search-forward-regexp (concat "\\bclass\\b.*\\b" class "[[:space:]]") nil t)
			  (when (search-backward ".java,")
				(setq class-file (funcall file-name)))))
		  (unless class-file
			(message "Cannot find class in TAGS file")))
	  (message "No TAGS file found:%s" tag-file))
	class-file))
		  
(defun z-java-view-class (dir class &optional mem)
  (let ((file (z-java-find-class-file-in-tag class))
		(buffer))
	(message file)
	(when file
	  (setq buffer (get-buffer (file-name-nondirectory file)))
	  (if buffer
		  (switch-to-buffer buffer)
		(find-file file)
		(read-only-mode))
	  (when mem
		(goto-char (point-min))
		(when (search-forward-regexp (concat "[[:space:]]" mem "\\b") nil t)
		  (recenter-top-bottom))))))

(defun z-java-proj-base ()
  "Return the project base directory"
  (let ((base))
	(setq base (shell-command-to-string "git rev-parse --show-toplevel"))
	(when (string-match "fatal:" base)
	  (setq base default-directory))
	(setq base (replace-regexp-in-string "\n" "" base))
	base
  ))

(defun z-java-goto-variable-class ()
  (interactive)
  (let ((class) (mem) (proj-base (z-java-proj-base)))
	(save-excursion
	  (while (not (or (equal (char-after) ?\s)
					  (equal (char-after) ?\t)
					  (equal (char-after) ?\.)))
		(forward-char))
	  (while (or (equal (char-after) ?\s)
				 (equal (char-after) ?\t))
		(forward-char))
	  (if (equal (char-after) ?\.)
		  (let ((beg) (end))
			(forward-sexp)
			(setq end (point))
			(backward-sexp)
			(setq beg (point))
			(setq mem (buffer-substring-no-properties beg end)))
		(setq mem nil)))
	(message "member: %s" mem)
	(save-excursion
	  (z-goto-variable-declaration t)
	  (setq class (car (z-variable-type (point)))))
	(z-java-view-class proj-base class mem)))

(defun z-java-base-class (buffer)
  "Return base class of current class."
  (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward-regexp "class.*\\bextends[[:space:]]+\\(\\w+\\)" nil t)
		  (match-string 1)
		nil))))

(defun z-java-base-classes (buffer)
  "Return base classes of current class."
  (let ((base) (ancestors))
	(setq base (z-java-base-class buffer))
	(when base
	  (with-temp-buffer
		(insert-file-contents (z-java-find-class-file-in-tag base))
		(setq ancestors (cons base (z-java-base-classes (current-buffer))))))
	ancestors))

(defun z-java-show-bases (&optional buffer)
  "Show base classes of the class in buffer."
  (interactive)
  (unless buffer
	(setq buffer (current-buffer)))
  (message (mapconcat 'identity (z-java-base-classes buffer) ", ")))

(defun z-java-build-tags ()
  "Build TAGS file using etags."
  (interactive)
  (with-temp-buffer
	(let ((default-directory (concat (z-java-proj-base) "/")))
	  (shell-command "rm TAGS")
	  (shell-command "git ls-files | grep java\\$ | xargs etags -a")))
  (message "tags generated."))

(defun git-diff (file commit1 commit2)
  (shell-command (format "~/bin/git_diff %s %s %s" file commit1 commit2))
  (ediff-files "_tmp1" "_tmp2"))

(defun z-eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;;(eshell-send-input)
	))

(defun z-add-line-number ()
  "Add line numbers to each line in buffer"
  (interactive)
  (let ((n 1))
	(goto-char 0)
	(while (not (eq (point) (point-max)))
	  (insert (format "%d: " n))
	  (beginning-of-line)
	  (setq n (+ 1 n))
	  (next-line))))

(defun z-open (&optional file)
  "Open file/dir."
  (interactive)
  (let ((cmd))
	(unless file
	  (if (string= major-mode "dired-mode")
		  (progn (setq file (dired-copy-filename-as-kill 0))
				 (when (eq system-type 'windows-nt)
				   (setq file (z-string-win-style-path file))))
		(setq file (thing-at-point 'filename)))
	  (unless file
		(setq file ".")))
	(cond ((eq system-type 'windows-nt)
		   (w32-shell-execute "open" file))
		  ((eq system-type 'darwin)
		   (shell-command "open " file)))))

(defun z-reload-file ()
  "Reload current file."
  (interactive)
  (let ((line-number (count-lines (point-min) (point))))
	(when (buffer-file-name)
	  (find-alternate-file (buffer-file-name))
	  (goto-line line-number))))

(defun z-copy-buffer-file-name ()
  "Copy buffer file name."
  (interactive)
  (let ((file (buffer-file-name)))
	(when (eq system-type 'windows-nt)
	  (setq file (z-string-win-style-path file)))
	(kill-new file)))

(defun z-path-dirs ()
  "Return a list of directories in environment variable PATH."
  (split-string (getenv "PATH") path-separator))

(defun z-show-path ()
  "Show directories in PATH."
  (interactive)
  (pop-to-buffer "*PATH*")
  (erase-buffer)
  (dolist (p (z-path-dirs))
	(insert (concat p "\n"))))

(defun z-cmd-here ()
  "Run Windows cmd in default directory."
  (interactive)
  (let ((dir default-directory))
	(setq dir (z-string-win-style-path dir))
	(w32-shell-execute "runas" "cmd" (concat " /K cd /d " dir))))

(defun z-file-contains (es-input &optional regex)
  "Run Everything Search to find files using ES-INPUT, and filter the files by REGEX."
  (let ((buf "*Files*") (file))
	(pop-to-buffer buf)
	(with-temp-buffer
	  (shell-command (concat "es " es-input) 1)
	  (goto-char (point-min))
	  (while (< (point) (point-max))
		(setq file (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
		(when (z--file-contains file regex)
		  (save-excursion
			(set-buffer buf)
			(insert file)
			(newline)))
		(forward-line)))
	(goto-char (point-min))))

(defun z--file-contains (file regex)
  "Returns non-nil if FILE contains REGEX."
  (with-temp-buffer
	(if (ignore-errors
		  (insert-file-contents file))
		(re-search-forward regex nil t)
	  nil)))
  

(defun z-chrome (&optional url)
  "Open URL in chrome."
  (interactive)
  (unless url
	(setq url (ffap-url-at-point)))
  (when url
	(w32-shell-execute "open" "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe" url)))

