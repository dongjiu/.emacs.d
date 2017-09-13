(defun z-show-windows ()
  "Show title and class for all windows."
  (interactive)
  (let ((buf "*WINDOWS*"))
	(pop-to-buffer buf)
	(erase-buffer)
	(call-process "c:/Users/donzhu/github/tools4win/show_windows.exe" nil buf)
	(sort-lines nil (point-min) (point-max))
	(goto-char (point-min))))

(defun z-windows ()
  "Get all windows."
  (let ((class) (title) (windows) (stop))
	(with-temp-buffer
	  (call-process "c:/Users/donzhu/github/tools4win/show_windows.exe" nil t)
	  (sort-lines t (point-min) (point-max))
	  (goto-char (point-min))
	  (message "Lines:　%d" (count-lines (point-min) (point-max)))
	  (while (not stop)
		(if (search-forward "Class: " nil t)
			(progn (push-mark)
				   (search-forward ",")
				   (backward-char)
				   (setq class (buffer-substring-no-properties (region-beginning) (region-end)))
				   (search-forward "Title: ")
				   (push-mark)
				   (end-of-line)
				   (setq title (buffer-substring-no-properties (region-beginning) (region-end)))
				   (push (list class title) windows)
				   (next-logical-line)
				   (beginning-of-line))
		  (setq stop t))))
	windows))

(defun z-windows-title-contains (str)
  "Return windows whose title contains STR."
  (cl-remove-if-not (lambda (w)
					  (string-match-p (regexp-quote str) (nth 1 w)))
					(z-windows)))

(defun z-cmd-here (&optional cmd)
  "Run Windows cmd.exe in default directory and execute CMD."
  (interactive)
  (let ((dir default-directory))
	(setq dir (z-string-win-style-path dir))
	(w32-shell-execute "runas" "cmd" (concat " /K cd /d " dir (when cmd (concat " && " cmd))))))

(defun z-ucmba ()
  "Run UCM Business Analytics shell."
  (interactive)
  (z-cmd-here "cd /d D:\\work\\UCM-BusinessAnalytics && title Business Analytics && set PATH=C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Enterprise\\MSBuild\\15.0\\Bin;%PATH% && init.cmd"))

(defun z-set-cl-env ()
  "Set environment variables for cl.exe."
  (setenv "LIB" "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\LIB;C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\ATLMFC\\LIB;C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.10240.0\\ucrt\\x86;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\lib\\um\\x86;C:\\Program Files (x86)\\Windows Kits\\8.1\\lib\\winv6.3\\um\\x86;")
  (setenv "INCLUDE" "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\INCLUDE;C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\ATLMFC\\INCLUDE;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.10240.0\\ucrt;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\include\\um;C:\\Program Files (x86)\\Windows Kits\\8.1\\include\\shared;C:\\Program Files (x86)\\Windows Kits\\8.1\\include\\um;C:\\Program Files (x86)\\Windows Kits\\8.1\\include\\winrt;")
  (let ((cl-path "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\bin"))
	(unless (member cl-path (z-path-dirs))
	  (setenv "PATH" (concat cl-path ";" (getenv "PATH"))))))

(defun z-set-cl-env-2 ()
  "Set environment variables for cl.exe."
  (setenv "LIB" "C:\\Program Files\\Microsoft Visual Studio 14.0\\VC\\lib")
  (setenv "INCLUDE" "C:\\Program Files\\Microsoft Visual Studio 14.0\\VC\\include;C:\\Program Files\\Windows Kits\\10\\Include\\10.0.15063.0\\ucrt;C:\\Program Files\\Windows Kits\\10\\Include\\10.0.15063.0\\um"))
