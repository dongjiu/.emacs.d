(setq z-ahk-exe (concat z-home "\\softwares\\AutoHotkey_1.1.26.01\\AutoHotkeyU" (format "%s" z-os-bit) ".exe"))
(setq z-ahk-script (expand-file-name "~/.emacs.d/ahk/init.ahk"))
(unless (cl-remove-if-not 'identity
						  (mapcar (lambda (pid)
									(string-match "AutoHotKey" (cdr (assoc 'comm (process-attributes pid)))))
								  (list-system-processes)))
  (when (and (file-exists-p z-ahk-exe)
			 (file-exists-p z-ahk-script))
	(w32-shell-execute "open" z-ahk-exe z-ahk-script)))

(defvar z-ahk-tmp-file nil "tmp ahk file name.")
(with-temp-buffer
  (find-file "~/.emacs.d/ahk/tmp.ahk")
  (setq z-ahk-tmp-file (buffer-file-name))
  (kill-buffer))

(defvar z-ahk-tmp-sql-file nil "tmp sql file.")
(with-temp-buffer
  (find-file "~/.emacs.d/tmp.sql")
  (setq z-ahk-tmp-sql-file (buffer-file-name))
  (kill-buffer))

(defun z--write-to-file (file content)
  "Write CONTENT to file."
  (with-temp-buffer
	(find-file file)
	(erase-buffer)
	(insert content)
	(save-buffer)
	(kill-buffer)))

(defun z--run-tmp-ahk-file (&optional as-admin)
  "Run tmp ahk script."
  (w32-shell-execute (if as-admin "runas" "open")
                     z-ahk-exe
					 (z-string-win-style-path z-ahk-tmp-file)))

(defun z-run-everything-search (text)
  "Search TEXT using everything."
										;	(shell-command (concat "echo " text "| clip"))
  (kill-new text)
  (z--write-to-file z-ahk-tmp-file
					(concat "Send ^+!{e}\n"
							"Sleep 100\n"
							"Send {F3}\n"
							"Send ^{v}\n"))
  (z--run-tmp-ahk-file))

(defun z-run-everything-search-in-current-dir (text)
  "Search TEXT using everything."
  (z-run-everything-search (concat (z-string-win-style-path default-directory) " " text)))

(defun z-open-in-vs (file &optional line-number)
  "Open file in Visual Studio. If LINE-NUMBER is not nil, go to the specific line."
  (setq file (z-string-win-style-path file))
  (kill-new file)
  (z--write-to-file z-ahk-tmp-file
					(concat "WinActivate, ahk_exe devenv.exe\n"
							"WinWaitActive, ahk_exe devenv.exe\n"
							"Send ^{o}\n"
							"WinWaitActive, Open File\n"
							"Send ^{v}\n"
							"Send {Enter}\n"
							(when (and line-number (> line-number 0))
							  (concat "Send ^{g}\n"
									  "SendRaw " (format "%d" line-number) "\n"
									  "Send {Enter}\n"))))
  (z--run-tmp-ahk-file))

(defun z-open-file-at-point-in-vs ()
  "Open file at point in Visual Studio. If there is no file at point, open the buffer file in visual studio."
  (interactive)
  (let ((file))
	(setq file (if (string= major-mode "dired-mode")
				   (dired-copy-filename-as-kill 0)
				 (ffap-file-at-point)))
	(if (and file (file-exists-p file))
		(z-open-in-vs file)
	  (if (file-exists-p (buffer-file-name))
		  (let ((line-number (count-lines (point-min) (point))))
			(z-open-in-vs (buffer-file-name) line-number))
		(message "Not a file.")))))

(defun z-ssms-run-sql (sql)
  "Run SQL in SSMS."
  (when (and sql (> (length sql) 5))
	(z--write-to-file z-ahk-tmp-sql-file sql)
	(kill-new (z-string-win-style-path z-ahk-tmp-sql-file))
	(z--write-to-file
	 z-ahk-tmp-file
	 (concat "WinActivate, ahk_exe Ssms.exe\n"
			 "WinWaitActive, ahk_exe Ssms.exe\n"
			 "IfWinNotActive, " (file-name-base z-ahk-tmp-sql-file) ".sql\n"
			 "{\n"
			 "Send ^{o}\n"
			 "WinWaitActive, Open File\n"
			 "Send ^{v}\n"
			 "Send {Enter}\n"
			 "}\n"
			 "Sleep 200\n"
			 "Send {F5}\n"
			 ))
	(z--run-tmp-ahk-file)))

(defun z-ssms-run-sql-in-region ()
  "Run SQL in region."
  (interactive)
  (z-ssms-run-sql (buffer-substring-no-properties (region-beginning) (region-end))))

(defun z-run-in-cmd (cmd win-title &optional as-admin)
  "Run CMD from Corex."
  (if (z-windows-title-contains win-title)
	  (progn (kill-new cmd)
			 (z--write-to-file z-ahk-tmp-file
							   (concat "SetTitleMatchMode, 2\n"
									   "IfWinExist, " win-title "\n"
									   "{\n"
									   "WinActivate, " win-title "\n"
									   "WinWaitActive, " win-title "\n"
									   "Send ^{v}\n"
									   "Send {Enter}\n"
									   "}\n"))
			 (z--run-tmp-ahk-file as-admin))
	(message "Window '%s' not found" win-title)))

(defun z-ucmweb ()
  "Run UCMWeb from Corex."
  (interactive)
  (let ((title "UCMWeb - Microsoft Visual Studio"))
	(if (z-windows-title-contains title)
        (call-process "C:\\Users\\donzhu\\github\\tools4win\\activate_win.exe" nil 0 nil title)
	  (z-run-in-cmd "D:\\work\\UCM\\private\\UI\\UCMWeb\\UCMWeb.sln" "Corext-Based Enlistment"))))

(defun z-ucmapi ()
  "Run UcmApi from Corex."
  (interactive)
  (let ((title "UcmApi - Microsoft Visual Studio"))
	(if (z-windows-title-contains title)
        (call-process "C:\\Users\\donzhu\\github\\tools4win\\activate_win.exe" nil 0 nil title)
	  (z-run-in-cmd "D:\\work\\UCM\\private\\Service\\UcmApi\\UcmApi.sln" "Corext-Based Enlistment"))))

(defun z-ucmbasql ()
  "UCM Business Analytics SQL project."
  (interactive)
  (let ((title "UcmBusinessAnalytics -"))
    (if (z-windows-title-contains title)
        (call-process "C:\\Users\\donzhu\\github\\tools4win\\activate_win.exe" nil 0 nil title)
	  (z-run-in-cmd "\"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Enterprise\\Common7\\IDE\\devenv.exe\" D:\\work\\UCM-BusinessAnalytics\\private\\Databases\\UcmBusinessAnalytics\\UcmBusinessAnalytics.sln" "Business Analytics" t))))

(defun z-ucmbadf ()
  "UCM Business Analytics Data Factory project."
  (interactive)
  (let ((title "UcmBusinessAnalyticsDataFactories -"))
    (if (z-windows-title-contains title)
        (call-process "C:\\Users\\donzhu\\github\\tools4win\\activate_win.exe" nil 0 nil title)
	  (z-run-in-cmd "\"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Common7\\IDE\\devenv.exe\" D:\\work\\UCM-BusinessAnalytics\\private\\AzureDataFactories\\UcmBusinessAnalyticsDataFactories\\UcmBusinessAnalyticsDataFactories.sln" "Business Analytics" t))))
