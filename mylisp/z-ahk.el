(defvar z-ahk-tmp-file nil "tmp ahk file name.")
(with-temp-buffer
  (find-file "~/.emacs.d/tmp.ahk")
  (setq z-ahk-tmp-file (buffer-file-name))
  (kill-buffer))
  
(defun z--write-to-tmp-ahk-file (ahk-script)
  "Write AHK-SCRIPT to tmp ahk file."
  (with-temp-buffer
	(find-file z-ahk-tmp-file)
	(erase-buffer)
	(insert ahk-script)
	(save-buffer)
	(kill-buffer)))

(defun z--run-tmp-ahk-file ()
  "Run tmp ahk script."
  (call-process-shell-command
   (concat "C:\\Users\\donzhu\\softwares\\AutoHotkey_1.1.26.01\\AutoHotkeyU64.exe "
		   (replace-regexp-in-string "/" "\\\\" z-ahk-tmp-file))))

(defun z-run-everything-search (text)
  "Search TEXT using everything."
;	(shell-command (concat "echo " text "| clip"))
  (kill-new text)
  (z--write-to-tmp-ahk-file
   (concat "Send ^+!{e}\n"
		   "Sleep 100\n"
		   "Send {F3}\n"
		   "Send ^{v}\n"))
  (z--run-tmp-ahk-file))

(defun z-run-everything-search-in-current-dir (text)
  "Search TEXT using everything."
  (z-run-everything-search (concat (replace-regexp-in-string "/" "\\\\" default-directory) " " text)))

(defun z-open-in-vs (file &optional line-number)
  "Open file in Visual Studio. If LINE-NUMBER is not nil, go to the specific line."
  (setq file (replace-regexp-in-string "/" "\\\\" file))
  (kill-new file)
  (z--write-to-tmp-ahk-file
   (concat "WinActivate, ahk_exe devenv.exe\n"
		   "WinWaitActive, ahk_exe devenv.exe\n"
		   "Send ^{o}\n"
		   "WinWaitActive, Open File\n"
		   "Send ^{v}\n"
		   "Send {Enter}\n"
		   (when line-number
			 (concat "Send ^{g}\n"
					 "SendRaw " (format "%d" line-number) "\n"
					 "Send {Enter}\n"))))
  (z--run-tmp-ahk-file))

(defun z-open-file-at-point-in-vs ()
  "Open file at point in Visual Studio. If there is no file at point, open the buffer file in visual studio."
  (interactive)
  (let ((file))
	(setq file (ffap-file-at-point))
	(if (and file (file-exists-p file))
		(z-open-in-vs file)
	  (if (file-exists-p (buffer-file-name))
		  (let ((line-number (count-lines (point-min) (point))))
			(z-open-in-vs (buffer-file-name) line-number))
		(message "Not a file.")))))

(defun z-ssms-run-sql (sql)
  "Run SQL in SSMS."
  (when (and sql (> (length sql) 5))
	(kill-new sql)
	(z--write-to-tmp-ahk-file
	 (concat "WinActivate, ahk_exe Ssms.exe\n"
			 "WinWaitActive, ahk_exe Ssms.exe\n"
			 "Send ^{n}\n"
			 "Send ^{v}\n"
			 "Send {F5}\n"
			 ))
	(z--run-tmp-ahk-file)))

(defun z-ssms-run-sql-in-region ()
  "Run SQL in region."
  (interactive)
  (z-ssms-run-sql (buffer-substring-no-properties (region-beginning) (region-end))))
