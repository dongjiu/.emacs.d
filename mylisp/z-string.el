(defun z-string-upcase-first-char (string)
  "Capitalize first character of STRING."
  (if (or (null string)
		  (= (length string) 0))
	  string
	(concat (upcase (substring string 0 1))
			(substring string 1))))

(defun z-string-win-style-path (path)
  "Convert PATH to Windows style path."
  (replace-regexp-in-string "/" "\\\\" (z-string-upcase-first-char (expand-file-name path))))
