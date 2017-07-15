(defun z-string-upcase-first-char (string)
  "Capitalize first character of STRING."
  (if (or (null string)
		  (= (length string) 0))
	  string
	(concat (upcase (substring string 0 1))
			(substring string 1))))
