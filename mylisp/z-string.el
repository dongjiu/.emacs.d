(defun z-string-upcase-first-char (string)
  "Convert STRING to Pascal case."
  (if (or (null string)
		  (= (length string) 0))
	  string
	(concat (upcase (substring string 0 1))
			(substring string 1))))
