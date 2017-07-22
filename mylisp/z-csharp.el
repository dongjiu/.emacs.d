(defun z-csharp-hide-methods ()
  "Hide method blocks."
  (interactive)
  (save-excursion
	(let ((stop))
	  (while (not stop)
		(if (re-search-forward "\\bclass\\b" nil t)
			(progn (search-forward "{")
				   (hs-hide-level 1)
				   (backward-char)
				   (z-goto-match-paren))
		  (setq stop t))))
	))

(defun z-csharp-parse-type ()
  "Parse type expression."
  (let ((type) (start-point) (end-point))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (setq start-point (point))
	  (forward-word)
	  (setq end-point (point))
	  (skip-chars-forward " \t\n")
	  (when (looking-at "<")
		(z-goto-match-paren)
		(forward-char)
		(setq end-point (point)))
	  (setq type (buffer-substring-no-properties start-point end-point)))
	(list 'name type
		  'start-point start-point
		  'end-point end-point)))
	  

(defun z-csharp-parse-field ()
  "Return t if text after point is a field declaration."
  (let ((type) (name) (start-point) (beg))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (setq start-point (point))
	  (while (or (looking-at "private")
				 (looking-at "internal")
				 (looking-at "public")
				 (looking-at "const")
				 (looking-at "readonly")
				 (looking-at "static"))
		(forward-word)
		(skip-chars-forward " \t\n"))
	  (when (setq type (z-csharp-parse-type))
		(goto-char (plist-get type 'end-point))
		(skip-chars-forward " \t\n")
		(setq beg (point))
		(forward-word)
		(setq name (buffer-substring-no-properties beg (point)))
		(skip-chars-forward " \t\n")
		(cond
		 ((looking-at ";")
		  (list 'type (plist-get type 'name)
				'name name
				'start-point start-point
				'end-point (point)))
		 ((looking-at "=")
		  (search-forward ";")
		  (list 'type (plist-get type 'name)
				'name name
				'start-point start-point
				'end-point (point))))))))

(defun z-csharp-parse-method ()
  "Return t if text after point is a method definition."
  (let ((type) (name) (beg) (end) (res) (start-point) (params))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (setq start-point (point))
	  (while (or (looking-at "private")
				 (looking-at "internal")
				 (looking-at "public")
				 (looking-at "const")
				 (looking-at "static"))
		(forward-word)
		(skip-chars-forward " \t\n"))
	  (when (setq type (z-csharp-parse-type))
		(goto-char (plist-get type 'end-point))
		(skip-chars-forward " \t\n")
		(setq beg (point))
		(forward-word)
		(setq end (point))
		(skip-chars-forward " \t\n")
		(when (looking-at "<")
		  (z-goto-match-paren)
		  (forward-char)
		  (setq end (point)))
		(setq name (buffer-substring-no-properties beg (point)))
		(skip-chars-forward " \t\n")
		(when (looking-at "(")
		  (setq beg (1+ (point)))
		  (z-goto-match-paren)
		  (setq end (point))
		  (setq params (z-csharp-parse-method-params beg end))
		  (forward-char)
		  (skip-chars-forward " \t\n")
		  (when (looking-at "{")
			(setq open-brace (point))
			(z-goto-match-paren)
			(list 'return-type (plist-get type 'name)
				  'name name
				  'params params
				  'start-point start-point
				  'open-brace open-brace
				  'end-point (point)
				  )))))))

(defun z-csharp-parse-ctor (class)
  "Parse constructor."
  (let ((type) (beg) (end) (start-point) (params))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (setq start-point (point))
	  (when (or (looking-at "private")
				(looking-at "internal")
				(looking-at "public"))
		(forward-word)
		(skip-chars-forward " \t\n")
		(setq beg (point))
		(forward-word)
		(setq end (point))
		(when (string= (buffer-substring-no-properties beg end) class)
		  (skip-chars-forward " \t\n")
		  (when (looking-at "(")
			(setq beg (1+ (point)))
			(z-goto-match-paren)
			(setq end (point))
			(setq params (z-csharp-parse-method-params beg end))
			(forward-char)
			(skip-chars-forward " \t\n")
			(when (looking-at "{")
			  (setq open-brace (point))
			  (z-goto-match-paren)
			  (list 'return-type (plist-get type 'name)
					'params params
					'start-point start-point
					'open-brace open-brace
					'end-point (point)
					))))))))

(defun z-csharp-parse-method-params (beg end)
  "Parse method parameters."
  (let ((params) (stop) (type-beg) (type-end) (type) (name-beg) (name))
	(save-excursion
	  (goto-char beg)
	  (unless (looking-at "\\s-*)")
		(while (not stop)
		  (skip-chars-forward " \t\n")
		  (setq type-beg (point))
		  (forward-word)
		  (setq type-end (point))
		  (skip-chars-forward " \t\n")
		  (when (looking-at "<")
			(z-goto-match-paren)
			(forward-char)
			(setq type-end (point)))
		  (setq type (buffer-substring-no-properties type-beg type-end))
		  (skip-chars-forward " \t\n")
		  (setq name-beg (point))
		  (forward-word)
		  (setq name (buffer-substring-no-properties name-beg (point)))

		  (setq params (cons (list 'type type
								   'name name)
							 params))
		  (skip-chars-forward " \t\n")
		  (cond
		   ((>= (point) end)
			(setq stop t))

		   ((looking-at ",")
			(forward-char))

		   ((looking-at "=")
			(unless (search-forward "," end t)
			  (setq stop t)))

		   (t (setq stop t))))))
	params))


(defun z-csharp-parse-comments ()
  "Parse comments."
  (let ((start-point) (end-point))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (cond
	   ((looking-at "//")
		(setq start-point (point))
		(end-of-line)
		(setq end-point (point))
		(list 'start-point start-point
			  'end-point end-point))

	   ((looking-at "/*")
		(setq start-point (point))
		(when (search-forward "*/" nil t)
		  (setq end-point (point))
		  (list 'start-point start-point
				'end-point end-point)))

	   (t nil)))))
	
(defun z-csharp-parse-property ()
  "Return t if text after point is a property definition."
  (let ((type) (name) (beg) (end) (res) (start-point))
	(save-excursion
	  (skip-chars-forward " \t\n")
	  (setq start-point (point))
	  (while (or (looking-at "private")
				 (looking-at "internal")
				 (looking-at "public")
				 (looking-at "const")
				 (looking-at "static"))
		(forward-word)
		(skip-chars-forward " \t\n"))
	  (when (setq type (z-csharp-parse-type))
		(goto-char (plist-get type 'end-point))
		(skip-chars-forward " \t\n")
		(setq beg (point))
		(forward-word)
		(setq name (buffer-substring-no-properties beg (point)))
		(skip-chars-forward " \t\n")
		(when (looking-at "{")
		  (z-goto-match-paren)
		  (list 'type (plist-get type 'name)
				'name name
				'start-point start-point
				'end-point (point)))))))

(defun z-csharp-parse-next-class ()
  "Return special points of next class."
  (let ((class-start) (class-name) (open-brace) (close-brace) (beg) (end) (parse-res) (fields) (props) (ctors) (methods) (stop))
	(save-excursion
	  (when (re-search-forward "\\bclass\\b" nil t)
		(skip-chars-forward " \t\n")
		(setq beg (point))
		(forward-word)
		(setq end (point))
		(setq class-name (buffer-substring-no-properties beg end))

		(beginning-of-line)
		(skip-chars-forward " \t\n")
		(setq class-start (point))

		(search-forward "{")
		(backward-char)
		(setq open-brace (point))
		(z-goto-match-paren)
		(setq close-brace (point))

		(goto-char (1+ open-brace))
		(skip-chars-forward " \t\n")

		(while (not stop)
		  (cond
		   ((setq parse-res (z-csharp-parse-field))
			(setq fields (cons parse-res fields))
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-property))
			(setq props (cons parse-res props))
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-method))
			(setq methods (cons parse-res methods))
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-ctor class-name))
			(setq ctors (cons parse-res ctors))
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-comments))
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   (t (setq stop t))
		   ))

		(list 'class-start class-start
			  'class-name class-name
			  'open-brace open-brace
			  'close-brace close-brace
			  'fields fields
			  'props props
			  'ctors ctors
			  'methods methods)))))

(defun z-csharp-parse-file ()
  "Parse current cs file."
  (let ((stop) (class) (classes))
	(save-excursion
	  (goto-char (point-min))
	  (while (setq class (z-csharp-parse-next-class))
		(setq classes (cons class classes))
		(goto-char (1+ (plist-get class 'close-brace)))))
	classes))

(defun z-csharp-parse-file-i ()
  ""
  (interactive)
  (insert (format "%s" (z-csharp-parse-file))))

(defun z-csharp-parse-type-i ()
  ""
  (interactive)
  (insert (format "%s" (z-csharp-parse-comments))))

