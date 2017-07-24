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

(defun z-csharp-skip-attribute ()
  "Skip c# attribute."
  (skip-chars-forward " \t\n")
  (when (looking-at "\\[")
	(z-goto-match-paren)
	(forward-char)
	(skip-chars-forward " \t\n")
	))

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
		  (z-csharp-skip-attribute)
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

		  (push (list 'type type
					  'name name)
				params)
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
		  (z-csharp-skip-attribute)
		  (cond
		   ((setq parse-res (z-csharp-parse-field))
			(push parse-res fields)
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-property))
			(push parse-res props)
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-method))
			(push parse-res methods)
			(goto-char (1+ (plist-get parse-res 'end-point))))

		   ((setq parse-res (z-csharp-parse-ctor class-name))
			(push parse-res ctors)
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
		(push class classes)
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

(defun z-csharp--current-class ()
  "Return the current class."
  (let ((class) (classes (z-csharp-parse-file)))
	(dolist (c classes)
	  (when (and (<= (plist-get c 'class-start) (point))
				 (>= (plist-get c 'close-brace) (point)))
		(setq class c)))
	class))

(defun z-csharp--current-member (members)
  "Return current member."
  (let ((member))
	(dolist (m members)
	  (when (and (<= (plist-get m 'start-point) (point))
				 (>= (plist-get m 'end-point) (point)))
		(setq member m)))
	member))

(defun z-csharp--next-member (members)
  "Return next member."
  (let ((member) (start) (dis) (distance (point-max)))
	(dolist (m members)
	  (setq start (plist-get m 'start-point))
	  (when (> start (point))
		(setq dis (- start (point)))
		(when (< dis distance)
		  (setq distance dis)
		  (setq member m))))
	member))

(defun z-csharp--prev-member (members)
  "Return previous member."
  (let ((member) (end) (dis) (distance (point-max)))
	(dolist (m members)
	  (setq end (plist-get m 'end-point))
	  (when (< end (point))
		(setq dis (- (point) end))
		(when (< dis distance)
		  (setq distance dis)
		  (setq member m))))
	member))

(defun z-csharp-goto-next-member (&optional n)
  "Go to next member."
  (interactive "p")
  (let ((class) (members)
		(current-member) (prev-member) (next-member)
		(member) (end) (distance (point-max)))
	(setq class (z-csharp--current-class))

	(dolist (m (plist-get class 'methods))
	  (push m members))
			
	(dolist (f (plist-get class 'fields))
	  (push f members))

	(dolist (p (plist-get class 'props))
	  (push p members))

	(sort members (lambda (lhs rhs) (< (plist-get lhs 'start-point)
									   (plist-get rhs 'start-point))))

	(setq current-member (z-csharp--current-member members))
	(if current-member
		(setq member (nth (+ (cl-position current-member members) n) members))
	  (if (> n 0)
		  (when (setq next-member (z-csharp--next-member members))
			(setq member (nth (+ (cl-position next-member members) (1- n)) members)))
		(when (setq prev-member (z-csharp--prev-member members))
		  (setq member (nth (+ (cl-position prev-member members) (1+ n)) members)))))
	
	(when member
	  (goto-char (plist-get member 'start-point)))))
