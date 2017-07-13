(defun z-git-merge-next-conflict ()
  "Go to next conflict."
  (interactive)
  (search-forward "<<<<<<<")
  (beginning-of-line))

(defun z-git-merge-previous-conflict ()
  "Go to previous conflict."
  (interactive)
  (search-backward "<<<<<<<")
  (beginning-of-line))

(defun z-git-merge--resolve (resolve-method)
  "Resolve next merge conflicts.
If RESOLVE-METHOD is \"FIRST\", take the first version.
If RESOLVE-METHOD is \"SECOND\", take the second version.
If RESOLVE-METHOD is \"BOTH\", take both versions."
  (let ((marker1-beg)
		(marker1-end)
		(marker2-beg)
		(marker2-end)
		(marker3-beg)
		(marker3-end))
	(save-excursion
	  (goto-char (point-min))
	  (search-forward "<<<<<<<")
	  (beginning-of-line)
	  (setq marker1-beg (point))
	  (forward-line)
	  (setq marker1-end (point))
	  (search-forward "=======")
	  (beginning-of-line)
	  (setq marker2-beg (point))
	  (forward-line)
	  (setq marker2-end (point))
	  (search-forward ">>>>>>>")
	  (beginning-of-line)
	  (setq marker3-beg (point))
	  (forward-line)
	  (setq marker3-end (point))
	  (cond
	   ((string= resolve-method "FIRST")
		(delete-region marker2-beg marker3-end)
		(delete-region marker1-beg marker1-end))
	   ((string= resolve-method "SECOND")
		(delete-region marker3-beg marker3-end)
		(delete-region marker1-beg marker2-end))
	   ((string= resolve-method "BOTH")
		(delete-region marker3-beg marker3-end)
		(delete-region marker2-beg marker2-end)
		(delete-region marker1-beg marker1-end)))
	(goto-char marker1-beg))))

(defun z-git-merge-take-first ()
  "Use first version to resolve next merge conflict."
  (interactive)
  (z-git-merge--resolve "FIRST"))

(defun z-git-merge-take-second ()
  "Use second version to resolve next merge conflict."
  (interactive)
  (z-git-merge--resolve "SECOND"))

(defun z-git-merge-take-both ()
  "Use both versions to resolve next merge conflict."
  (interactive)
  (z-git-merge--resolve "BOTH"))
