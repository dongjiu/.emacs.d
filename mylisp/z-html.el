(defun z-html-wrap-line (tag)
  "Wrap current line with TAG."
  (interactive "sTag: ")
  (let ((beg))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (setq beg (point))
      (insert (concat "<" tag ">"))
      (end-of-line)
      (delete-horizontal-space)
      (insert (concat "</" tag ">"))
      (indent-region beg (point)))))
  
(defun z-html-wrap-lines-in-region (tag)
  "Wrap lines in region with TAG."
  (interactive "sTag: ")
  (let ((line-count (count-lines (region-beginning) (region-end)))
        (count 0))
    (save-excursion
      (goto-char (region-beginning))
      (while (< count line-count)
        (z-html-wrap-line tag)
        (setq count (1+ count))
        (next-line)))))

(defun z-html-wrap-region-lines (tag)
  "Wrap region lines with TAG."
  (let ((beg (region-beginning))
        (end (region-end))
        (len))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (if (save-excursion
            (beginning-of-line)
            (= end (point)))
          (goto-char (1- (point)))
        (end-of-line))
      (setq end (point))
      (setq len (- end beg))
      (goto-char beg)
      (insert (concat "<" tag ">\n"))
      (goto-char (+ (point) len))
      (insert (concat "\n</" tag ">"))
      (indent-region beg (point))
      (deactivate-mark))))

(defun z-html-region-to-ul ()
  "Convert lines in region to ul."
  (interactive)
  (z-html-wrap-lines-in-region "li")
  (z-html-wrap-region-lines "ul"))

(defun z-html-region-to-ol ()
  "Convert lines in region to ol."
  (interactive)
  (z-html-wrap-lines-in-region "li")
  (z-html-wrap-region-lines "ol"))
