;;; z-reminder.el --- remind you to take a break

;;; Commentary:

;; Don't stay in front of the computer for a long time.
;; This package keeps track of the time you spent in Emacs
;; and remind you to take a break.

;;; Code:

(defun z-reminder-popup (msg &optional interval)
  "Show the message in other window.
MSG is the message to be shown.
The message window will disappera after INTERVAL seconds.  INTERVAL is optional with default value 5."
  (let ((obuffer (current-buffer))
		(popup-buffer (get-buffer-create "*z-reminder*")))
	(set-buffer popup-buffer)
	(message-mode)
	(setq buffer-read-only nil)
	(end-of-buffer)
	(newline)
	(insert (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)))
	(insert (propertize msg 'font-lock-face '(:foreground "red")))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(set-buffer obuffer)
	(display-buffer popup-buffer)
	(unless interval (setq interval 5))
	(setq z-reminder-timer
		  (run-with-timer interval nil (lambda (buf) (delete-windows-on buf)) popup-buffer))
	))

(defun z-reminder--handler (first-alert-secs subseq-alert-secs)
  "Timer handler.
After FIRST-ALERT-SECS seconds, the first alert will occur.  Subseqent alert interval is specified by SUBSEQ-ALERT-SECS."
  (let ((elapsed-secs (- (time-to-seconds (current-time))
						 (time-to-seconds z-reminder-last-time)))
		(interval))
	(setq minutes (/ elapsed-secs 60))
	(z-reminder-popup (format "你已经连续在线%d分钟" minutes))
	(if (> elapsed-secs first-alert-secs)
		(setq interval subseq-alert-secs)
	  (setq interval first-alert-secs))
	(setq z-reminder-timer
	  (run-with-timer interval nil 'z-reminder--handler first-alert-secs subseq-alert-secs))
	))

(defun z-reminder--clear ()
  "Clear registered timers."
  (dolist (timer timer-list)
	(if (eq (elt timer 5) 'z-reminder--handler)
		(cancel-timer timer))))

(defun z-reminder-start (&optional silent)
  "Start reminder.
The message window will not be shown if the optional argument SILENT is t."
  (interactive)
  (z-reminder--clear)
  (setq z-reminder-last-time (current-time))
  (setq z-reminder-timer
		(run-with-timer 2400 nil 'z-reminder--handler 2400 60))
  (unless silent (z-reminder-popup "开始计时")))

(defun z-reminder-stop ()
  "Stop reminder."
  (interactive)
  (z-reminder--clear)
  (z-reminder-popup "结束计时"))

(defun z-reminder-report ()
  "Report elapsed time."
  (interactive)
  (let ((elapsed-secs (- (time-to-seconds (current-time))
						 (time-to-seconds z-reminder-last-time))))
		(z-reminder-popup (format "你已经连续在线%d分钟" (/ elapsed-secs 60)))))

;(z-reminder-start)

;(setq timer-list nil)

(provide 'z-reminder)

;;; z-reminder.el ends here
