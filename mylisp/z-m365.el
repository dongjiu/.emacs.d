(defun z-machine-cpu-counter ()
  "Open PerfSpy and search processor counter for machine."
  (interactive)
  (let ((machine-name (buffer-substring-no-properties (region-beginning) (region-end))))
    (browse-url (concat "https://osp.office.net/EDS/PerfSpyApp?team=Exchange&measureInstances=Processor%5C%25%20Processor%20Time%2C_total&machines=" machine-name))))

(defun z-exo-machine ()
  "Open OSP server page with an Exchange server."
  (interactive)
  (let ((machine-name (buffer-substring-no-properties (region-beginning) (region-end))))
    (browse-url (format "https://osp.office.net/osp/server/dropzone/status?machine=%s&environment=Prod&workload=Exchange" machine-name))))
