(setq jmm-workout-log-directory "~/projects/fitness/workouts/")

(defun datestamp-jmm ()
  (format-time-string "%Y%m%d")
)

(defun next-workout-log-file-jmm (filename)
  (interactive
   (let ((default-log-file-name (format "%s%s.txt" jmm-workout-log-directory (datestamp-jmm))))
   (list (read-file-name "Create log file: "
			 default-log-file-name
			 default-log-file-name
			 nil))))
  (let ((value (find-file-noselect filename nil nil)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))


(defun insert-date ()
  (interactive)
  (insert (datestamp-jmm))
  )


