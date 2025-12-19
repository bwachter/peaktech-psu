;;; peaktech-psu-eplot.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Bernd Wachter
;; Keywords: tools, plot

;;; Commentary:
;;
;; Hook to plot current and voltage over time via eplot. Loading this file
;; registers the hook, the actual data logging can be controlled with
;; peaktech-psu-eplot-start and peaktech-psu-eplot-stop.
;;
;; Control when to generate the graphs with peaktech-psu-eplot-at.
;;
;;; Code:

(require 'pcsv)
(require 'eplot)
(require 'peaktech-psu)

(defvar-local peaktech-psu--elapsed-time 0
  "Buffer local variable for tracking timestamps")

(defvar peaktech-psu-eplot-active nil
  "Track if we should be plotting")

(defcustom peaktech-psu-eplot-at nil
  "Variable to control when the plot should be generated

nil never
t always
number when the elapsed time evenly divides by `number'"
  :type '(choice
          (const :tag "True" t)
          (const :tag "False" nil)
          (number :tag "Number"))
  :group 'peaktech-psu)

(defcustom peaktech-psu-eplot-data-buffer "*peaktech-psu-eplot-data*"
  "The buffer name holding plot data"
  :type 'string
  :group 'peaktech-psu)

(defun peaktech-psu-eplot-should-run-p (counter)
  "Return t if eplot should be called based on `peaktech-psu-eplot-at'."
  (cond
   ((null peaktech-psu-eplot-at) nil)
   ((eq peaktech-psu-eplot-at t) t)
   ((numberp peaktech-psu-eplot-at) (and (> peaktech-psu-eplot-at 0)
                                        (= (mod counter peaktech-psu-eplot-at) 0)))
   (t nil)))

(defun peaktech-psu-eplot-hook (voltage current state)
  "Hook to log periodic data from PSU"
  (when (and peaktech-psu-eplot-active state)
    (condition-case err
        (peaktech-psu-eplot-add-measurement voltage current)
      (error
       (setq peaktech-psu-eplot-active nil)
       (message "Error in peaktech-psu-eplot-hook: %S. Disabling active state." err)
       (debug))))
  nil)

(defun peaktech-psu-eplot-init ()
  "Init the plot data buffer"
  (interactive)
  (with-current-buffer (get-buffer-create peaktech-psu-eplot-data-buffer)
    (erase-buffer)
    (setq peaktech-psu--elapsed-time 0)
    (insert "Time,Voltage,Current\n")))

(defun peaktech-psu-eplot-start()
  "Start plotting"
  (interactive)
  (peaktech-psu-eplot-init)
  (setq peaktech-psu-eplot-active t))

(defun peaktech-psu-eplot-stop()
  "Start plotting"
  (interactive)
  (setq peaktech-psu-eplot-active nil))

(defun peaktech-psu-eplot-add-measurement (voltage current)
  "Add a new data line to the plot data buffer"
  (interactive)
  (with-current-buffer (get-buffer-create peaktech-psu-eplot-data-buffer)
    (goto-char (point-max))
    (insert (format "%s,%s,%s\n" peaktech-psu--elapsed-time voltage current))
    (setq peaktech-psu--elapsed-time (1+ (or peaktech-psu--elapsed-time 0)))
    (when (peaktech-psu-eplot-should-run-p peaktech-psu--elapsed-time)
      (eplot))))

(add-hook 'peaktech-psu-measurement-hook #'peaktech-psu-eplot-hook)

(provide 'peaktech-psu-eplot)

;;; peaktech-psu-eplot.el ends here
