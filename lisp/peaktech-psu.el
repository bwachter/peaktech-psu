;;; peaktech-psu.el --- Control interface for Peaktech P 6070 lab PSU -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Bernd Wachter
;; Keywords: hardware, tools
;; Version: 0.1.0

;;; Commentary:

;; This package provides an Emacs interface for controlling the Peaktech P 6070
;; laboratory power supply via USB-serial connection.  It displays voltage and
;; current readings in a nice graphical interface and allows control of output
;; state, voltage, and current settings.
;;
;; This should also work with the P 6172 and P 6173 power supplies.
;;
;; It should be relatively simple to modify this to support some two-channel
;; power supplies as well: P 6075, P 6192 and P 6193
;;
;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup peaktech-psu nil
  "Interface for Peaktech P 6070 laboratory power supply."
  :group 'hardware
  :prefix "peaktech-psu-")

(defcustom peaktech-psu-serial-port nil
  "Serial port for the Peaktech PSU."
  :type 'string
  :group 'peaktech-psu)

(defcustom peaktech-psu-poll-interval 1.0
  "Interval in seconds between status polls."
  :type 'number
  :group 'peaktech-psu)

(defcustom peaktech-psu-debug nil
  "Enable debug messages for serial communication."
  :type 'boolean
  :group 'peaktech-psu)

(defcustom peaktech-psu-measurement-hook nil
  "Hook run after each measurement update.
Functions are called with three arguments: voltage, current, and output-state."
  :type 'hook
  :group 'peaktech-psu)

;;; Internal variables

(defvar peaktech-psu--process nil
  "Serial process for PSU communication.")

(defvar peaktech-psu--buffer nil
  "Buffer for displaying PSU interface.")

(defvar peaktech-psu--poll-timer nil
  "Timer for periodic polling.")

(defvar peaktech-psu--response-buffer ""
  "Buffer for accumulating response data.")

(defvar peaktech-psu--current-voltage 0.0
  "Current voltage reading.")

(defvar peaktech-psu--current-current 0.0
  "Current current reading (in amperes).")

(defvar peaktech-psu--output-state nil
  "Current output state (t for ON, nil for OFF).")

(defvar peaktech-psu--set-voltage 0.0
  "Last set voltage value.")

(defvar peaktech-psu--set-current 0.0
  "Last set current value.")

(defvar peaktech-psu--pending-command nil
  "Pending command waiting for response.")

;;; CRC-16/MODBUS calculation

(defun peaktech-psu--crc16-modbus (data)
  "Calculate CRC-16/MODBUS checksum for DATA (list of bytes).

Returns a cons cell (low-byte . high-byte) in LSB-first format."
  (let ((crc #xFFFF))
    (dolist (byte data)
      (setq crc (logxor crc byte))
      (dotimes (_ 8)
        (if (= (logand crc 1) 1)
            (setq crc (logxor (ash crc -1) #xA001))
          (setq crc (ash crc -1)))))
    ;; Return LSB first
    (cons (logand crc #xFF) (logand (ash crc -8) #xFF))))

(defun peaktech-psu--build-command (payload)
  "Build a complete command from PAYLOAD (list of bytes).

Appends CRC checksum and end code (0xFD)."
  (let* ((crc (peaktech-psu--crc16-modbus payload))
         (command (append payload (list (car crc) (cdr crc) #xFD))))
    command))

;;; Command builders

(defun peaktech-psu--cmd-read-values ()
  "Build command to read all PSU values."
  (peaktech-psu--build-command '(#xF7 #x01 #x03 #x04 #x03)))

(defun peaktech-psu--cmd-set-output (state)
  "Build command to set output STATE (t for ON, nil for OFF)."
  (peaktech-psu--build-command
   (if state
       '(#xF7 #x01 #x0A #x1E #x01 #x00 #x01)
     '(#xF7 #x01 #x0A #x1E #x01 #x00 #x00))))

(defun peaktech-psu--voltage-to-hex (voltage)
  "Convert VOLTAGE to hex bytes.

Returns a cons cell (high-byte . low-byte).
Example: 5.14V -> (0x02 . 0x02) = 514 in hex."
  (let ((val (round (* voltage 100))))
    (cons (logand (ash val -8) #xFF) (logand val #xFF))))

(defun peaktech-psu--current-to-hex (current)
  "Convert CURRENT to hex bytes.

Returns a cons cell (high-byte . low-byte).
Example: 0.514A -> (0x02 . 0x02) = 514 in hex."
  (let ((val (round (* current 1000))))
    (cons (logand (ash val -8) #xFF) (logand val #xFF))))

(defun peaktech-psu--cmd-set-voltage (voltage)
  "Build command to set VOLTAGE (in volts)."
  (let ((hex (peaktech-psu--voltage-to-hex voltage)))
    (peaktech-psu--build-command
     (list #xF7 #x01 #x0A #x09 #x01 (car hex) (cdr hex)))))

(defun peaktech-psu--cmd-set-current (current)
  "Build command to set CURRENT (in amperes)."
  (let ((hex (peaktech-psu--current-to-hex current)))
    (peaktech-psu--build-command
     (list #xF7 #x01 #x0A #x0A #x01 (car hex) (cdr hex)))))

;;; Response parsing

(defun peaktech-psu--hex-to-voltage (high low)
  "Convert hex bytes HIGH and LOW to voltage in volts."
  (/ (+ (ash high 8) low) 100.0))

(defun peaktech-psu--hex-to-current (high low)
  "Convert hex bytes HIGH and LOW to current in amperes."
  (/ (+ (ash high 8) low) 1000.0))

(defun peaktech-psu--parse-response (response)
  "Parse RESPONSE frame (list of bytes).

Returns a plist with :output-state, :voltage, and :current."
  (when (and (>= (length response) 14)
             (= (nth 0 response) #xF7)
             (= (nth 13 response) #xFD))
    ;; Response format: F7 01 03 04 03 [status-high] [status-low] [volt-high] [volt-low] [curr-high] [curr-low] [crc-low] [crc-high] FD
    ;; Status byte layout:
    ;; - HIGH byte (D5/byte 5): bit 5 (0x20) indicates output state
    ;; - LOW byte (D6/byte 6): unused for output state
    ;; When ON:  status-high=0x61 (0110 0001) - bit 5 set
    ;; When OFF: status-high=0x41 (0100 0001) - bit 5 clear

    (when peaktech-psu-debug
      (message "Response bytes: %s" (mapconcat (lambda (b) (format "%02X" b)) response " ")))

    (let* ((status-high (nth 5 response))
           (status-low (nth 6 response))
           ;; Check bit 5 (0x20) of status-high byte for output state
           (output-state (not (= (logand status-high #x20) 0)))
           (voltage (peaktech-psu--hex-to-voltage (nth 7 response) (nth 8 response)))
           (current (peaktech-psu--hex-to-current (nth 9 response) (nth 10 response))))
      (when peaktech-psu-debug
        (message "Parsed: status-high=%02X (bit5=%s) status-low=%02X output=%s voltage=%.2f current=%.3f"
                 status-high (if (not (= (logand status-high #x20) 0)) "1" "0")
                 status-low output-state voltage current))
      (list :output-state output-state
            :voltage voltage
            :current current))))

;;; Serial communication

(defun peaktech-psu--send-command (command)
  "Send COMMAND (list of bytes) to PSU."
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (when peaktech-psu-debug
      (message "Sending command: %s" (mapconcat (lambda (b) (format "%02X" b)) command " ")))
    (let ((str (apply #'unibyte-string command)))
      (process-send-string peaktech-psu--process str))))

(defun peaktech-psu--process-filter (_proc string)
  "Process filter for serial data from PROC.

STRING contains the received data."
  (setq peaktech-psu--response-buffer
        (concat peaktech-psu--response-buffer string))

  ;; Look for complete frame (starts with 0xF7, ends with 0xFD)
  (when (and (string-match-p "\xF7" peaktech-psu--response-buffer)
             (string-match-p "\xFD" peaktech-psu--response-buffer))
    (let* ((start (string-match "\xF7" peaktech-psu--response-buffer))
           (end (string-match "\xFD" peaktech-psu--response-buffer))
           (frame (substring peaktech-psu--response-buffer start (1+ end)))
           (bytes (mapcar (lambda (c) (if (multibyte-string-p frame)
                                          (encode-char c 'latin-1)
                                        c))
                         (string-to-list frame))))

      ;; Clear buffer after this frame
      (setq peaktech-psu--response-buffer
            (substring peaktech-psu--response-buffer (1+ end)))

      ;; Parse response
      (let ((parsed (peaktech-psu--parse-response bytes)))
        (when parsed
          (let ((new-output-state (plist-get parsed :output-state))
                (new-voltage (plist-get parsed :voltage))
                (new-current (plist-get parsed :current)))

            (when peaktech-psu-debug
              (message "Updating state: output %s->%s, voltage %.2f->%.2f, current %.3f->%.3f"
                       peaktech-psu--output-state new-output-state
                       peaktech-psu--current-voltage new-voltage
                       peaktech-psu--current-current new-current))

            (setq peaktech-psu--output-state new-output-state
                  peaktech-psu--current-voltage new-voltage
                  peaktech-psu--current-current new-current)

            ;; Run hooks
            (run-hook-with-args 'peaktech-psu-measurement-hook
                               peaktech-psu--current-voltage
                               peaktech-psu--current-current
                               peaktech-psu--output-state)

            ;; Update display
            (peaktech-psu--update-display)))))))

(defun peaktech-psu--connect (port)
  "Connect to the PSU via serial PORT."
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (delete-process peaktech-psu--process))

  (condition-case err
      (progn
        (setq peaktech-psu--process
              (make-serial-process
               :port port
               :speed 9600
               :bytesize 8
               :parity nil
               :stopbits 1
               :flowcontrol nil
               :coding 'binary
               :filter #'peaktech-psu--process-filter
               :name "peaktech-psu"
               :buffer nil))
        (setq peaktech-psu--response-buffer ""
              peaktech-psu-serial-port port)
        (message "Connected to Peaktech PSU on %s" port))
    (error
     (message "Failed to connect to PSU: %s" (error-message-string err))
     nil)))

(defun peaktech-psu--disconnect ()
  "Disconnect from the PSU."
  (when peaktech-psu--poll-timer
    (cancel-timer peaktech-psu--poll-timer)
    (setq peaktech-psu--poll-timer nil))

  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (delete-process peaktech-psu--process)
    (setq peaktech-psu--process nil))

  (message "Disconnected from Peaktech PSU"))

;;; Polling

(defun peaktech-psu--poll ()
  "Poll PSU for current values."
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (peaktech-psu--send-command (peaktech-psu--cmd-read-values))))

(defun peaktech-psu--start-polling ()
  "Start periodic polling of PSU values."
  (when peaktech-psu--poll-timer
    (cancel-timer peaktech-psu--poll-timer))

  (setq peaktech-psu--poll-timer
        (run-at-time t peaktech-psu-poll-interval #'peaktech-psu--poll))

  ;; Do initial poll
  (peaktech-psu--poll))

;;; Display

(defun peaktech-psu--create-display ()
  "Create the PSU interface display buffer."
  (let ((buf (get-buffer-create "*Peaktech PSU*")))
    (with-current-buffer buf
      (peaktech-psu-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (peaktech-psu--render-interface)))
    (setq peaktech-psu--buffer buf)
    buf))

(defun peaktech-psu--render-interface ()
  "Render the PSU interface in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")
    (insert (propertize "  PEAKTECH P 6070\n"
                       'face '(:weight bold :height 1.2)))
    (insert (propertize (make-string 45 ?═) 'face '(:foreground "gray50")))
    (insert "\n\n")

    ;; Output state indicator
    (insert "  OUTPUT: ")
    (if peaktech-psu--output-state
        (insert (propertize "● ON " 'face '(:foreground "green" :weight bold :height 1.3)))
      (insert (propertize "○ OFF" 'face '(:foreground "red" :weight bold :height 1.3))))
    (insert "\n\n")
    (insert (propertize (make-string 45 ?─) 'face '(:foreground "gray30")))
    (insert "\n\n")

    ;; Voltage display
    (insert (propertize "  VOLTAGE\n" 'face '(:foreground "cyan" :weight bold)))
    (insert (propertize (format "  %6.2f V" peaktech-psu--current-voltage)
                       'face '(:height 3.0 :weight bold :foreground "yellow")))
    (when (or (> peaktech-psu--set-voltage 0.01)
              (and (= peaktech-psu--set-voltage 0.0)
                   (> peaktech-psu--current-voltage 0.01)))
      (insert (propertize (format "  (set: %.2fV)" peaktech-psu--set-voltage)
                         'face '(:height 1.2 :foreground "gray50"))))
    (insert "\n\n")

    ;; Current display
    (insert (propertize "  CURRENT\n" 'face '(:foreground "cyan" :weight bold)))
    (insert (propertize (format "  %6.3f A" peaktech-psu--current-current)
                       'face '(:height 3.0 :weight bold :foreground "orange")))
    (when (or (> peaktech-psu--set-current 0.001)
              (and (= peaktech-psu--set-current 0.0)
                   (> peaktech-psu--current-current 0.001)))
      (insert (propertize (format "  (set: %.3fA)" peaktech-psu--set-current)
                         'face '(:height 1.2 :foreground "gray50"))))
    (insert "\n\n")
    (insert (propertize (make-string 45 ?═) 'face '(:foreground "gray50")))
    (insert "\n\n")

    ;; Controls help
    (insert (propertize "  CONTROLS\n" 'face '(:weight bold :foreground "gray70")))
    (insert "  ")
    (insert (propertize "o" 'face '(:foreground "green" :weight bold)))
    (insert " Toggle Output    ")
    (insert (propertize "v" 'face '(:foreground "green" :weight bold)))
    (insert " Set Voltage\n  ")
    (insert (propertize "c" 'face '(:foreground "green" :weight bold)))
    (insert " Set Current      ")
    (insert (propertize "d" 'face '(:foreground "green" :weight bold)))
    (insert " Toggle Debug\n  ")
    (insert (propertize "q" 'face '(:foreground "green" :weight bold)))
    (insert " Quit\n")
    (insert "\n")

    ;; Connection status
    (insert "  Status: ")
    (if (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
        (insert (propertize "Connected" 'face '(:foreground "green")))
      (insert (propertize "Disconnected" 'face '(:foreground "red"))))
    (insert (format " | Poll: %.1fs" peaktech-psu-poll-interval))
    (when peaktech-psu-debug
      (insert (propertize " | DEBUG" 'face '(:foreground "yellow" :weight bold))))
    (insert "\n"))

  (goto-char (point-min)))

(defun peaktech-psu--update-display ()
  "Update the PSU display with current values."
  (when (and peaktech-psu--buffer
             (buffer-live-p peaktech-psu--buffer))
    (with-current-buffer peaktech-psu--buffer
      (let ((pos (point)))
        (peaktech-psu--render-interface)
        (goto-char pos)))))

;;; Interactive commands

(defun peaktech-psu-toggle-debug ()
  "Toggle debug output."
  (interactive)
  (setq peaktech-psu-debug (not peaktech-psu-debug))
  (message "Peaktech PSU debug: %s" (if peaktech-psu-debug "ON" "OFF"))
  (peaktech-psu--update-display))

(defun peaktech-psu-toggle-output ()
  "Toggle PSU output on/off."
  (interactive)
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (let ((new-state (not peaktech-psu--output-state)))
      (when peaktech-psu-debug
        (message "Toggle: current-state=%s, sending command for new-state=%s"
                 peaktech-psu--output-state new-state))
      (peaktech-psu--send-command (peaktech-psu--cmd-set-output new-state))
      ;; Optimistically update the state immediately
      (setq peaktech-psu--output-state new-state)
      (peaktech-psu--update-display)
      (message "Setting output %s..." (if new-state "ON" "OFF"))
      ;; Poll after a short delay to confirm the state
      (run-at-time 0.5 nil #'peaktech-psu--poll))))

(defun peaktech-psu-set-voltage (voltage)
  "Set PSU output VOLTAGE (in volts)."
  (interactive "nSet voltage (V): ")
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (if (and (>= voltage 0) (<= voltage 60))
        (progn
          (setq peaktech-psu--set-voltage voltage)
          (peaktech-psu--send-command (peaktech-psu--cmd-set-voltage voltage))
          (message "Setting voltage to %.2fV..." voltage)
          (peaktech-psu--update-display)
          (run-at-time 0.3 nil #'peaktech-psu--poll))
      (message "Voltage must be between 0 and 60V"))))

(defun peaktech-psu-set-current (current)
  "Set PSU output CURRENT (in amperes)."
  (interactive "nSet current (A): ")
  (when (and peaktech-psu--process
             (process-live-p peaktech-psu--process))
    (if (and (>= current 0) (<= current 6))
        (progn
          (setq peaktech-psu--set-current current)
          (peaktech-psu--send-command (peaktech-psu--cmd-set-current current))
          (message "Setting current to %.3fA..." current)
          (peaktech-psu--update-display)
          (run-at-time 0.3 nil #'peaktech-psu--poll))
      (message "Current must be between 0 and 6A"))))

(defun peaktech-psu-quit ()
  "Quit the Peaktech PSU interface."
  (interactive)
  (peaktech-psu--disconnect)
  (when (and peaktech-psu--buffer
             (buffer-live-p peaktech-psu--buffer))
    (kill-buffer peaktech-psu--buffer))
  (setq peaktech-psu--buffer nil))

;;; Mode definition

(defvar peaktech-psu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'peaktech-psu-toggle-output)
    (define-key map (kbd "v") #'peaktech-psu-set-voltage)
    (define-key map (kbd "c") #'peaktech-psu-set-current)
    (define-key map (kbd "d") #'peaktech-psu-toggle-debug)
    (define-key map (kbd "q") #'peaktech-psu-quit)
    map)
  "Keymap for `peaktech-psu-mode'.")

(define-derived-mode peaktech-psu-mode special-mode "Peaktech-PSU"
  "Major mode for controlling Peaktech P 6070 laboratory power supply.

\\{peaktech-psu-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Entry point

;;;###autoload
(defun peaktech-psu (port)
  "Start the Peaktech PSU control interface on PORT.
When called interactively, prompts for the serial port device."
  (interactive
   (list (read-file-name "Serial port: " "/dev/" nil t)))
  (let ((buf (peaktech-psu--create-display)))
    (switch-to-buffer buf)
    (when (peaktech-psu--connect port)
      (peaktech-psu--start-polling))))

(provide 'peaktech-psu)
;;; peaktech-psu.el ends here
