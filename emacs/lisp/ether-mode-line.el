;;; ether-mode-line.el --- Inspired by minibuffer-line and feebleline -*- lexical-binding: t -*-

;;; Commentary:

;; Displays a configurable mode-line in the minibuffer.
;;
;; Features:
;;  - periodically refreshes after `ether-mode-line-refresh-interval', similarly
;;    to feebleline and minibuffer-line (but only in case of new content)
;;  - auto-hides the echo area if unmodified after `ether-mode-line-echo-duration' seconds
;;  - still tries to show a horizontal delimiter between windows defined
;;    by `ether-mode-line-format'
;;  - redefine `ether-mode-line-hook' to change the mode-line content,
;;    for example `current-time-string'
;;  - should better handle random stuff appearing in the minibuffer
;;
;; feebleline: https://github.com/tautologyclub/feebleline
;; minibuffer-line: http://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/packages/minibuffer-line

;;; Code:

(require 'cl-lib)

(defgroup ether-mode-line nil
  "Unobtrusive mode-line."
  :group 'ether-mode-line)

(defcustom ether-mode-line-format "%-"
  "`mode-line-format' used as separator between windows."
  :type 'string
  :group 'ether-mode-line)

(defcustom ether-mode-line-refresh-interval .1
  "Interval between each attempted refresh of the mode-line."
  :type 'float
  :group 'ether-mode-line)

(defcustom ether-mode-line-echo-duration 3.
  "How long the echo area content will be displayed."
  :type 'float
  :group 'ether-mode-line)

(defcustom ether-mode-line-hook
  (lambda ()
    (concat
     ;; standard stuff
     (format-mode-line "%l,%c") " "
     (propertize (ether-mode-line--major-mode) 'face 'mode-line) " "
     ;; buffer indication
     (propertize
      (if (buffer-file-name) (buffer-file-name) (buffer-name))
      'face (if (and (buffer-file-name) (buffer-modified-p))
                'warning 'mode-line-buffer-id))
     ;; irc
     (when (ether-mode-line--circe-tracking-buffers)
       (concat " " (ether-mode-line--circe-tracking-buffers)))))
  "Return a string to be used as the mode-line."
  :type 'function
  :group 'ether-mode-line)

(defcustom ether-mode-line--debug nil
  "Enable debug logging."
  :type 'bool
  :group 'ether-mode-line)

(defun ether-mode-line--log (string &rest objects)
  "Log a message to stderr to avoid flushing the echo area.
STRING and OBJECTS are passed to `format'."
  (when ether-mode-line--debug
    (princ (apply #'format (cons (concat string "\n") objects)) #'external-debugging-output)))

(defvar ether-mode-line--last-error nil
  "Last internal error encountered.")

(defvar ether-mode-line--echo-content ""
  "Last content of the echo area.")

(defvar ether-mode-line--echo-content-count 0
  "How many times the echo area content has been displayed instead of the mode-line.")

(defvar ether-mode-line--mode-line-content nil
  "Last content of the mode-line.")

(defun ether-mode-line--insert (&optional resize)
  "Update the displayed mode-line.
RESIZE indicates a spurious call due to window resize."
  (when (not resize) ; a resize should not increment any timer
    (let ((content (current-message)))
      (if (and content
               (equal-including-properties ether-mode-line--echo-content content))
          (cl-incf ether-mode-line--echo-content-count) ; same displayed content, display timer continues
        (setq ether-mode-line--echo-content-count 0)) ; content changed, display timer resets
      (setq ether-mode-line--echo-content content)))

  (when (>= (* ether-mode-line-refresh-interval ether-mode-line--echo-content-count)
            ether-mode-line-echo-duration)
    (ether-mode-line--log "timeouting echo area")
    (setq ether-mode-line--echo-content nil
          ether-mode-line--echo-content-count 0
          ether-mode-line--mode-line-content nil) ; force redraw
    (message nil)) ; this is the correct way to flush the echo area

  (unless (current-message) ; no content displayed, show the mode-line
    (condition-case err
        (let ((content (funcall ether-mode-line-hook)))
          (when (or resize ; a change in windows should force a resize of the minibuffer
                    (not (equal-including-properties ether-mode-line--mode-line-content content))) ; avoid unnecessary updates
            (setq ether-mode-line--mode-line-content content)
            (with-selected-window (minibuffer-window)
              (erase-buffer)
              (insert content)
              (let ((lines (ceiling (/ (string-width content) (float (window-total-width))))))
                ;; this is dubious at best (it won't properly handle some characters)
                ;; but fit-window-to-buffer, count-screen-line, ivy--resize-minibuffer-to-fit, ...
                ;; all have problems
                (ether-mode-line--log "resize minibuffer for %d lines" lines)
                (window-resize nil (- lines (window-total-height)) nil t)))))
      (text-read-only (ether-mode-line--log "minibuffer content is read-only")) ; skip when prompting
      (error (unless (equal err ether-mode-line--last-error) ; keep track of internal errors so we don't spam
               (setq ether-mode-line--last-error err)
               (message "ether-mode-line: %s" err))))))

(defmacro ether-mode-line--with-walk-windows (frame &rest body)
  "Execute BODY in each window of FRAME that isn't the minibuffer."
  `(walk-windows (lambda (window)
                   (with-selected-window window
                     ,@body))
                 0 frame)) ; don't include minibuffer

(defun ether-mode-line--window-size-change-hook (frame)
  "Selectively show the regular mode-line as an horizontal separator to tell buffers in FRAME apart."
  ;; https://emacs.stackexchange.com/questions/30513/making-a-variable-window-local
  ;; https://github.com/tautologyclub/feebleline/issues/24
  (ether-mode-line--with-walk-windows frame (setq mode-line-format nil)) ; reset the mode line of each buffer
  (ether-mode-line--with-walk-windows frame
   ;; {frame,window}-edges functions return (x1 y1 x2 y2)
   ;; check if the current buffer (which may be displayed on mutltiple windows) doesn't sit on top of the minibuffer
   ;; this isn't perfect but unfortunately, the mode-line-format is buffer-local and not window-local
   (when (/= (nth 1 (window-edges (minibuffer-window)))
             (nth 3 (window-edges)))
     (setq mode-line-format ether-mode-line-format)))
  (ether-mode-line--log "windows resized")
  (ether-mode-line--insert t))

;;;###autoload
(define-minor-mode ether-mode-line-mode
  "Toggle ether-mode-line on or off.
No effort made to restore previous settings..."
  :group 'ether-mode-line
  :global t
  (if ether-mode-line-mode
      (progn
        (add-hook 'window-size-change-functions #'ether-mode-line--window-size-change-hook)
        (setq ether-mode-line--timer (run-with-timer 0 ether-mode-line-refresh-interval
                                                     #'ether-mode-line--insert))
        (setq-default mode-line-format nil)) ; necessary or may reappear in some cases, like revert-buffer
    (cancel-timer ether-mode-line--timer)
    (remove-hook 'window-size-change-functions #'ether-mode-line--window-size-change-hook)))

(defun ether-mode-line--circe-tracking-buffers ()
  "Circe maintains a list of buffers with activity.
Join them together as they are already propertized in case of highlight."
  (when (boundp 'tracking-buffers)
    (cl-flet ((filter (buffer)
                      (text-property-any 0 1 'face 'circe-highlight-nick-face buffer)))
      (string-join (append ; put the highlighted buffers first
                    (sort (seq-filter #'filter tracking-buffers) 'string-collate-lessp)
                    (sort (seq-filter (lambda (buffer) (not (filter buffer))) tracking-buffers) 'string-collate-lessp))
                   " "))))

(defun ether-mode-line--major-mode ()
  "Preferable to `format-mode-line''s %m."
  (string-trim-right (prin1-to-string major-mode) "-mode"))

(provide 'ether-mode-line)
;;; ether-mode-line.el ends here
