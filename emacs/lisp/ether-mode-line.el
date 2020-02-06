;;; ether-mode-line.el --- Inspired by {mini{-mode,buffer-},feeble}line -*- lexical-binding: t -*-

;;; Commentary:

;; Displays a configurable mode-line in the minibuffer.
;;
;; Features:
;;  - periodically refreshes after `ether-mode-line-refresh-interval', similarly
;;    to feebleline and minibuffer-line but only in case of new content
;;    and without the introduced latency
;;  - auto-hides the echo area if unmodified after
;;    `ether-mode-line-echo-duration' seconds
;;  - still tries to show a horizontal delimiter between windows defined
;;    by `ether-mode-line-format'
;;  - redefine `ether-mode-line-hook' to change the mode-line content,
;;    for example `current-time-string'
;;  - should better handle random stuff appearing in the minibuffer
;;
;; Known issues:
;;  - some commands like `isearch-forward` don't protect the minibuffer
;;    so they'll be hidden after `ether-mode-line-echo-duration' seconds
;;  - the mode-line isn't window-local so some horizontal borders may be
;;    superfluous
;;
;; feebleline: https://github.com/tautologyclub/feebleline
;; minibuffer-line: http://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/packages/minibuffer-line
;; mini-modeline: https://github.com/kiennq/emacs-mini-modeline

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup ether-mode-line nil
  "Unobtrusive mode-line."
  :group 'ether-mode-line)

(defcustom ether-mode-line-format "%-"
  "`mode-line-format' used as separator between windows."
  :type 'string
  :group 'ether-mode-line)

(defcustom ether-mode-line-refresh-interval .5
  "Interval between each attempted refresh of the mode-line or nil to disable."
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
     (when (> (length (ether-mode-line--circe-tracking-buffers)) 0)
       (concat " " (ether-mode-line--circe-tracking-buffers)))
     (when (fboundp 'circe-lagmon-format-mode-line-entry)
       (concat " " (string-trim (circe-lagmon-format-mode-line-entry))))))
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

(defvar ether-mode-line--last-message nil
  "Last content of the echo area.")

(defvar ether-mode-line--last-message-time 0
  "Last display time of the echo area.")

(defvar ether-mode-line--last-mode-line nil
  "Last content of the mode-line.")

(defun ether-mode-line--insert (origin)
  "Update the displayed mode-line.
ORIGIN may be either 'window for a window configuration change, 'message for
a new message, 'timer for a tick or 'command after a command."
  (let ((message (current-message))) ; on empty message should return nil
    ;; bookkeeping
    (when (and message (not (equal-including-properties ether-mode-line--last-message message)))
      (ether-mode-line--log "echo area content changed: %S" message)
      (setq ether-mode-line--last-message message)
      (setq ether-mode-line--last-message-time (float-time)))
    (when (and (> ether-mode-line--last-message-time 0)
               (>= (- (float-time) ether-mode-line--last-message-time) ether-mode-line-echo-duration))
      (ether-mode-line--log "echo area content displayed for too long")
      (setq ether-mode-line--last-message nil
            ether-mode-line--last-message-time 0
            ether-mode-line--last-mode-line nil ; flush cache used below
            message nil))
    ;; mode-line
    (unless message ; no content displayed, show the mode-line
      (condition-case err
          (let ((mode-line (funcall ether-mode-line-hook)))
            (if (not (or (memq origin '(message ; a reset message should force a redisplay
                                        window)) ; a window change should force a resize
                         (not ; try to avoid unnecessary updates
                          (equal-including-properties ether-mode-line--last-mode-line mode-line))))
                (ether-mode-line--log "keep cached content of minibuffer: %S" mode-line)
              (setq ether-mode-line--last-mode-line mode-line)
              (with-selected-window (minibuffer-window)
                (erase-buffer)
                (insert mode-line)
                (let ((lines (ceiling (/ (string-width mode-line) (float (window-total-width))))))
                  ;; this is dubious at best (it won't properly handle some characters)
                  ;; but fit-window-to-buffer, count-screen-line,
                  ;; ivy--resize-minibuffer-to-fit, ... all have problems
                  (ether-mode-line--log "insert %d lines into minibuffer: %S" lines mode-line)
                  (window-resize nil (- lines (window-total-height)) nil t))) ; calls the hook...
              ;; it may be necessary to flush the echo area sometimes
              (unless (memq origin '(command window)) ; unneeded and flickers
                (message 'ether-mode-line--message-flush))))
        (text-read-only (ether-mode-line--log "minibuffer content is read-only")) ; skip when prompting
        (error (unless (equal ether-mode-line--last-error err) ; keep track of internal errors so we don't spam
                 (setq ether-mode-line--last-error err)
                 (message "ether-mode-line: %S" err)))))))

(defmacro ether-mode-line--with-walk-windows (frame &rest body)
  "Execute BODY in each window of FRAME that isn't the minibuffer."
  `(walk-windows (lambda (window)
                   (with-selected-window window
                     ,@body))
                 0 frame)) ; don't include minibuffer

(cl-defun ether-mode-line--window-size-change-hook (&optional (frame (selected-frame)))
  "Selectively show the regular mode-line as an horizontal separator to tell buffers in FRAME apart."
  ;; https://emacs.stackexchange.com/questions/30513/making-a-variable-window-local
  ;; https://github.com/tautologyclub/feebleline/issues/24
  (ether-mode-line--with-walk-windows
   frame ; reset the mode line of each buffer
   (setq mode-line-format nil))
  (ether-mode-line--with-walk-windows
   frame
   ;; check if the current buffer (which may be displayed on mutltiple windows)
   ;; doesn't sit on top of the minibuffer this isn't perfect but unfortunately,
   ;; the mode-line-format is buffer-local and not window-local
   ;; {frame,window}-edges functions return (x1 y1 x2 y2)
   (when (/= (nth 1 (window-edges (minibuffer-window)))
             (nth 3 (window-edges)))
     (setq mode-line-format ether-mode-line-format)))
  (ether-mode-line--insert 'window))

(defun ether-mode-line--message (f &rest args)
  "An advice of `message' so some spurious stuff can be debounced.
ARGS are formatting arguments passed to F."
  ;; displaying empty messages too frequently will result in flickering
  ;; unfortunately some modes do that, like some lsp implementations when
  ;; they try to display empty help messages...
  ;; maybe this will break some stuff that was relying on it?
  (catch 'return ; simulate proper return value
    (when (and (= (length args) 1)
               (eq (nth 0 args) 'ether-mode-line--message-flush))
      ;; when the mode-line changes, flush any existing message
      ;; unfortunately some modes are misbehaving and will message soon after
      ;; but I don't want to go down the rabbit hole and try to fix unrelated
      ;; code here
      (ether-mode-line--log "flushing message")
      (throw 'return (apply f '(nil))))
    (when (and (= (length args) 1) (eq (nth 0 args) nil))
      (ether-mode-line--log "discarding nil message")
      (throw 'return nil))
    (when (equal-including-properties (apply 'format args) "")
      (ether-mode-line--log "discarding empty message")
      (throw 'return ""))
    (let ((result (apply f args)))
      (ether-mode-line--insert 'message)
      result)))

(defun ether-mode-line--insert-command-hook ()
  "Wrapper for using `ether-mode-line--insert' as hook."
  (ether-mode-line--insert 'command))

(defvar ether-mode-line--timer nil
  "Holds the timer, makes the linter happy.")

;;;###autoload
(define-minor-mode ether-mode-line-mode
  "Toggle ether-mode-line on or off.
No effort made to restore previous settings..."
  :group 'ether-mode-line
  :global t
  (if ether-mode-line-mode
      (progn
        (advice-add 'message :around #'ether-mode-line--message)
        (add-hook 'window-size-change-functions 'ether-mode-line--window-size-change-hook)
        ;; the iteration over the windows for each command is required in this case:
        ;; C-h f describe-function *scratch*
        ;; advice-add
        ;; C-x o other-window *scratch*
        ;; C-x 2 split-window-below *Help*
        ;; C-x o other-window *Help*
        ;; C-h f describe-function *Help*
        ;; add-function
        ;; RET ivy-alt-done  *Minibuf-1*
        (add-hook 'post-command-hook 'ether-mode-line--window-size-change-hook)
        (add-hook 'post-command-hook 'ether-mode-line--insert-command-hook)
        (setq ether-mode-line--timer (run-with-timer 0 ether-mode-line-refresh-interval
                                                     #'ether-mode-line--insert 'timer))
        (setq-default mode-line-format nil)) ; necessary or may reappear in some cases, like revert-buffer
    (cancel-timer ether-mode-line--timer)
    (remove-hook 'post-command-hook 'ether-mode-line--insert-command-hook)
    (remove-hook 'post-command-hook 'ether-mode-line--window-size-change-hook)
    (remove-hook 'window-size-change-functions 'ether-mode-line--window-size-change-hook)
    (advice-remove 'message #'ether-mode-line--message)))

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
