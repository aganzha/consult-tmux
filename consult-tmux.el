;;; consult-tmux.el --- Connect to tmux sessions from Emacs via consult -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `consult-tmux', which lists active tmux sessions in the
;; minibuffer (consult/vertico) and attaches to the chosen one in a
;; dedicated vterm buffer.  Reuses an existing buffer if the session
;; is already attached.

;;; Code:

(require 'consult)
(require 'vterm)

(defgroup consult-tmux nil
  "Connect to tmux sessions via consult."
  :group 'convenience)

(defcustom consult-tmux-buffer-prefix "*vterm-tmux-"
  "Prefix for vterm buffers created by `consult-tmux'.
Full buffer name is PREFIX + session name + \"*\"."
  :type 'string
  :group 'consult-tmux)

(defcustom consult-tmux-format
  "#{session_name} #{pane_current_path}"
  "tmux format string used to list panes."
  :type 'string
  :group 'consult-tmux)

(defcustom consult-tmux-prompt "tmux session: "
  "Minibuffer prompt."
  :type 'string
  :group 'consult-tmux)

(defcustom consult-tmux-category 'consult-tmux
  "Completion category for `consult-tmux' candidates.
Used by marginalia and embark to dispatch annotators/actions."
  :type 'symbol
  :group 'consult-tmux)

(defcustom consult-tmux-last-line-width 80
  "Max width of the last-line column in `consult-tmux' candidates."
  :type 'integer
  :group 'consult-tmux)

(defun consult-tmux--last-line (session)
  "Return the last non-empty line of output in SESSION, truncated."
  (let* ((out (shell-command-to-string
               (format "tmux capture-pane -p -t %s 2>/dev/null" session)))
         (line (or (car (last (split-string out "\n" t))) "")))
    (if (> (length line) consult-tmux-last-line-width)
        (concat (substring line 0 (- consult-tmux-last-line-width 3)) "...")
      line)))


(defun consult-tmux--sessions ()
  "Return an alist of (display-string . session-name) for all tmux panes."
  (let ((output (shell-command-to-string
                 (format "tmux list-panes -a -F '%s' 2>/dev/null"
                         consult-tmux-format))))
    (when (string-empty-p output)
      (error "No tmux server running or no sessions found"))
    (delq nil
          (mapcar
           (lambda (line)
             (when (string-match "^\\([^ ]+\\) \\(.*\\)$" line)
               (let* ((session (match-string 1 line))
                      (path    (match-string 2 line))
                      (title   (if (string-match-p "^[0-9]+$" session)
                                   "unnamed"
                                 session))
                      (last    (consult-tmux--last-line session))
                      (display (format "%-8s  %-30s  %s"
                                       title path last)))
                 (cons display session))))
           (split-string output "\n" t)))))


(defun consult-tmux--buffer-name (session)
  "Return the vterm buffer name for SESSION."
  (format "%s%s*" consult-tmux-buffer-prefix session))

(defun consult-tmux--attach (session)
  "Attach to SESSION in a dedicated vterm buffer."
  ;; here is the #number of tnix session extracted as property
  ;; =consult--candidate=
  (message "🪛 attach %s intro: %s" session (prin1-to-string session))
  (let* ((bufname (consult-tmux--buffer-name session))
         (buf     (get-buffer bufname)))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (let ((vterm-buffer-name bufname))
        (vterm bufname))
      (with-current-buffer bufname
        (vterm-send-string (format "tmux attach -t %s\n" session))))))

;;;###autoload
(defun consult-tmux ()
  "Select a tmux session and attach to it in a vterm buffer."
  (interactive)
  (let* ((sessions (consult-tmux--sessions))
         (cands    (mapcar (lambda (s)
                             (propertize (car s)
                                         'consult--candidate (cdr s)))
                           sessions)))
    (message "🐪first candidate %s\n. introspect %s\n. property %s"
             (car cands)
             (prin1-to-string (car cands))
             (get-text-property 0 'consult--candidate (car cands)))
    (when-let ((session (consult--read cands
                                       :prompt consult-tmux-prompt
                                       :sort nil
                                       :category consult-tmux-category
                                       :lookup #'consult--lookup-candidate
                                       :require-match t
                                       )))
      (consult-tmux--attach session))))


(defvar consult-tmux-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'consult-tmux-rename-session)
    map)
  "Keymap for `consult-tmux' embark actions.")

(message "DEBUG: consult-tmux-map defined = %S" (boundp 'consult-tmux-map))

(defun consult-tmux-rename-session (session)
  "Rename tmux SESSION, then refresh the candidate list."
  (message "1⛑️____________________> %s intro %s"
           session
           (prin1-to-string session))
  (interactive "sSession to rename: ")
  (let ((new
         (read-string (format "New name for %s: " session)))
        (session-num
         (get-text-property 0 'consult--candidate session)))
    (message "🦴 execute command %s" (format "tmux rename-session -t %s %s"
             (shell-quote-argument session-num)
             (shell-quote-argument new)))
    (shell-command
     (format "tmux rename-session -t %s %s"
             (shell-quote-argument session-num)
             (shell-quote-argument new)))
    ;; Close the current minibuffer and re-open with fresh candidates.
    (when (bound-and-true-p vertico-exit)
      (vertico-exit))
    (consult-tmux)))

(message "DEBUG: rename-session defined = %S" (fboundp 'consult-tmux-rename-session))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist
               (cons consult-tmux-category 'consult-tmux-map))
  (message "DEBUG: registered in embark-keymap-alist"))


(provide 'consult-tmux)
;;; consult-tmux.el ends here
