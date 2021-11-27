;;; ebn-core.el -- Random stuff I use -*- lexical-binding: t -*-

;; Author: Erik Bäckman
;; Package-Requires ((emacs "25"))

;;; Code:

(defmacro ebn/font (&rest spec)
  `(let ((fs (funcall 'font-spec ,@spec)))
     (message "%s" (font-get fs :name) (font-family-list))
     (when (member (font-get fs :name) (font-family-list))
       (set-face-attribute 'default nil :font fs)
       (set-face-attribute 'fixed-pitch nil :font fs))))

(defmacro ebn/font-variable-pitch (&rest spec)
  `(let ((fs (funcall 'font-spec ,@spec)))
     (message "%s" (font-get fs :name) (font-family-list))
     (when (member (font-get fs :name) (font-family-list))
       (set-face-attribute 'variable-pitch nil :font fs))))

(defun ebn/kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ebn/rename-current-file ()
  "Rename current file to NEWNAME."
  (interactive)
  (let ((fname (buffer-file-name))
	(bname (buffer-name)))
    (if (not (and bname (file-exists-p fname)))
	(error "Buffer '%s' is not visiting a file" bname)
      (let ((new-name (read-file-name "New name: " fname)))
	(if (get-buffer new-name)
	    (error "Buffer with name %s already exists" new-name)
	  (rename-file fname new-name t)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File renamed to %s" new-name))))))

(defun ebn/dired-up-directory ()
  "Up directory - killing current buffer."
  (interactive)
  (let ((cb (current-buffer)))
    (progn (dired-up-directory)
	   (kill-buffer cb))))

(defun ebn/project-rg ()
  "Run ripgrep in current project"
  (interactive)
  (funcall-interactively #'consult-ripgrep (project-root (project-current))))

(defun ebn/display-startup-time ()
  "Message startup time."
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(defun ebn/open-line-below ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ebn/open-line-above ()
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))

(provide 'ebn-core)
;;; ebn-core.el ends here
