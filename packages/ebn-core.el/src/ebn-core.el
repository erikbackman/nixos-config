;;; ebn-core.el -- Random stuff I use -*- lexical-binding: t -*-

;; Author: Erik Bäckman
;; Package-Requires ((emacs "25"))

;;; Code:

(defmacro ebn/font (&rest spec)
  `(let ((fs (funcall 'font-spec ,@spec)))
     (when (member (font-get fs :name) (font-family-list))
       (set-frame-font fs t t))))

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

(provide 'ebn-core)
;;; ebn-core.el ends here