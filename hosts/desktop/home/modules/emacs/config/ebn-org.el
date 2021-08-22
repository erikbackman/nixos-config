(use-package org
  :config
;;; lisp/my-org-agenda.el -*- lexical-binding: t; -*-

  (setq org-agenda-files '("~/org/todo.org"
			   "~/org/gtd.org"
			   "~/org/tickler.org"))

  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)))

  (setq org-agenda-custom-commands
	'(("s" "Studies" tags-todo "@studies"
	   ((org-agenda-overriding-header "Studies")
	    (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
	(setq should-skip-entry t))
      (save-excursion
	(while (and (not should-skip-entry) (org-goto-sibling t))
	  (when (org-current-is-todo)
	    (setq should-skip-entry t))))
      (when should-skip-entry
	(or (outline-next-heading)
	    (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))


  )

(provide 'ebn-org)
