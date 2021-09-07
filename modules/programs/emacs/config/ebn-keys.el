(defun ebn/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ebn/rename-current-file ()
  (interactive)
  (rename-file (buffer-file-name)))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :keymaps 'normal
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   ""	'(nil :which-key "Leader")

   ;; Basic
   ":"	'(counsel-M-x :which-key "M-x")
   "qq" '(kill-emacs :which-key "Exit")

   ;; File
   "f"	'(:ignore t :which-key "File")
   "ff" '(counsel-find-file :which-key "Find file")
   "fr" '(counsel-recentf :which-key "Recent files")
   "fs" '(save-buffer :which-key "Save file")
   "fR" '(ebn/rename-current-file :which-key "Rename File")

   ;; Buffer
   "b"	'(:ignore t :which-key "Buffer")
   "bk" '(ebn/kill-current-buffer :which-key "Kill buffer")
   "bb" '(counsel-ibuffer :which-key "Switch buffer")

   ;; Notes
   "n"	'(:ignore t :which-key "Notes")
   "na" '(org-agenda :which-key "Agenda")

   ;; Window
   "w"	'(:ignore t :which-key "Window")
   "ww" '(other-window :which-key "Other window")
   "wd" '(delete-window :which-key "Delete window")
   "wo" '(delete-other-windows :which-key "Delete other windows")
   "ws" '(split-window-below :which-key "Split window")
   ))
(provide 'ebn-keys)
