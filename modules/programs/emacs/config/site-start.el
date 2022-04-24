;; -*- lexical-binding: t; -*-
(defun nix--profile-paths ()
  "Return a list of all paths in NIX_PROFILES.
The list is ordered from more-specific (the user profile) to the
least specific (the system profile)"
  (reverse (split-string (or (getenv "NIX_PROFILES") ""))))

;;; Extend `load-path' to search for elisp files in subdirectories of
;;; all folders in `NIX_PROFILES'. Also search for one level of
;;; subdirectories in these directories to handle multi-file libraries
;;; like `mu4e'.'
(require 'seq)
(let* ((subdirectory-sites (lambda (site-lisp)
                             (when (file-exists-p site-lisp)
                               (seq-filter (lambda (f) (file-directory-p (file-truename f)))
                                           ;; Returns all files in `site-lisp', excluding `.' and `..'
                                           (directory-files site-lisp 'full "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))))
       (paths (apply #'append
                     (mapcar (lambda (profile-dir)
                               (let ((site-lisp (concat profile-dir "/share/emacs/site-lisp/")))
                                 (cons site-lisp (funcall subdirectory-sites site-lisp))))
                             (nix--profile-paths)))))
  (setq load-path (append paths load-path)))

;;; Remove wrapper site-lisp from EMACSLOADPATH so it's not propagated
;;; to any other Emacsen that might be started as subprocesses.
(let ((wrapper-site-lisp (getenv "emacsWithPackages_siteLisp"))
      (env-load-path (getenv "EMACSLOADPATH")))
  (when wrapper-site-lisp
    (setenv "emacsWithPackages_siteLisp" nil))
  (when (and wrapper-site-lisp env-load-path)
    (let* ((env-list (split-string env-load-path ":"))
           (new-env-list (delete wrapper-site-lisp env-list)))
      (setenv "EMACSLOADPATH" (when new-env-list
                                (mapconcat 'identity new-env-list ":"))))))

(let ((wrapper-site-lisp (getenv "emacsWithPackages_siteLispNative"))
      (env-load-path (getenv "EMACSNATIVELOADPATH")))
  (when wrapper-site-lisp
    (setenv "emacsWithPackages_siteLispNative" nil))
  (when (and wrapper-site-lisp env-load-path)
    (let* ((env-list (split-string env-load-path ":"))
           (new-env-list (delete wrapper-site-lisp env-list)))
      (setenv "EMACSNATIVELOADPATH" (when new-env-list
                                (mapconcat 'identity new-env-list ":"))))))

;;; Set up native-comp load path.
(when (featurep 'comp)
  ;; Append native-comp subdirectories from `NIX_PROFILES'.
  (setq native-comp-eln-load-path
        (append (mapcar (lambda (profile-dir)
                          (concat profile-dir "/share/emacs/native-lisp/"))
                        (nix--profile-paths))
                native-comp-eln-load-path)))

;;; Make `woman' find the man pages
(defvar woman-manpath)
(eval-after-load 'woman
  '(setq woman-manpath
         (append (mapcar (lambda (x) (concat x "/share/man/"))
                         (nix--profile-paths))
                 woman-manpath)))

;;; Make tramp work for remote NixOS machines
(defvar tramp-remote-path)
(eval-after-load 'tramp-sh
  ;; TODO: We should also add the other `NIX_PROFILES' to this path.
  ;; However, these are user-specific, so we would need to discover
  ;; them dynamically after connecting via `tramp'
  '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

;;; C source directory
;;;
;;; Computes the location of the C source directory from the path of
;;; the current file:
;;; from: /nix/store/<hash>-emacs-<version>/share/emacs/site-lisp/site-start.el
;;; to:   /nix/store/<hash>-emacs-<version>/share/emacs/<version>/src/
(defvar find-function-C-source-directory)
(let ((emacs
       (file-name-directory                      ; .../emacs/
        (directory-file-name                     ; .../emacs/site-lisp
         (file-name-directory load-file-name)))) ; .../emacs/site-lisp/
      (version
       (file-name-as-directory
        emacs-version))
      (src (file-name-as-directory "src")))
  (setq find-function-C-source-directory (concat emacs version src)))


;;; Copied from early-init.el
;;;
;;; Emacs only reads early-init.el the first directory in it's preferred
;;; list of directories which means if the user has a .emacs.d or .config/emacs.d directory
;;; it will try to load early-init.el from one of those and ignore early-init.el existing in
;;; any other directory in it's load-path.
;;; To fix this I override the emacs site-start.el and append the code below.
;;; 
;; Packages will be initialized by use-package later.
(setq package-enable-at-startup nil)
(setq package-archives nil)

(defvar last-file-name-handler-alist file-name-handler-alist)

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)
;;(setq gc-cons-threshold 134217728)   ; 128mb
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 2 1000 1000)
		  file-name-handler-alist last-file-name-handler-alist)))

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
;; TODO: Probably the better approach is:
;; (setq inhibit-x-resources t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
(setq comp-deferred-compilation nil
      native-comp-deferred-compilation nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq fancy-startup-text nil)
(setq fancy-about-text nil)
