;; Packages will be initialized by use-package later.
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)
;;(setq gc-cons-threshold 134217728)   ; 128mb
(setq gc-cons-threshold most-positive-fixnum)

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
