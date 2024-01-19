;; Annenpolka early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)
;; suppress cl deprecation warnings
(setq byte-compile-warnings '(not cl-functions obsolete))
;; suppress ad-redefinition warnings
(setq ad-redefinition-action 'accept)

(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.5)

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)


(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (eq system-type 'gnu/linux))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (or IS-MAC (eq system-type 'berkeley-unix)))


;; disable bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; GUIs are inconsistent across systems, will rarely match our active Emacs
;; theme, and impose their shortcut key paradigms suddenly. Let's just avoid
;; them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Prioritize old byte-compiled source files over newer sources. It saves us a
;; little IO time to skip all the mtime checks on each lookup.
(setq load-prefer-newer nil)

(let ((old-file-name-handler-alist file-name-handler-alist))
  ;; `file-name-handler-alist' is consulted on each `require', `load' and
  ;; various path/io functions. You get a minor speed up by unsetting this.
  ;; Some warning, however: this could cause problems on builds of Emacs where
  ;; its site lisp files aren't byte-compiled and we're forced to load the
  ;; *.el.gz files (e.g. on Alpine).
  (setq-default file-name-handler-alist nil)
  ;; ...but restore `file-name-handler-alist' later, because it is needed for
  ;; handling encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    (setq file-name-handler-alist
          ;; Merge instead of overwrite because there may have bene changes to
          ;; `file-name-handler-alist' since startup we want to preserve.
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which pull
;; in a ton of packages. `doom/open-scratch-buffer' provides a better scratch
;; buffer anyway.
;; (setq initial-major-mode 'fundamental-mode
;;       initial-scratch-message nil)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC    (setq command-line-ns-option-alist nil))
(unless IS-LINUX  (setq command-line-x-option-alist nil))

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted and
;; indicates misconfiguration.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
;; (setq-default cursor-in-non-selected-windows nil)
;; (setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 0.5)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;;; early-init.el ends here;;End: