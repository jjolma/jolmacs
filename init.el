;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq source-dir (or (getenv "SOURCE_DIR") "~/depot/web2"))

;; 
;; Load paths
;;
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))


;;
;; Elpa
;;
(setq package-user-dir (concat dotfiles-dir "elpa"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))


(autoload 'color-theme-select "color-theme" "color-theme" t)

;;
;; Color theme
;;

(defun color-theme-jpj ()
  (color-theme-install
   '(color-theme-jpj
     ((background-color . "black")
      (foreground-color . "white")
      (cursor-color     . "yellow")
      (background-mode  . dark))
     
     (default      ((t (nil))))
     (fringe       ((t (                    :background "grey20"))))
     (modeline     ((t (:foreground "white" :background "darkslateblue"))))
     (region       ((t (                    :background "midnight blue"))))
     (highlight    ((t (                    :background "#13385b"))))
     
     (font-lock-builtin-face       ((t (:foreground "cornflower blue"))))
     (font-lock-comment-face       ((t (:foreground "green"))))
     (font-lock-doc-face           ((t (:foreground "green"))))
     (font-lock-constant-face      ((t (:foreground "gold"))))
     (font-lock-function-name-face ((t (:foreground "goldenrod" :bold t))))
     (font-lock-keyword-face       ((t (:foreground "DeepSkyBlue1"))))
     (font-lock-string-face        ((t (:foreground "red"))))
     (font-lock-type-face          ((t (:foreground "CadetBlue1" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
     (font-lock-warning-face       ((t (:foreground "Pink"))))
     )))
(color-theme-jpj)


(add-to-list 'auto-mode-alist '("\\.otl$" . outline-mode))

;; full screen
(defun maximize-frame () 
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))
(maximize-frame)

;; Use Command key as Meta.  http://www.emacswiki.org/emacs/MetaKeyProblems
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)


;;
;; Git
;;
(global-set-key (kbd "C-x g") 'magit-status)


;;
;; Miscellaneous configs
;;
(setq inhibit-startup-message t)


;;
;; Key bindings
;;
(global-set-key "\C-c\C-c"  'comment-region)  
(global-set-key "\C-c\C-u"  'uncomment-region)
(global-set-key "\C-x\C-b"      'electric-buffer-list)
(global-set-key "\M-g"          'goto-line)

;; mini-buffer
(define-key minibuffer-local-map "\t" 'hippie-expand)

;; mistakes
(global-set-key "\C-xf"     'find-file) 
(global-set-key "\C-x\C-f"  'find-file)
(global-set-key "\C-xs"     'save-buffer)
(global-set-key "\C-x\C-s"  'save-buffer)

;; nice for grep-find
(global-set-key [M-up]      'previous-error)
(global-set-key [M-down]    'next-error)

;; grep-find
(setq grep-find-command (format "find %s '(' -name '*.rb' -o -name '*.erb' ')' -type f -print | xargs grep -n -s -F " source-dir))
(put 'downcase-region 'disabled nil)

;; feature-mode
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))