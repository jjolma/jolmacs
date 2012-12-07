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


(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
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
(setq grep-find-command (format "find %s '(' -name '*.rb' -o -name '*.haml' ')' -type f -print | xargs grep -n -s -F " source-dir))
(global-set-key [f8] 'grep-find)
(put 'downcase-region 'disabled nil)

(global-set-key [f5] 'revert-buffer)

(global-set-key "\C-s"  'isearch-forward-regexp)
(global-set-key "\C-r"  'isearch-forward-regexp)


(add-to-list 'load-path "~/.emacs.d/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(setq column-number-mode t)

(eval-after-load "ediff"
  '(setq ediff-split-window-function 'split-window-horizontally))
(put 'upcase-region 'disabled nil)

(defun clever-hippie-tab (arg)
  "Ordinary tab or dabbrev"
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   (t (indent-for-tab-command))))

(global-set-key "\t"          'clever-hippie-tab)

(defun my-find-file-hook ()
  (local-set-key "\t" 'clever-hippie-tab)
  )
(add-hook 'find-file-hooks 'my-find-file-hook)



;; Add a hook to ruby-mode to modify ruby-mode-map
;; ;;;###autoload
;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; (defun jpj-ruby-setup ()
;;   (message "DONK")
;;   (define-key ruby-mode-map "TAB" 'clever-hippie-tab))

;;(eval-after-load 'ruby-mode
;; '(add-hook 'ruby-mode-hook 'jpj-ruby-setup))

;; (require 'ruby-mode)
;; (add-hook 'ruby-mode-hook
;; 	  (lambda ()
;; 	    (message "JPJ in hook")
;; 	    (define-key ruby-mode-map "TAB" 'clever-hippie-tab)))


;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project)))



(define-generic-mode 'blue-text-mode
  nil
  nil
  '(("^\\([^\\$]*\\)$" 1 'font-lock-keyword-face))
  nil
  nil
  "Simple mode for blue text.")

;; delete trailing whitespace
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))