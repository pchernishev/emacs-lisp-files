;;; package --- Init file
;;; Commentary:
;; Initial Emacs load file
;;

;;; Code:
;; Add the given path to the load-path variable.
(defun add-to-load-path (path-string)
  (message (format "Passed %S..." path-string))
  (if (stringp path-string)
      (when (file-exists-p path-string)
	(message (format "Adding %S to load-path..." path-string))
	(add-to-list 'load-path (expand-file-name path-string)))
    (crs-add-to-load-path (car path-string))
    (if (cdr path-string)
	(crs-add-to-load-path (cdr path-string)))))

(add-to-load-path (expand-file-name "~/.emacs.d/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/ecb-20140215.114/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/xcscope-20160628.2324/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/async-20160711.2235/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/helm-20161010.1043/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/helm-cscope-20150609.649/"))
(add-to-load-path (expand-file-name "~/.emacs.d/elpa/adaptive-wrap-0.5/"))


(require 'settings)
(require 'marks)
;(require 'python-mode)
(require 'ido)
;; (require 'pc-select)
(require 'quick-yes)
;; (require 'newshell)
;(require 'lua-mode)
;; (require 'git)
(require 'gitextension)
(require 'internet-search)
;(require 'popwin)
;(popwin-mode 1)
;(require 'multiple-cursors)
;(require 'minimal-session-saver)
(require 'xcscope)
;; (require 'sr-speedbar)

(python-mode)
(emacs-lisp-mode)
(ido-mode t)
(font-lock-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 4)
 '(bell-volume 0)
 '(blink-cursor-mode nil)
 '(blink-matching-paren t)
 '(column-number-mode t)
 '(company-idle-delay 0.05)
 '(compilation-scroll-output t)
 '(custom-enabled-themes (quote (light-blue)))
 '(delete-selection-mode t)
 '(ecb-source-path (quote (("/" "/"))))
 '(efs-use-passive-mode t)
 '(elpy-default-minor-modes (quote (flymake-mode yas-minor-mode)))
 '(font-lock-maximum-size 400000)
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(load-home-init-file t t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (dired+ sr-speedbar multishell pc-mode python-mode realgud test-simple loc-changes load-relative yasnippet ac-helm git-gutter diff-hl magit magit-popup git-commit with-editor jedi-direx ac-anaconda flycheck-pyflakes fill-column-indicator python-pylint pylint company-anaconda anaconda-mode pythonic ecb company jedi jedi-core multiple-cursors py-autopep8 pep8 helm-cscope flycheck adaptive-wrap)))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(standard-indent 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(truncate-partial-width-windows t))

;; Let's use CYGWIN bash...
;;
;; (setq binary-process-input t)
;; (setq w32-quote-process-args ?\")
(setq shell-file-name "bash") ;; or sh if you rename your bash executable to sh.
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
;; (setq explicit-bash-args '("--noediting" "-i"))

(global-visual-line-mode)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;; Make Emacs split horizontally by default (i.e when doing grep/completion/C-h b)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Turn off bell alarms
(setq ring-bell-function 'ignore)

(setq-default line-spacing '1)
;to scroll line by line
;(setq scroll-step            1
;      scroll-conservatively  10000)
(setq linum-format "%3d ")

;; (setq-default left-margin-width '0)
;; (setq-default right-margin-width '0)
;; (setq-default left-fringe-width '0)
;; (setq-default right-fringe-width '0)

(package-initialize)
;(elpy-enable)
;(elpy-clean-modeline)
;; (require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; (flycheck-define-checker python-runflake
;;   "A Python syntax and style checker using the pyflakes utility.

;; See URL `http://pypi.python.org/pypi/pyflakes'."
;;   :command ("/home/pavelc/xtremapp/xms/test/run_flake.py" source-inplace)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" (message) line-end))
;;   :modes python-mode)

;; (add-to-list 'flycheck-checkers 'python-runflake)

;; (defun flycheck-python-setup ()
;;   (flycheck-mode))
;; (add-hook 'python-mode-hook #'flycheck-python-setup)

;; Load keys the last, in order to override bad key bindings
(require 'keys)


;; Helper for compilation. Close the compilation window if
;; there was no error at all.
;(defun compilation-exit-autoclose (status code msg)
;  ;; If M-x compile exists with a 0
;  (when (and (eq status 'exit) (zerop code))
;    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;    (bury-buffer)
;    ;; and delete the *compilation* window
;    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
;  ;; Always return the anticipated result of compilation-exit-message-function
;  (cons msg code))
;;; Specify my function (maybe I should have done a lambda function)
;(setq compilation-exit-message-function 'compilation-exit-autoclose)
;;; (setq max-lisp-eval-depth 30000)

;; (defun cygwin-shell ()
;;   "Run cygwin bash in shell mode."
;;   (interactive)
;;   (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
;;     (call-interactively 'shell)))

(defun ecb-activated-in-selected-frame ()
  "A hack to use ECB in multiple frames. It first deactivates ECB, then
    activate it in current frame."
  (interactive)
  (let ((current-frame (selected-frame)))
                                        ; The frame foucs change when activating or deactivating ECB is weird, so
                                        ; activate current selected frame explicitly.
    (if (and (boundp 'ecb-minor-mode) (ecb-minor-mode))
        (ecb-deactivate)
      )
    (select-frame current-frame)
    (ecb-activate)
    ))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down)))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; (pc-select-selection-keys-only t)
;; (pc-selection-mode)
(electric-pair-mode)
(cscope-setup)
(helm-cscope-mode t)
(semantic-mode t)
(which-function-mode 1)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;(require 'stickyfunc-enhance)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq ido-default-buffer-method 'selected-window)
;; Set key bindings
;(eval-after-load "helm-cscope"
;  '(progn
(global-set-key (kbd "C-(") 'forward-or-backward-sexp)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-s d") 'helm-cscope-find-global-definition)
(global-set-key (kbd "M-s M-d") 'cscope-find-global-definition)
(global-set-key (kbd "M-s c") 'helm-cscope-find-called-function)
(global-set-key (kbd "M-s M-c") 'cscope-find-called-functions)
(global-set-key (kbd "M-s f") 'helm-cscope-find-calling-this-funtcion)
(global-set-key (kbd "M-s M-f") 'cscope-find-functions-calling-this-function)
(global-set-key (kbd "M-s F") 'helm-cscope-find-this-file)
(global-set-key (kbd "M-s M-F") 'cscope-find-this-file)
(global-set-key (kbd "M-s s") 'helm-cscope-find-this-symbol)
(global-set-key (kbd "M-s M-s") 'cscope-find-this-symbol)
(global-set-key (kbd "M-s t") 'helm-cscope-find-this-text-string)
(global-set-key (kbd "M-s M-t") 'cscope-find-this-text-string)
(global-set-key (kbd "M-s a") 'helm-cscope-find-assignments-to-this-symbol)
(global-set-key (kbd "M-s M-a") 'cscope-find-assignments-to-this-symbol)
(global-set-key (kbd "M-s e") 'helm-cscope-find-egrep-pattern)
(global-set-key (kbd "M-s M-e") 'cscope-find-egrep-pattern)
(global-set-key (kbd "M-s i") 'helm-cscope-find-files-including-file)
(global-set-key (kbd "M-s M-i") 'cscope-find-files-including-file)
(global-set-key (kbd "M-s q") 'helm-cscope-pop-mark)
(global-set-key (kbd "M-s M-q") 'cscope-pop-mark)
;))

(global-set-key (kbd "C-M-<prior>") 'move-line-region-up)
(global-set-key (kbd "C-M-<next>") 'move-line-region-down)

(global-set-key (kbd "C-c C-o") 'helm-occur-from-isearch)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c C-g") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-x r c") 'copy-rectangle-as-kill)
(global-set-key (kbd "C-x r e") 'string-insert-rectangle)
(global-set-key (kbd "C-x r C") 'clear-rectangle)
(global-set-key (kbd "C-M-z") 'pop-global-mark)
(global-set-key (kbd "C-\"") 'py-shift-block-left)
(global-set-key (kbd "C-|") 'py-shift-block-right)
(global-set-key (kbd "C-{") 'py-beginning-of-class)
(global-set-key (kbd "C-}") 'py-end-of-class)

(global-set-key (kbd "C-$") 'shrink-window)
(global-set-key (kbd "C-%") 'enlarge-window)
(global-set-key (kbd "C-&") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'shrink-window-horizontally)

(global-set-key (kbd "C-M-<home>") 'replace-string)
(global-set-key (kbd "C-M-<insert>") 'query-replace)
(global-set-key (kbd "<home>") 'back-to-indentation)
(global-set-key (kbd "C-a") 'beginning-of-line)
;; (global-set-key (kbd "C-x C-a") 'beginning-of-line-mark)
(global-set-key (kbd "C-M-g") 'py-mark-clause)
(global-set-key (kbd "C-!") 'rename-buffer)
(global-unset-key (kbd "S-<home>"))

(global-set-key (kbd "M-<right>") 'previous-buffer)
(global-set-key (kbd "M-<left>") 'next-buffer)

(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "C-o") 'occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-x") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x f") 'helm-find-files)

;; ;(setq py-return-key 'py-newline-and-indent)
;; (add-hook 'python-mode-hook
;;        (lambda ()
;; (add-hook 'python-mode-hook
;;        (lambda ()
;; ;(electric-indent-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)


  ;; (add-hook 'python-mode-hook 'turn-on-fci-mode)
  ;; (setq-default fill-column '79)
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))
(global-diff-hl-mode t)
(add-hook 'python-mode-hook #'hs-minor-mode)
;; (add-hook 'python-mode-hook 'comment-set-column)
(add-hook 'python-mode-hook
          (lambda () (jedi:setup)))
(setq ac-max-width '0.6)
;; (defun update-ac-sources()
;;   (setq ac-sources '(ac-source-jedi-direct)))
;; (add-hook 'python-mode-hook 'update-ac-sources)
;; (add-hook 'python-mode-hook
;;           (lambda () (anaconda-mode)))
;; (add-hook 'python-mode-hook
;;           (lambda () (ac-anaconda-setup)))

(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay nil)
(setq jedi:tooltip-method '(popup))
(define-key python-mode-map (kbd "C-c c") 'jedi:complete)
(define-key python-mode-map (kbd "C-c n") 'jedi:goto-definition-next)
(define-key python-mode-map (kbd "C-c m") 'jedi:goto-definition-pop-marker)
(define-key python-mode-map (kbd "C-c f") 'jedi:get-in-function-call)
(define-key python-mode-map (kbd "C-<return>") 'newline)
(define-key python-mode-map (kbd "<return>") 'newline-and-indent)

;; (defun update-ac-sources()
;;   (setq ac-sources '(ac-source-jedi-direct)))
;; (add-hook 'python-mode-hook 'update-ac-sources)

(global-set-key (kbd "C-<kp-1>") 'mc/mark-all-in-region)
(global-set-key (kbd "C-<kp-0>") 'mc/mark-all-like-this)

;; Custom-set-faces was added by Custom.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit default :stipple nil :background "#161616" :foreground "lightGrey" :family "Consolas"))))
 '(border ((t (:height 1.0 :width normal))))
 '(comint-highlight-input ((t (:foreground "grey70" :bold t))))
 '(company-preview-common ((t (:foreground "red" :underline t :background "#161616"))))
 '(company-scrollbar-bg ((t (:background "gray38"))))
 '(company-scrollbar-fg ((t (:background "coral"))))
 '(company-tooltip ((t (:background "gainsboro" :foreground "grey15"))))
 '(company-tooltip-selection ((t (:background "peach puff"))))
 '(cscope-line-number-face ((t (:foreground "tomato2"))))
 '(cscope-mouse-face ((t (:foreground "grey90" :background "grey25"))))
 '(cscope-separator-face ((t (:foreground "tomato3" :bold t :height 120))))
 '(cursor ((t (:foreground "grey5" :background "red2"))))
 '(custom-button-face ((t (:bold t :foreground "#3fdfcf"))) t)
 '(custom-group-tag-face ((t (:underline t :foreground "blue"))) t)
 '(custom-saved-face ((t (:underline t :foreground "orange"))) t)
 '(custom-state-face ((t (:foreground "green3"))) t)
 '(custom-variable-button-face ((t (:bold t :underline t :foreground "white"))) t)
 '(ecb-analyse-face ((t (:foreground "grey70" :background "firebrick4" :bold t))))
 '(ecb-default-highlight-face ((t (:inherit match))))
 '(ecb-directories-general-face ((t (:inherit match))))
 '(ecb-directory-face ((t (:inherit match))))
 '(ecb-tag-header-face ((t (:foreground darkgreen))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-doc-string-face ((t (:foreground "cyan"))) t)
 '(font-lock-function-name-face ((t (:foreground "forestgreen" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "orange3"))))
 '(font-lock-preprocessor-face ((t (:foreground "deepskyblue" :bold t))))
 '(font-lock-reference-face ((t (:foreground "orangered"))) t)
 '(font-lock-string-face ((t (:foreground "cyan3"))))
 '(font-lock-type-face ((t (:foreground "green3" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "gold"))))
 '(font-lock-warning-face ((t (:foreground "yellow" :bold t))))
 '(fringe ((t (:background "#161616"))))
 '(highlight ((t (:foreground "red3" :background "grey75"))))
 '(hl-line ((t (:background "grey12"))))
 '(isearch ((t (:foreground "firebrick4" :background "cyan3" :bold t))))
 '(lazy-highlight ((t (:foreground "grey70" :background "firebrick4"))))
 '(linum ((t (:foreground "moccasin" :background "grey17"))) t)
 '(match ((t (:foreground "grey70" :background "firebrick4"))))
 '(minibuffer-prompt ((t (:foreground "cyan3" t))))
 '(py-import-from-face ((t (:inherit font-lock-constant-face))))
 '(py-number-face ((t (:foreground "tomato2"))))
 '(py-object-reference-face ((t (:inherit font-lock-variable-name-face))))
 '(region ((t (:background "grey20"))))
 '(speedbar-highlight-face ((t (:inherit highlight)))))

(provide 'init)
;;; init ends here
