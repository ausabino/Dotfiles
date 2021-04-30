;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alan Sabino"
      user-mail-address "alan.sabino@usp.br")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
;; (setq gc-cons-threshold 64000000)
;; (add-hook 'after-init-hook #'(lambda ()
;;                                ;; restore after startup
;;                                (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

;; Overwrite region selected
(delete-selection-mode t)

;; ;; Show column numbers by default
;; (setq column-number-mode t)

;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)

;; Highlight the line we are currently on
(global-hl-line-mode t)

;; Auto-wrap at 80 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Always end a file with a newline
(setq require-final-newline nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; Global Keyboard Shortcuts
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; Disable the menu bar since we don't use it, especially not in the
;; ;; terminal
;; (when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
;;   (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Select all
 (global-set-key "\C-c\C-a" 'mark-whole-buffer)

;; ;; Duplicate current line
;; (global-set-key "\C-c\C-y" "\C-a\C- \C-n\M-w\C-y")

;; Copy current line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key "\C-c\C-k" 'copy-line)

;; Move current line
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    ;; restore point to original column in moved line
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package which-key
;;   ;; :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function which-key-mode "which-key.el"))
;;   :config
;;   (which-key-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; autopair: Automatically at closing brace, bracket and quote
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! autopair
;;   ;; :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function autopair-global-mode "autopair.el"))
;;   :config
;;   (autopair-global-mode t)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags: Create tag tables thrown emacs functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" -or -name \"*.py\" -or -name \"*.cc\" | xargs etags --append" dir-name)))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill-Column-Indicator: draw a line on the limit number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fci-rule-column 79)
(setq fci-rule-width 1)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))

(add-hook 'before-save-hook 'xah-clean-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aspell/Huspell configure dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      (setq args (list "--sug-mode=ultra" "--lang=pt_BR"))
      (if run-together
          (setq args (append args '("--run-together"))))
      )
     args)))
(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "pt_BR")
  (setq ispell-local-dictionary-alist
        '(("pt_BR" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "pt_BR") nil utf-8))))
 (t (setq ispell-program-name nil)))
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word)
      current-location (cadr word) (caddr word)
      current-location))))
(global-set-key (kbd "C-4") 'my-save-word)
(let ((langs '("english" "pt_BR")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)
    (flyspell-buffer)))
(global-set-key (kbd "C-1") 'cycle-ispell-languages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
(load "/usr/share/emacs/site-lisp/clang-format-10/clang-format.el")
(use-package clang-format
  ;; :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  ;; :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))

(use-package py-autopep8
  ;; :ensure t
  :after python)
;; Format files to PEP8 convention style
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)
;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 2)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 2)
;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  ;; :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-source-correlate-start-server t)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Windows does not have a PDF viewer set for auctex")))
   ((string-equal system-type "darwin") ; Mac OS X
    (setq-default
     TeX-view-program-list
     '(("Skim"
        "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
       )
     TeX-view-program-selection '((output-pdf "Skim"))))
   ((string-equal system-type "gnu/linux") ; linux
    (setq-default TeX-view-program-list
                  '(("Evince" "evince --page-index=%(outpage) %o"))
                  TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (add-hook 'TeX-mode-hook 'linum-mode)
  (add-hook 'TeX-mode-hook 'hl-line-mode)
  (setq-default reftex-plug-into-AUCTeX t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  ;; :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  ;; Turn flycheck on everywhere
  (global-flycheck-mode)
  :config
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  ;; :ensure t
  :after python)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
 ;; :ensure t
 :config
 ;; Zero delay when pressing tab
 (setq company-idle-delay 0)
 (add-hook 'after-init-hook 'global-company-mode)
 ;; remove unused backends
 (setq company-backends (delete 'company-eclim company-backends))
 (setq company-backends (delete 'company-xcode company-backends))
 (setq company-backends (delete 'company-bbdb company-backends))
 (setq company-backends (delete 'company-oddmuse company-backends))
)
;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time
(use-package company-jedi
  ;; :ensure t
  :after python
  :init
  (defun company-jedi-setup ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'company-jedi-setup)
  )
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
