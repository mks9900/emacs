;; package --- Summary

;;; Commentary:
;; I use this config on four computers and so there are some cases to handle
;; certain cases separately.
;; To install programs called for in the below code, install them on Ubuntu
;; using the following command:
;; $ sudo apt install npm hunspell cargo pandoc
;;
;; Besides the above, also install texlive-type of package to handle Tex-files.
;;
;; Also, install the following:
;; $ brew install marksman # markdown LSP-server
;; $ npm install -g prettier # markdown formatting in Emacs
;; $ npm install dictionary-sv
;; $ npm install dictionary-en-gb
;; $ sudo npm install -g pyright
;;
;; Every other package should be installed using use-package.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set repositories to use:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "0. Loading packages...")

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))



;; Bootstrap straight:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; suppress a warning about Emacs complaining about
;; straight not being defined:
(declare-function straight-use-package "ext:straight.el")

;; Integrates `straight' directly into the `use-package' package through the `:straight' expression.
(straight-use-package 'use-package)
(setq straight-check-for-modifications '(find-when-checking))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "1. Applying global settings...")
;; Always use utf-8:
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)


;; Set the width of the line numbers to be fixed, depending
;; on the number of lines.
(setq display-line-numbers-width-start t)

;; Don't produce backup-files:
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)


;; Don't write sensitive info in plain text:
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-save-behavior t)

;; Stop Emacs from hiding:
(unbind-key "C-z") ;; suspend-frame


;; Stop Emacs from throwing up buffers with warnings:
(defvar warning-minimum-level)
(setq warning-minimum-level :emergency)


;; Mark whole buffer:
(global-set-key (kbd "C-c a") 'mark-whole-buffer)


;; Indent region (instead of C-M-\)
(global-set-key (kbd "C-c i") 'indent-region)


;; Short-cut for editing config.el:
(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/config.el"))
(bind-key "C-c e" #'open-init-file)


;; Comment/uncomment lines of marked code:
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (let* ((start (if (region-active-p)
                    (region-beginning)
                  (line-beginning-position)))
         (end (if (region-active-p)
                  (region-end)
                (line-end-position)))
         (line-text (buffer-substring-no-properties start end)))
    (cond
     ;; JSON mode handling
     ((derived-mode-p 'json-mode)
      (save-excursion
        (goto-char start)
        (if (string-match-p "^[ \t]*//" line-text)
            ;; Remove comment
            (while (re-search-forward "^[ \t]*//" end t)
              (replace-match "" nil nil))
          ;; Add comment
          (while (< (point) end)
            (unless (looking-at "^[ \t]*//")
              (beginning-of-line)
              (insert "// "))
            (forward-line 1)))))

     ;; C++ block comment handling
     ((and (derived-mode-p 'c++-mode)
           (region-active-p))  ; Only use block comments for regions
      (save-excursion
        (goto-char start)
        (if (and (string-match-p "^[ \t]*/\\*" line-text)
                 (string-match-p "\\*/[ \t]*$"
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  end)))
            ;; Remove block comment
            (progn
              (goto-char start)
              (when (re-search-forward "/\\*" end t)
                (replace-match "" nil nil))
              (goto-char start)
              (when (re-search-forward "\\*/" end t)
                (replace-match "" nil nil)))
          ;; Add block comment
          (goto-char start)
          (insert "/* ")
          (goto-char end)
          (insert " */"))))

     ;; Default handling for other modes
     (t (comment-or-uncomment-region start end)))))

;; (defun comment-or-uncomment-line-or-region ()
;;   "Comments or uncomments the current line or region."
;;   (interactive)
;;   (let* ((start (if (region-active-p)
;;                     (region-beginning)
;;                   (line-beginning-position)))
;;          (end (if (region-active-p)
;;                   (region-end)
;;                 (line-end-position)))
;;          (line-text (buffer-substring-no-properties start end)))
;;     (if (derived-mode-p 'json-mode)
;;         ;; For JSON modes, manually handle // comments
;;         (save-excursion
;;           (goto-char start)
;;           (if (string-match-p "^[ \t]*//" line-text)
;;               ;; Remove comment
;;               (while (re-search-forward "^[ \t]*//" end t)
;;                 (replace-match "" nil nil))
;;             ;; Add comment
;;             (while (< (point) end)
;;               (unless (looking-at "^[ \t]*//")
;;                 (beginning-of-line)
;;                 (insert "// "))
;;               (forward-line 1))))
;;       ;; For other modes, use standard comment function
;;       (comment-or-uncomment-region start end))))

;; (global-set-key (kbd "M-1") 'comment-or-uncomment-line-or-region)


;; Create a function to indent buffers:
(defun indent-buffer-smart ()
  ;;   "Indent buffer while preserving point and window position.
  ;; Also handles various cleanup tasks like removing trailing whitespace."
  (interactive)
  ;; Remember window position
  (let ((window-start (window-start)))
    ;; Remember cursor position
    (let ((current-point (point)))
      ;; Indent and cleanup
      (save-excursion
        (delete-trailing-whitespace)
        (indent-region (point-min) (point-max) nil)
        (untabify (point-min) (point-max)))
      ;; Restore cursor and window position
      (goto-char current-point)
      (set-window-start (selected-window) window-start))
    (message "Buffer indented and cleaned up!"))
  ;; Flash modeline to indicate completion
  (force-mode-line-update)
  (sit-for 0.5))

(global-set-key (kbd "M-2") 'indent-buffer-smart)



;; Always show linenumbers in the left margin:
(global-display-line-numbers-mode 1)


(defun pt-yank ()
  "Call yank, then indent the pasted region, as TextMate does."
  (interactive)
  (let ((point-before (point)))
    (when mark-active (call-interactively 'delete-backward-char))
    (yank)
    (indent-region point-before (point))))

(bind-key "C-y" #'pt-yank)
(bind-key "C-z" #'undo)
(bind-key "s-v" #'pt-yank)
(bind-key "C-Y" #'yank)


;; Always center current line:
;; (use-package centered-cursor-mode
;;   :ensure t
;;   :config
;;   ;; To enable it globally (in all buffers):
;;   (global-centered-cursor-mode 1)
;;   ;; Or if you prefer to enable it only in specific modes:
;;   ;; (add-hook 'text-mode-hook 'centered-cursor-mode)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specifics:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "2. OS-specifics...")
(defvar mac-right-option-modifier)
(defvar mac-command-modifier)
(defvar xdg-bin (getenv "XDG_BIN_HOME"))
(defvar xdg-cache (getenv "XDG_CACHE_HOME"))
(defvar xdg-config (getenv "XDG_CONFIG_HOME"))


;; Theme setup function
(defun my/setup-themes ()
  "Set up themes based on display type."
  (message "3. Setting up themes...")
  (mapc #'disable-theme custom-enabled-themes)
  (if (display-graphic-p)
      ;; GUI mode
      (load-theme 'ef-owl t)
    ;; Terminal mode
    (progn
      (load-theme 'tango-dark t)
      ;; Terminal-specific face settings
      (set-face-background 'hl-line "gray25")
      (set-face-foreground 'hl-line nil)
      (set-face-attribute 'hl-line nil :inherit nil)
      ;; Vertico face settings for terminal
      (with-eval-after-load 'vertico
        (set-face-background 'vertico-current "gray25")
        (set-face-attribute 'vertico-current nil :inherit nil)))))

(defun my/setup-system-gui ()
  "Configure system-specific GUI settings"
  (when (display-graphic-p)
    (cond
     ;; macOS specific
     ((and (eq system-type 'darwin)
           (string-equal (system-name) "macbook15-macos.vilanelva.se"))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (set-face-attribute 'default nil :font "Source Code Pro" :height 180))
     ;; Linux specific
     ((eq system-type 'gnu/linux)
      (cond
       ((string-equal (system-name) "rocky-ws")
        (set-frame-size (selected-frame) 100 70)
        (set-frame-size (selected-frame) 120 70)
        (set-frame-position (selected-frame) 850 0)
        (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 160))
       ((string-equal (system-name) "macbook13-linux")
        (add-to-list 'default-frame-alist '(fullscreen . maximized))
        (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 200))
       ((string-equal (system-name) "sodra-ds-test")
        ;; (set-frame-size (selected-frame) 100 50)
        ;; (set-frame-position (selected-frame) 10 10)
        (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 180))
       ((string-equal (system-name) "sod-as103403")
        (set-frame-size (selected-frame) 160 90)
        (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 240)))))))


;; Set up hooks
(add-hook 'after-init-hook #'my/setup-themes)
(add-hook 'window-setup-hook #'my/setup-system-gui)


;; Set up non-GUI specific settings immediately
(cond
 ((eq system-type 'darwin)
  ;; macOS keybinding-fixes:
  (setq mac-right-option-modifier 'nil)
  (setq mac-command-modifier 'control
        select-enable-clipboard t)))


;; For daemon mode, also set up GUI settings when creating new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/setup-themes)
              (my/setup-system-gui))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and feel:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "4. Look and feel...")

;; For text files: auto-fill for actual line breaks at 88 chars
(setq-default fill-column 88)
(add-hook 'text-mode-hook (lambda ()
                            (visual-line-mode 1)
                            (display-fill-column-indicator-mode 1)))

;; For programming modes: visual-line-mode for soft wrapping
(add-hook 'prog-mode-hook (lambda ()
                            (visual-line-mode 1)
                            (display-fill-column-indicator-mode 1)))

;; Keep column indicator globally available but let modes enable it
(setq-default display-fill-column-indicator-column 88)


(custom-set-variables
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(warning-suppress-types '((use-package))))


;; Customize mode line appearance for active and inactive buffers
(set-face-attribute 'mode-line nil :foreground "gray50" :background "black" :box '(:line-width 1 :color "gray50"))
(set-face-attribute 'mode-line-inactive nil :foreground "white" :background "gray20" :box '(:line-width 1 :color "gray20"))


;; Make the window divider line thicker
(setq window-divider-default-places t
      window-divider-default-bottom-width 2
      window-divider-default-right-width 2)


;; Show filename in title:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;; Make commented text stand out better:
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "gray60")))))


;; Make a greater difference between active and inactive buffers:
(defvar dimmer-delay)
(use-package dimmer
  :ensure t
  :config
  ;; Adjust the dimming fraction (the default is 0.20)
  (setq dimmer-fraction 0.10)
  (setq dimmer-delay 0.01) ; Adjust the delay in seconds


  ;; Exclude some buffers from being dimmed
  ;; (add-to-list 'dimmer-exclusion-regexp-list "^\*helm")
  (add-to-list 'dimmer-exclusion-regexp-list "^\*Minibuf")

  ;; Configure and activate dimmer
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-mode t))


;; Mouse scroll speed:
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;; Scroll speed!
(use-package ultra-scroll
                                        ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))



;; Install icons:
(use-package all-the-icons
  :ensure t)


(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


;; add icons to the files:
(use-package all-the-icons-completion
  :ensure t
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))


(use-package rainbow-delimiters
  :straight t
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))
(electric-pair-mode)


;; Delight let's you customise how modes are displayed (or hidden):
(use-package delight
  :ensure t
  :straight t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "5. Misc settings...")
(setq-default
 ad-redefinition-action 'accept                      ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                    ; Hide the cursor in inactive windows
 display-time-default-load-average nil               ; Don't display load average
 help-window-select t                                ; Focus new help windows when opened
 indent-tabs-mode nil                                ; Prefer spaces over tabs
 inhibit-startup-screen t                            ; Disable start-up screen
 initial-scratch-message ""                          ; Empty the initial *scratch* buffer
 kill-ring-max 128                                   ; Maximum length of kill ring
 load-prefer-newer t                                 ; Prefer the newest version of a file
 mark-ring-max 128                                   ; Maximum length of mark ring
 ;; read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
 scroll-conservatively most-positive-fixnum          ; Always scroll by one line
 select-enable-clipboard t                           ; Merge system's and Emacs' clipboard
 ;; tab-width 4                                      ; Set width for tabs
 ;; use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 user-full-name "Johan Thor"                         ; Set the full name of the current user
 ;;user-mail-address "terencio.agozzino@gmail.com"   ; Set the email address of the current user
 vc-follow-symlinks t                                ; Always follow the symlinks
 fast-but-imprecise-scrolling t                      ; More scrolling performance!
 view-read-only t)                                   ; Always open read-only buffers in view-mode
(column-number-mode 1)                               ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                        ; Replace yes/no prompts with y/n
(global-hl-line-mode)                                ; Hightlight current line
(set-default-coding-systems 'utf-8)                  ; Default to utf-8 encoding
(show-paren-mode 1)                                  ; Show the parent


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addons and customisations:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "6. Addons and customisations...")

(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)


;; Windmove:
(use-package windmove
  :ensure t
  :straight t
  :bind
  (
   ("M-j" . windmove-left)
   ("M-i" . windmove-up)
   ("M-k" . windmove-down)
   ("M-l" . windmove-right)
   ("C-c M-l" . windmove-delete-left)
   ("C-c M-r" . windmove-delete-right)
   ("C-c M-d" . windmove-delete-down)
   ("C-c M-u" . windmove-delete-up)
   )
  )


;; which-key:
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)  ; Adjust the delay as needed
  ;; Other customization options here
  )


;; Edit files with sudo:
(use-package sudo-edit
  :ensure t
  :straight t)


;; ibuffer:
(use-package ibuffer
  :straight t
  :ensure nil
  :preface
  (defvar protected-buffers '("*scratch*" "*Messages*")
    "Buffer that cannot be killed.")

  (defun my/protected-buffers ()
    "Protect some buffers from being killed."
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill))))
  :bind ("C-x C-b" . ibuffer)
  :init (my/protected-buffers))


;; ;; Spell checking:
;; (use-package ispell
;;   :ensure nil ; `ispell` is bundled with Emacs, so we don't need to install it...
;;   :config
;;   ;; Set Hunspell as the default spell checker
;;   (setq ispell-program-name "hunspell")
;;   ;; Set the default dictionaries
;;   (setq ispell-dictionary "en_GB,sv_SE"))


;; ;; Function to switch dictionaries:
;; (defun switch-dictionary-between-swedish-and-english ()
;;   "Switch the current spell-checking dictionary between Swedish and English."
;;   (interactive)
;;   (let* ((current (if (bound-and-true-p ispell-local-dictionary)
;;                       ispell-local-dictionary
;;                     ispell-dictionary))
;;          (new (if (string= current "sv_SE") "en_GB" "sv_SE")))
;;     (ispell-change-dictionary new)
;;     (message "Switched dictionary to %s" new)))

;; (global-set-key (kbd "<f8>") 'switch-dictionary-between-swedish-and-english) ; Bind to F8 key


;; Custom function for zsh-like completion
(defun my/zsh-like-completion ()
  "Complete like zsh - complete until ambiguity."
  (interactive)
  (let ((completion-styles '(basic partial-completion))
        (completion-category-overrides nil)
        (completion-cycle-threshold nil))
    (minibuffer-complete)))


;; Global completion settings
(setq completion-styles '(basic partial-completion)
      completion-category-defaults nil
      completion-category-overrides nil
      completion-ignore-case t)


;; Vertico configuration
(use-package vertico
  :ensure t
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up)
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("TAB" . my/zsh-like-completion))  ; Override TAB in Vertico
  :bind (:map minibuffer-local-map
              ("TAB" . my/zsh-like-completion)) ; Override TAB in minibuffer
  :custom
  (vertico-cycle t)
  (vertico-preselect 'prompt)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-multiform-commands
   '((consult-line buffer)
     (consult-imenu buffer)
     (consult-ripgrep buffer)))
  :config
  (setq vertico-count-format nil)
  :custom-face
  (vertico-current ((t (:background "#1d1f21")))))


;; Marginalia
(use-package marginalia
  :ensure t
  :straight t
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Orderless - simplified configuration
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (basic partial-completion))))))


(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  ;; Add the exceptions
  (setq vterm-keymap-exceptions '("C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y"
                                  "M-i" "M-j" "M-k" "M-l"))
  ;; Define these keys in vterm's own keymap
  :bind (:map vterm-mode-map
              ("M-i" . windmove-up)
              ("M-j" . windmove-left)
              ("M-k" . windmove-down)
              ("M-l" . windmove-right))
  :bind (("C-c t" . vterm)))


;; LaTeX support:

(message "7. LaTeX...")
;; LaTeX configuration for Emacs

;; Set indentation for LaTeX lists
(setq LaTeX-indent-level 2)
(setq LaTeX-item-indent 0)
(setq LaTeX-indent-level-item-continuation 4)

;; Define the indentation function for list environments
(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments. \"\\item\" is indented 'LaTeX-indent-level'
spaces relative to the beginning of the environment.

Continuation lines are indented either twice 'LaTeX-indent-level',
or 'LaTeX-indent-level-item-continuation' if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq-default TeX-engine 'luatex)  ; Set default engine
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-engine 'luatex
        ;; Use PDF mode by default
        TeX-PDF-mode t
        ;; Prevent confirmation for cleaning generated files
        TeX-clean-confirm nil
        ;; Add -shell-escape by default for minted package
        LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)"))
        ;; ;; Swedish quotes
        TeX-quote-language-alist '(("swedish" "\"" "\"" t))
        TeX-quote-language "swedish"))


;; Set up PDF Tools as the viewer
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  ;; Don't ask to reload when the PDF changes
  (setq revert-without-query '(".*"))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Enable useful minor modes
  ;; (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-hook 'post-self-insert-hook
                        (lambda ()
                          (when (eq last-command-event ?-)
                            (message "Dash typed: preceding-char is '%c'"
                                     (char-before (1- (point))))))
                        nil t)))

  ;; Enable minted for code highlighting
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "mintinline"))


;; Enhanced reference management
(use-package reftex
  :ensure t
  :after tex
  :config
  (setq reftex-plug-into-AUCTeX t
        ;; Ensure RefTeX finds your bibliography files
        reftex-default-bibliography '("references.bib")))


;; Completion support
(use-package company-auctex
  :ensure t
  :after (company tex)
  :config
  (company-auctex-init))


;; LSP support through texlab
(use-package lsp-latex
  :ensure t
  :after tex
  :hook ((LaTeX-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-latex-texlab-executable "/usr/bin/texlab"
        lsp-latex-build-on-save t))

;; Preview equations inline
(use-package math-preview
  :ensure t
  :after tex
  :custom
  (math-preview-command "/usr/local/bin/math-preview"))


;; Keybinding for error navigation (Swedish keyboard friendly)
(with-eval-after-load 'tex
  (define-key TeX-mode-map (kbd "C-c f") 'TeX-next-error))


;; Optional: Set up structure folding
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)


;; Set up proper list environment indentation
(with-eval-after-load "latex"
  (add-to-list 'LaTeX-indent-environment-list '("itemize" LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list '("enumerate" LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list '("description" LaTeX-indent-item)))


;; Markdown support:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Pandoc is located at different places:
  (cond
   ((eq system-type 'darwin)
    ;; macOS:
    (setq markdown-command "/usr/local/bin/pandoc"))

   ;; Linux-specific configurations
   ((eq system-type 'gnu/linux)
    (setq markdown-command "/usr/bin/pandoc"))
   )
  :hook (markdown-mode . lsp-deferred)
  :config
  (require 'lsp-marksman)
  )

(add-hook 'markdown-mode-hook (lambda ()
                                ;; (visual-line-mode 1)
                                (display-fill-column-indicator-mode 1)))


(use-package markdown-preview-mode
  :ensure t)

(defun markdown-export-pdf ()
  "Export the current Markdown file to PDF using Pandoc."
  (interactive)
  (let* ((input-file (buffer-file-name))
         (output-file (concat (file-name-sans-extension input-file) ".pdf")))
    (call-process "pandoc" nil nil nil
                  input-file
                  "-o" output-file
                  "--pdf-engine=xelatex"
                  "-V" "geometry:margin=1in")
    (message "Exported to %s" output-file)))

;; Markdown code formatting
(use-package prettier-js
  :ensure t
  :hook (markdown-mode . prettier-js-mode))


;; Completions
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  ;; Make TAB behave less aggressively
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package yasnippet
  :ensure t
  :hook ((python-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


;; ;; Optional: Company AUCTeX integrates Company with AUCTeX
;; (use-package company-auctex
;;   :ensure t
;;   :after (company auctex)
;;   :config (company-auctex-init))

;; ;; Global completion settings
;; (setq completion-styles '(partial-completion basic)
;;       completion-cycle-threshold nil
;;       completion-pcm-complete-word-inserts-delimiters t
;;       completion-ignore-case t
;;       completion-category-defaults nil
;;       completion-category-overrides nil)


;; Enhances minibuffers:
(use-package marginalia
  :ensure t
  :straight t
  :after vertico
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))  ; Cycle through annotation levels
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))


(use-package all-the-icons :ensure t)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


;; Use flycheck to check code:
(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode)
  :hook (after-init . global-flycheck-mode)
  ;; How can this be set on a per project way?
  ;; It seems flake8 doesn't support this?
  :config
  (cond
   ((eq system-type 'darwin)
    (setq flycheck-flake8rc "/Users/johanthor/.config/flake8"))
   ((eq system-type 'gnu/linux)
    (setq flycheck-flake8rc "/home/johanthor/.config/flake8"))
   )
  )

;; tip to automatically set the correct interpreter:
(defun my/set-flycheck-python-interpreter ()
  "Set Flycheck Python interpreter to the one specified by pyenv."
  (let ((pyenv-path (executable-find "python")))
    (setq-local flycheck-python-pyflakes-executable pyenv-path)
    (setq-local flycheck-python-flake8-executable pyenv-path)))

(add-hook 'python-mode-hook #'my/set-flycheck-python-interpreter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python-section:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "9. Python-specifics...")

;; Basic Python settings
(setq python-indent-offset 4)

;; Yasnippet setup
(use-package yasnippet
  :ensure t
  :hook ((python-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;; Add the official snippet collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


;; Pyenv configuration
(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  :config
  (pyenv-mode)
  (when (executable-find "pyenv")
    (setenv "PYENV_ROOT" (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv root")))
    (add-to-list 'exec-path (concat (getenv "PYENV_ROOT") "/shims")))
  :hook (python-mode . pyenv-mode)
  :bind ("C-c C-s" . pyenv-mode-set))


;; Configure eglot to use both pylsp and ruff-lsp
;; (use-package eglot
;;   :ensure t
;;   :hook (python-mode . eglot-ensure)
;;   :config
;;   ;; (add-to-list 'eglot-server-programs
;;   ;; '(python-mode . ("pylsp")))
;;   ;; Add ruff-lsp as an additional server
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("ruff-lsp")))
;;   :bind (:map eglot-mode-map
;;               ("C-c l a" . eglot-code-actions)
;;               ("C-c l r" . eglot-rename)
;;               ("C-c l f" . eglot-format)
;;               ("C-c l d" . eglot-find-declaration))
;;   Customize eglot features
;;   (setq eglot-autoshutdown t  ; shutdown language server after closing last file
;;         eglot-confirm-server-initiated-edits nil))  ; allow server to edit files


;; Optional: If you want to run ruff fixes on save
(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))


;; Company for completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-backends '((company-files          ; Enable filename completion
                            company-keywords         ; Complete programming language keywords
                            company-capf            ; Completion-at-point functions
                            company-yasnippet))))   ; Snippet completion



;; Indentation guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-prefer-character 1
   indent-bars-width-frac 0.9
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:palette ("red" "green" "orange" "cyan") :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5))
  :hook ((python-base-mode) . indent-bars-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (message "C++ stuff...")

;; Define Google C++ style
(c-add-style "google"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist
                (access-label . -)
                (arglist-cont-nonempty . +)
                (arglist-intro . +)
                (case-label . 0)
                (func-decl-cont . +)
                (inclass . +)
                (inher-cont . c-lineup-multi-inher)
                (inline-open . 0)
                (label . /)
                (member-init-intro . +)
                (namespaces . 0)
                (statement-cont . +)
                (substatement-open . 0)
                (template-args-cont . +))))

(use-package clang-format
  :ensure t
  :bind (("C-c f" . clang-format-buffer)
         ("C-c r f" . clang-format-region)))

;; Existing packages
(use-package clang-format
  :ensure t
  :bind (("C-c f" . clang-format-buffer)
         ("C-c r f" . clang-format-region)))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

;; Add LSP support
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (c++-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)) ; if you prefer clang-format to handle formatting

;; Optional but recommended: Add company for completion
(use-package company
  :ensure t
  :hook (c++-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; Your existing settings
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq compile-command "cmake -B build -G Ninja && cmake --build build")

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "google")
            (c-set-offset 'innamespace 0)
            (c-toggle-auto-newline 1)
            (c-toggle-hungry-state 1)
            (c-set-offset 'substatement-open 0)))

(global-set-key (kbd "C-c C-c") 'compile)
;; (global-set-key (kbd "C-c f") 'clang-format-buffer)

;; performance enhancing stuff for LSP:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))


;; (defun my-comment-or-uncomment-region ()
;;   (interactive)
;;   (let ((start (region-beginning))
;;         (end (region-end)))
;;     (if (eq major-mode 'c++-mode)
;;         (save-excursion
;;           (goto-char start)
;;           (insert "/*")
;;           (goto-char (+ end 2))
;;           (insert "*/"))
;;       (comment-or-uncomment-region start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github copilot:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (message "Copilot...")

;; ;; On WSL2, we don't have the right node-version available and therefore
;; ;; we have to skip copilot-installation:
;; (if (not (string-equal (system-name) "SOD-AS104301") or (string-equal (system-name) "sod-as103403"))
;;     (use-package copilot
;;       :ensure t
;;       :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;       :hook ((python-mode . copilot-mode)
;;              (markdown-mode . copilot-mode)
;;              (emacs-lisp-mode . copilot-mode)
;;              (latex-mode . copilot-mode))
;;       :config
;;       (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;       (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;;       (define-key copilot-completion-map (kbd "M-TAB") 'copilot-accept-completion-by-word)
;;       (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-line))

;;   ;; Fix warnings about assignment to free variables:
;;   (defvar ac-disable-inline)
;;   (defvar ac-candidate-menu-min)

;;   ;; complete by copilot first, then auto-complete:
;;   (defun my-tab ()
;;     "Complete by copilot first, then auto-complete."
;;     (interactive)
;;     (or (copilot-accept-completion-by-word)
;;         (ac-expand nil)))

;;   (with-eval-after-load 'auto-complete
;;                                         ; disable inline preview
;;     (setq ac-disable-inline t)
;;                                         ; show menu if have only one candidate
;;     (setq ac-candidate-menu-min 0))

;;   (with-eval-after-load 'company
;;     ;; disable inline previews
;;     (delq 'company-preview-if-just-one-frontend company-frontends)))


;; Local llms with ellama!
;; (message "10. ellama...")
;; (use-package ellama
;;   :bind ("C-c l" . ellama-transient-main-menu)
;;   :init
;;   ;; setup key bindings
;;   (setopt ellama-keymap-prefix "C-c l")
;;   ;; language you want ellama to translate to
;;   (setopt ellama-language "English")
;;   ;; could be llm-openai for example
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;;           (make-llm-ollama
;;            ;; this model should be pulled to use it
;;            ;; value should be the same as you print in terminal during pull
;;            :chat-model "llama3.1:8b-instruct-q8_0"
;;            :embedding-model "nomic-embed-text"
;;            :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   (setopt ellama-summarization-provider
;;           (make-llm-ollama
;;            :chat-model "qwen2.5-coder:3b"
;;            :embedding-model "nomic-embed-text"
;;            :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   (setopt ellama-coding-provider
;;           (make-llm-ollama
;;            :chat-model "qwen2.5-coder:3b"
;;            :embedding-model "nomic-embed-text"
;;            :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   ;; Predefined llm providers for interactive switching.
;;   ;; You shouldn't add ollama providers here - it can be selected interactively
;;   ;; without it. It is just example.
;;   (setopt ellama-providers
;;           '(("llama" . (make-llm-ollama
;;                         :chat-model "llama3.1:8b-instruct-q8_0"
;;                         :embedding-model "llama3.1:8b-instruct-q8_0"))
;;             ("mistral" . (make-llm-ollama
;;                           :chat-model "mistral:7b-instruct-v0.2-q6_K"
;;                           :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
;;             ("mixtral" . (make-llm-ollama
;;                           :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
;;                           :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
;;   ;; Naming new sessions with llm
;;   (setopt ellama-naming-provider
;;           (make-llm-ollama
;;            :chat-model "llama3.1:8b-instruct-q8_0"
;;            :embedding-model "nomic-embed-text"
;;            :default-chat-non-standard-params '(("stop" . ("\n")))))
;;   (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
;;   ;; Translation llm provider
;;   (setopt ellama-translation-provider
;;           (make-llm-ollama
;;            :chat-model "qwen2.5-coder:3b"
;;            :embedding-model "nomic-embed-text"
;;            :default-chat-non-standard-params
;;            '(("num_ctx" . 32768)))))


;; magit:
(message "11. Magit...")
(use-package magit
  :ensure t
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/code" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package magit-filenotify
  :ensure t
  :straight t
  :commands (magit-filenotify-mode)
  :hook (magit-status-mode . magit-filenotify-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Different modes for different cases:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "12. Different modes...")

;; Text mode settings
(add-hook 'text-mode-hook
          (lambda ()
            ;; Basic formatting
            ;; (auto-fill-mode 1)                          ; Auto wrap at fill-column
            (display-fill-column-indicator-mode 1)      ; Show the fill column
            (display-line-numbers-mode 1)               ; Show line numbers

            ;; Spelling
            (flyspell-mode 1)                          ; Enable spell checking

            ;; Sentences
            (setq-local sentence-end-double-space nil) ; Don't require double space after periods

            ;; Paragraphs
            (setq-local paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")  ; Better paragraph detection
            (setq-local paragraph-separate "[ \t\f]*$")

            ;; Navigation and editing
            (show-paren-mode 1)                        ; Highlight matching parentheses
            (electric-pair-local-mode 1)               ; Auto-close parentheses, quotes, etc.

            ;; Word wrap settings
            (setq-local word-wrap t)                   ; Wrap at word boundaries
            (setq-local truncate-lines nil)            ; Enable line wrapping

            ;; Indentation
            (setq-local tab-width 4)                   ; Set tab width
            (setq-local indent-tabs-mode nil)))        ; Use spaces instead of tabs


;; Better word count
(use-package wc-mode
  :ensure t
  :hook (text-mode . wc-mode))


;; Enhanced text navigation
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))


;; Multiple cursors for text editing
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))


;; Define command to format the whole buffer
(defun format-text-buffer ()
  "Format all paragraphs in the current buffer to respect fill-column."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (fill-region (region-beginning) (region-end)))
  (message "Buffer formatted to %d columns" fill-column))

;; Bind it to a key if you want
(global-set-key (kbd "C-c q") 'format-text-buffer)


(use-package sh-script
  :ensure t
  :straight t
  :delight "δ"
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; ;; Enable company mode in shell scripts
;; (defun my-shell-script-mode-setup ()

;;   (Company-Mode 1)) ; Enable Company-Mode

;; (Add-Hook 'Sh-Mode-Hook #'My-shell-script-mode-setup)


(use-package csv-mode
  :ensure t
  :mode ("\\.\\(csv\\|tsv\\)\\'"))
(add-hook 'csv-mode-hook (lambda () (flyspell-mode -1)))


(use-package dockerfile-mode
  :ensure t
  :delight "δ"
  :mode "Dockerfile\\'")


(use-package yaml-mode
  :ensure t)


(use-package toml-mode
  :ensure t)


(use-package json-mode
  :straight t
  :ensure t
  :delight "J"
  :mode (("\\.json\\'" . my-json-mode)
         ("\\.jsonc\\'" . my-jsonc-mode)
         ("\\.json5\\'" . my-jsonc-mode))
  :hook ((json-mode . my/json-mode-setup)
         (before-save . my/json-mode-before-save-hook))
  :preface
  (defun my/json-mode-setup ()
    "Setup function for JSON modes."
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)
    (setq-local indent-tabs-mode nil))

  (defun my/json-mode-before-save-hook ()
    "Format JSON buffer before saving, if in a JSON mode."
    (when (derived-mode-p 'json-mode)
      (when (not (bound-and-true-p my-json-format-disabled))
        (json-pretty-print-buffer))))

  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Print arrays of numbers in one line."
    (let* ((is-all-numbers (catch 'not-all-numbers
                             (dotimes (i (length array))
                               (unless (numberp (aref array i))
                                 (throw 'not-all-numbers nil)))
                             t))
           (json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not is-all-numbers)))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))

  (defun my/toggle-json-format-on-save ()
    "Toggle JSON formatting on save."
    (interactive)
    (setq-local my-json-format-disabled (not (bound-and-true-p my-json-format-disabled)))
    (message "JSON format on save %s" (if my-json-format-disabled "disabled" "enabled")))

  :config
  (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line)

  ;; Base JSON mode with comments
  (define-derived-mode my-json-mode json-mode "JSON"
    "Major mode for editing JSON files."
    (setq-local indent-tabs-mode nil)
    (setq-local js-indent-level 2))

  ;; JSONC mode (JSON with Comments)
  (define-derived-mode my-jsonc-mode my-json-mode "JSONC"
    "Major mode for editing JSON files with C-style comments."
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+\\s-*")
    (setq-local comment-use-syntax t)
    (modify-syntax-entry ?/ ". 124b" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    (modify-syntax-entry ?\n "> b" syntax-table))

  (define-key json-mode-map (kbd "C-c C-t") 'my/toggle-json-format-on-save)
  (define-key json-mode-map (kbd "C-c C-f") 'json-pretty-print-buffer))


(use-package json-navigator
  :straight t
  :after json-mode)


(use-package prettier
  :straight t
  :hook ((json-mode . prettier-mode)
         (my-jsonc-mode . prettier-mode)))

;;; config.el ends here
