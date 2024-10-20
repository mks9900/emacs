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
;; cargo is used to build texlab later on.
;; Also, install the following:
;; $ sudo snap install marksman # markdown LSP-server
;; $ npm install -g prettier # markdown formatting in Emacs
;; $ npm install dictionary-sv
;; $ npm install dictionary-en-gb
;; $ sudo npm install -g pyright
;; $ cargo install --git https://github.com/latex-lsp/texlab.git
;;
;; Every other package should be installed using use-package.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set repositories to use:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading packages...")

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))


;; Not necessary since Emacs >= 29?
;; (unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  ;; (package-install 'use-package))


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
;; (setq straight-check-for-modifications '(find-when-checking))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Applying global settings...")
;; Always use utf-8:
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)


;; Apply line wrapping only to text and markdown:
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)


;; Set the width of the line numbers to be fixed, depending
;; on the number of lines.
(setq display-line-numbers-width-start t)

;; Don't produce backup-files:
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)


(defvar python-indent-offset)
(setq python-indent-offset 4)


;; Stop Emacs from hiding:
(unbind-key "C-z") ;; suspend-frame


;; Stop Emacs from throwing up buffers with warnings:
(defvar warning-minimum-level)
(setq warning-minimum-level :emergency)


;; Mark whole buffer:
(global-set-key (kbd "C-c a") 'mark-whole-buffer)


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
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
(global-set-key (kbd "C-1") 'comment-or-uncomment-line-or-region)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specifics:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "OS-specifics...")

(defvar mac-right-option-modifier)
(defvar mac-command-modifier)
(cond
 ((eq system-type 'darwin)
  ;; macOS keybinding-fixes:
  (setq mac-right-option-modifier 'nil)
  (setq mac-command-modifier 'control
        select-enable-clipboard t)
  (cond
   ((string-equal (system-name) "macbook15-macos.vilanelva.se")
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (set-face-attribute 'default nil :font "Source Code Pro" :height 180)
    )
   )
  )

 ;; Linux-specific configurations
 ((eq system-type 'gnu/linux)
  (defvar xdg-bin (getenv "XDG_BIN_HOME"))
  (defvar xdg-cache (getenv "XDG_CACHE_HOME"))
  (defvar xdg-config (getenv "XDG_CONFIG_HOME"))

  (cond
   ((string-equal (system-name) "rocky-ws")
    (set-frame-size (selected-frame) 150 70)
    (set-frame-position (selected-frame) 850 0)
    (set-face-attribute 'default nil :font "Source Code Pro" :height 160)
    )
   ((string-equal (system-name) "macbook13-linux")
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (set-face-attribute 'default nil :font "Source Code Pro" :height 200)
    )
   ((string-equal (system-name) "sodra-ds-test")
    (set-frame-size (selected-frame) 150 50)
    (set-frame-position (selected-frame) 10 10)
    (set-face-attribute 'default nil :font "Source Code Pro" :height 200)
    )
   ;; WSL is also reported as Linux :-)
   ((string-equal (system-name) "SOD-AS104301")
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (set-face-attribute 'default nil :font "Source Code Pro" :height 140)
    )
   ((string-equal (system-name) "sod-as103403")
    (set-frame-size (selected-frame) 160 90)
    ;; (set-frame-position (selected-frame) 100 100)
    (set-face-attribute 'default nil :font "Source Code Pro" :height 240)
    )
   )
  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and feel:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Look and feel...")
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
  (setq dimmer-fraction 0.40)
  (setq dimmer-delay 0.5) ; Adjust the delay in seconds

  
  ;; Exclude some buffers from being dimmed
  (add-to-list 'dimmer-exclusion-regexp-list "^\*helm")
  (add-to-list 'dimmer-exclusion-regexp-list "^\*Minibuf")
  
  ;; Configure and activate dimmer
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-mode t))


;; Mouse scroll speed:
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;; Set a nice theme:
;; (straight-use-package 'material-theme)
;; (load-theme 'material t)

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


;; Below is a one-liner to suppress a warning...
(defvar doom-themes-treemacs-theme)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Ensure this is set after doom-themes has been loaded
  (with-eval-after-load 'doom-themes
    (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-height 25)
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-env-version t)
  ;; (setq doom-modeline-env-enable-python t)
  ;; (setq doom-modeline-env-python-executable "python-shell-interpreter") ; was: "python" or `python-shell-interpreter'
  )


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
(message "Misc settings...")
(setq-default
 ad-redefinition-action 'accept                      ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                    ; Hide the cursor in inactive windows
 display-time-default-load-average nil               ; Don't display load average
 fill-column 88                                      ; Set width for automatic line breaks
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
(message "Addons and customisations...")

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


;; Vertico provides vertical interactive mode for autocompletion when opening files etc.:
(use-package vertico
  :ensure t
  :straight (:files (:defaults "extensions/*"))
  :init (vertico-mode)
  ;; Make completion case-insensitive
  (setq completion-ignore-case t)
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :custom (vertico-cycle t)
  :custom-face (vertico-current ((t (:background "#1d1f21")))))


;; Enhances minibuffers:
(use-package marginalia
  :ensure t
  :straight t
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))


;; Spell checking:
(use-package ispell
  :ensure nil ; `ispell` is bundled with Emacs, so we don't need to install it...
  :config
  ;; Set Hunspell as the default spell checker
  (setq ispell-program-name "hunspell")
  ;; Set the default dictionaries
  (setq ispell-dictionary "en_GB,sv_SE"))

;; Isn't this set in .zshrc?
;; (cond
;;  ((eq system-type 'darwin)
;;   ;; macOS:
;;   (setenv "DICPATH" "/Users/johanthor/Library/Spelling:")
;;   )
 
;;  ;; Linux-specific configurations
;;  ((eq system-type 'gnu/linux)
;;   (setenv "DICPATH" "/home/johanthor/.local/share/spelling:")
;;   )
;;  )


;; Function to switch dictionaries:
(defun switch-dictionary-between-swedish-and-english ()
  "Switch the current spell-checking dictionary between Swedish and English."
  (interactive)
  (let* ((current (if (bound-and-true-p ispell-local-dictionary)
                      ispell-local-dictionary
                    ispell-dictionary))
         (new (if (string= current "sv_SE") "en_GB" "sv_SE")))
    (ispell-change-dictionary new)
    (message "Switched dictionary to %s" new)))

(global-set-key (kbd "<f8>") 'switch-dictionary-between-swedish-and-english) ; Bind to F8 key


;; ;; Decide which modes to spellcheck:
;; (dolist (hook '(text-mode-hook
;;                 markdown-mode-hook
;;                 latex-mode-hook))
;;                 ;; python-mode-hook)) ; Add other modes as needed.
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; ;; Use below to not spellcheck code:
;; (dolist (hook '(python-mode-hook)) ; Add other programming modes as needed.
;;   (add-hook hook (lambda () (flyspell-prog-mode))))


;; LaTeX support:

(message "LaTeX...")
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar TeX-master)
(defvar TeX-engine)
(defvar TeX-view-program-selection)
(defvar TeX-view-program-list)
(defvar LaTeX-item-indent)
(defvar LaTeX-indent-level-item-continuation)
(defvar TeX-quote-language-alist)
(defvar TeX-quote-language)



(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
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

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;; Enable automatic saving and parsing of TeX files
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil  ;; Query for the master file
        TeX-quote-language-alist '(("swedish" "\"" "\"" t))
        TeX-quote-language "swedish"
        TeX-engine 'xetex)
  
  ;; Modify LaTeX command to include -shell-escape for all engines
  (setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
  
  ;; Optional: Set the PDF viewer (e.g., Skim on macOS, evince on Linux)
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (cond
   ((eq system-type 'darwin)
    ;; macOS:
    (setq TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/MacOS/Skim"))))
   ((eq system-type 'gnu/linux)
    ;; Linux:
    (setq TeX-view-program-list '(("PDF Viewer" "evince %o")))))
  
  ;; Enable useful minor modes
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
  ;; Enable parsing of minted package
  (add-to-list 'LaTeX-verbatim-environments "minted")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "mintinline"))

;; Ensure -shell-escape is used for compilation
(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-command-list)
     (add-to-list 'TeX-command-list
                  '("LaTeX" "%`%l%(mode) -shell-escape%' %t" TeX-run-TeX nil t))))

;; Since C-c ` is a little tricky to type on my Swedish keyboard,
;; I've assigned it to this instead:
(with-eval-after-load 'tex
  (define-key TeX-mode-map (kbd "C-c f") 'TeX-next-error))

;; Tell Emacs to treat the minted environment like verbatim
(require 'latex)
(add-to-list 'LaTeX-verbatim-environments "minted")


;; (defun refresh-latex-font-lock ()
;;   "Refresh font-lock for LaTeX after minted environment."
;;   (when (derived-mode-p 'latex-mode)
;;     (font-lock-refresh-defaults)))

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook #'refresh-latex-font-lock nil t)))



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


(use-package lsp-latex
  :ensure t
  :hook ((LaTeX-mode . lsp)  ;; or (TeX-mode . lsp) for some setups
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; Set the path to the TexLab server if it's not automatically detected
  ;; Pandoc is located at different places:
  (cond
   ((eq system-type 'darwin)
    ;; macOS:
    (setq lsp-latex-texlab-executable "/usr/local/bin/texlab")
    )
   
   ;; Linux-specific configurations
   ((eq system-type 'gnu/linux)
    (setq lsp-latex-texlab-executable "/home/johanthor/.cargo/bin/texlab")
    )
   )

  ;; Customization options as needed
  )


(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp)
  )

;; Previewing equations inline from LaTeX:
(use-package math-preview
  :ensure t
  :custom
  ;; Below is the same for macOS and Linux, install through npm:
  (math-preview-command "/usr/local/bin/math-preview")
  ;; (cond
  ;;  ((eq system-type 'darwin)
  ;;   ;; macOS:
  ;;   (math-preview-command "/usr/local/bin/math-preview")
  ;;   )
   
  ;;  ;; Linux-specific configurations
  ;;  ((eq system-type 'gnu/linux)
  ;;   (math-preview-command "/usr/local/bin/math-preview")
  ;;   )
  ;;  )
  )


;; Use LSP:
(message "LSP...")
(defvar lsp-pyright-venv-path)
(defvar lsp-pyright-auto-search-paths)
(defvar lsp-pyright-use-library-code-for-types)
(defvar lsp-ui-flycheck-enable)
(defvar lsp-ui-flycheck-list-position)
(defvar lsp-ui-flycheck-live-reporting)

(use-package lsp-mode
  :ensure t
  :hook ((lisp-mode . lsp)
         (scheme-mode . lsp)
         (python-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :config
  (setq lsp-enable-snippet nil)  ; Disable snippets support. Set to t if you want snippets.
  (setq lsp-pyright-venv-path (expand-file-name "~/.pyenv/versions")) ; Adjust to your pyenv versions path
  ;; Auto-detect pyenv virtual environments
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-use-library-code-for-types t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))


;; Completions
(use-package company
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
         (after-init . global-company-mode))
  :config
  (add-to-list 'company-backends 'company-files) ; Add company-files for path completion
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ; Decrease delay before autocompletion popup shows
  (setq company-backends '((company-files          ; Enable filename completion
                            company-keywords       ; Complete programming language keywords
                            company-capf          ; Completion-at-point functions (e.g., LSP)
                            company-yasnippet)))  ; Snippet completion
  )


;; Optional: Company AUCTeX integrates Company with AUCTeX
(use-package company-auctex
  :ensure t
  :after (company auctex)
  :config (company-auctex-init))


;; Fuzzy completion of filenames etc.:
(defvar helm-autoresize-mode)
(defvar helm-autoresize-max-height)
(defvar helm-mode-fuzzy-match)
(defvar helm-completion-in-region-fuzzy-match)

(use-package helm
  :ensure t
  :config
  ;; Let Helm decide best size of minibuffer:
  (helm-autoresize-mode 1)
  ;; Relative sizes of the above:
  ;; (setq helm-autoresize-max-height 30) ; Max height in percentage of frame height.
  ;; (setq helm-autoresize-min-height 20)) ; Min height in percentage of frame height.
  ;; Set a fixed height:
  ;; (setq helm-display-buffer-height 20) ; Set the fixed height of the Helm window.

  ;; Enable fuzzy matching for all Helm commands
  ;; (setq helm-mode-fuzzy-match t)
  ;; (setq helm-completion-in-region-fuzzy-match t))

;; (helm-mode 1)

;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Sometimes Emacs complains about file-notify being missing, then use polling:
;; (setq file-notify--library 'polling)


(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)


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
(message "Python-specifics...")

;; Pyenv:
(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  :config
  (pyenv-mode))

(when (executable-find "pyenv")
  (setenv "PYENV_ROOT" (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv root")))
  (add-to-list 'exec-path (concat (getenv "PYENV_ROOT") "/shims")))

(add-hook 'python-mode-hook 'pyenv-mode)
(global-set-key (kbd "C-c C-s") 'pyenv-mode-set) ; Set key binding to switch pyenv environments


;; Indentation guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
   (indent-bars-treesit-ignore-blank-lines-types '("module"))
   (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
       			              list list_comprehension
       			              dictionary dictionary_comprehension
       			              parenthesized_expression subscript)))
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-prefer-character 1
   ;; indent-bars-pattern ".*.*.*.*"
   indent-bars-width-frac 0.9
   indent-bars-pad-frac 0.2
   indent-bars-zigzag 0.1
   indent-bars-color-by-depth '(:palette ("red" "green" "orange" "cyan") :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5))
  :hook
  ((python-base-mode) . indent-bars-mode))




;; ;; Experimental elpy integration...
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))


;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))


;; ;; what do set here?
;; ;; (setq elpy-rpc-python-command "python3")
;; ;; (setq python-shell-interpreter "python3")

;; ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; ;; (add-hook 'elpy-mode-hook 'flycheck-mode)


;; (defun my/elpy-set-python-interpreter ()
;;   "Set the Python interpreter for elpy based on the active pyenv version."
;;   (let ((pyenv-version-name (pyenv-mode-version)))
;;     (when pyenv-version-name
;;       (setq elpy-rpc-python-command (concat "~/.pyenv/versions/" pyenv-version-name "/bin/python"))
;;       (setq python-shell-interpreter elpy-rpc-python-command))))

;; (add-hook 'pyenv-mode-hook 'my/elpy-set-python-interpreter)


;; ;; find a .python-version and activate it...
;; (defun my/activate-pyenv-virtualenv ()
;;   "Automatically activate the virtualenv that matches the .python-version."
;;   (let ((pyenv-version (pyenv-mode-version)))
;;     (when pyenv-version
;;       (pyvenv-activate (concat "~/.pyenv/versions/" pyenv-version)))))

;; (add-hook 'python-mode-hook 'my/activate-pyenv-virtualenv)


;; (defun my/pyvenv-activate-pyenv ()
;;   "Activate a pyenv virtual environment by selecting from a list of available pyenv environments."
;;   (interactive)
;;   (let* ((root "~/.pyenv/versions") ; Adjust if your pyenv versions are stored elsewhere
;;          (envs (mapcar (lambda (dir)
;;                          (substring dir (length root) (length dir)))
;;                        (directory-files root t directory-files-no-dot-files-regexp)))
;;          (chosen-env (completing-read "Choose pyenv environment: " envs)))
;;     (pyvenv-activate (expand-file-name chosen-env root))))

;; (global-set-key (kbd "C-c M-p") 'my/pyvenv-activate-pyenv)





;; ;;; end elpy experiment




;; Misc Python goodies:
(use-package buftra
  :ensure t
  :straight (:host github :repo "humitos/buftra.el"))


(use-package py-pyment
  :ensure t
  :straight (:host github :repo "humitos/py-cmd-buffer.el")
  :config
  (setq py-pyment-options '("--output=numpydoc")))


;; py-isort below seems to depend on this:
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;; Set up your preferred keymap prefix, e.g., "C-c p"
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package py-isort
  :ensure t
  :straight (:host github :repo "humitos/py-cmd-buffer.el")
  :hook (python-mode . py-isort-enable-on-save)
  :config
  (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca")))


(use-package py-autoflake
  :ensure t
  :straight (:host github :repo "humitos/py-cmd-buffer.el")
  :hook (python-mode . py-autoflake-enable-on-save)
  :config
  (setq py-autoflake-options '("--expand-star-imports")))


(use-package py-docformatter
  :ensure t
  :straight (:host github :repo "humitos/py-cmd-buffer.el")
  :hook (python-mode . py-docformatter-enable-on-save)
  :config
  (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))


(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '88))


(use-package py-isort
  :straight t
  :hook ((before-save . py-isort-before-save)
         (python-mode . py-isort-enable-on-save))
  :config
  (setq py-isort-options '("-l=88" "--profile=black")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github copilot:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Copilot...")

;; On WSL2, we don't have the right node-version available and therefore
;; we have to skip copilot-installation:
(if (not (string-equal (system-name) "SOD-AS104301") or (string-equal (system-name) "sod-as103403"))
    (use-package copilot
      :ensure t
      :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
      :hook ((python-mode . copilot-mode)
             (markdown-mode . copilot-mode)
             (emacs-lisp-mode . copilot-mode)
             (latex-mode . copilot-mode))
      :config
      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "M-TAB") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-line))

  ;; Fix warnings about assignment to free variables:
  (defvar ac-disable-inline)
  (defvar ac-candidate-menu-min)

  ;; complete by copilot first, then auto-complete:
  (defun my-tab ()
    "Complete by copilot first, then auto-complete."
    (interactive)
    (or (copilot-accept-completion-by-word)
        (ac-expand nil)))

  (with-eval-after-load 'auto-complete
                                        ; disable inline preview
    (setq ac-disable-inline t)
                                        ; show menu if have only one candidate
    (setq ac-candidate-menu-min 0))

  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends)))


;; magit:
(message "Magit...")
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
(message "Different modes...")
(use-package sh-script
  :ensure t
  :straight t
  :delight "δ"
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Enable company mode in shell scripts
(defun my-shell-script-mode-setup ()

  (Company-Mode 1)) ; Enable Company-Mode

(Add-Hook 'Sh-Mode-Hook #'My-shell-script-mode-setup)


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


;; json:
(use-package json-mode
  :straight t
  :ensure t
  :delight "J"
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))
  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Print the arrays of numbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config 
  (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line)
  
  ;; Define a custom JSON mode that supports C-style comments
  (define-derived-mode my-json-mode json-mode "JSON with C comments"
    "Major mode for editing JSON files with C-style comments."
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+\\s-*"))

  ;; Use the custom mode for .json files
  (add-to-list 'auto-mode-alist '("\\.json\\'" . my-json-mode)))

;; Optional: Add a function to toggle between standard JSON mode and custom JSON mode
(defun toggle-json-mode ()
  "Toggle between standard JSON mode and custom JSON mode with C-style comments."
  (interactive)
  (if (eq major-mode 'my-json-mode)
      (json-mode)
    (my-json-mode))
  (message "Switched to %s" major-mode))

;; Optional: Bind the toggle function to a key
(global-set-key (kbd "C-c t j") 'toggle-json-mode)


;;; config.el ends here
