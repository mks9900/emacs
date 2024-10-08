;; init.el --- GNU Emacs Configuration

;; Commentary:

;; Following lines build the configuration code out of the config.el file.

;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 100 1024 1024))
(setq default-directory "~/")

(if (file-exists-p (expand-file-name "config.el" user-emacs-directory))
    (load-file (expand-file-name "config.el" user-emacs-directory))
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

;; init.el ends here
