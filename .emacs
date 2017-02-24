;;; File: .emacs
;;; Author: GNU Administrator 
;;;
;;; If you are looking for the FRB Emacs initializaion procedures,
;;; take a look at the file: 
;;;
;;;           /usr/gnu/lib/emacs/site-lisp/default.el
;;;
;;; Unless you prevent it, the system default.el file will be loaded after
;;; your own .emacs file.  Finally, your own ~/.emacs-19_setup will
;;; be loaded. 
;;;
;;; Neither a .emacs nor a .emacs-19_setup is required.
;;;

    (setq inhibit-startup-message t)  ; Don't show the GNU message.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(TeX-PDF-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;;; This brings in a Mathematica mode into emacs
(add-to-list `load-path "~/.emacs.d/modes/")
(setq wolfram-program "/opt/mathematica10/math")
(add-to-list `auto-mode-alist '("\\.mth$" . wolfram-mode))
(autoload `wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
