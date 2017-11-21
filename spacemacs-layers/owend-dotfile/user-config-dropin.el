(spacemacs/toggle-smartparens-globally-on)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
;; ludwig mode cloned into ~/lib/emacs from https://github.com/fugue/ludwig-mode
(add-to-list 'load-path "~/lib/emacs/ludwig-mode/")
(autoload 'ludwig-mode "ludwig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lw\\'" . ludwig-mode))

;; org mode posthook
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

;; osx system copy/paste integration from terminal
(defun paste-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun copy-to-osx (text &optional push)
    (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

(when (eq system-type 'darwin)
  (setq interprogram-cut-function 'copy-to-osx)
  (setq interprogram-paste-function 'paste-from-osx))
