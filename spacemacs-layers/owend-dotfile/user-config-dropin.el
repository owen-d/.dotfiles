(spacemacs/toggle-smartparens-globally-on)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
;; ludwig mode cloned into ~/lib/emacs from https://github.com/fugue/ludwig-mode
(add-to-list 'load-path "~/lib/emacs/ludwig-mode/")
(autoload 'ludwig-mode "ludwig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lw\\'" . ludwig-mode))

;; org mode posthook
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

;; osx system copy/paste integration from terminal -- paste currently breaks convenience features
;; like yy-p for pasting on a new inserted line below,
;; so we just enable copying kill ring into system clipboard

(defun copy-to-osx (text &optional push)
    (let ((process-connection-type nil))
        (let ((proc (start-process-shell-command "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

;; ;; old osx specific variant
;; (when (eq system-type 'darwin)
;;   (setq interprogram-cut-function 'copy-to-osx))

;; new variant which works with bash aliases (pbcopy is not exec'able when it's aliased via xclip :D)
;; It requires interactive login shells, thus the command-switch addition
(setq interprogram-cut-function 'copy-to-osx)
(setq shell-file-name "bash")
(setq shell-command-switch "-ilc")

;; append custom themes
(setq dotspacemacs-themes (append dotspacemacs-themes '(wombat tsdh-dark misterioso whiteboard)))

