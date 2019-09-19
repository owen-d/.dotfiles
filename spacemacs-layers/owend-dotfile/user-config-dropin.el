(spacemacs/toggle-smartparens-globally-on)

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

;; new variant which works with bash aliases (pbcopy is not exec'able when it's aliased via xclip :D)
;; It requires interactive login shells, thus the command-switch addition
(setq interprogram-cut-function 'copy-to-osx)
(setq shell-file-name "bash")
(setenv "BASH_ENV" "~/.bashrc")

;; append custom themes
(setq dotspacemacs-themes (cons 'zerodark
                                (append dotspacemacs-themes '(wombat tsdh-dark misterioso whiteboard))))

;; use ripgrep
(evil-leader/set-key "/" 'spacemacs/helm-project-do-ag)
(setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case --hidden")

(setq dotspacemacs-additional-packages (cons 'nodejs-repl dotspacemacs-additional-packages))
(spacemacs/set-leader-keys-for-major-mode 'js2-mode
  "ne" 'nodejs-repl-send-last-expression
  "nb" 'nodejs-repl-send-buffer
  "ns" 'nodejs-repl-switch-to-repl
  "nr" 'nodejs-repl-send-region
  "nf" 'nodejs-repl-load-file
)
