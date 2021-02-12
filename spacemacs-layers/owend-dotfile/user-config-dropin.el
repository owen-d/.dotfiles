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
;; load bashrc (which in turn expands aliases)
(setq interprogram-cut-function 'copy-to-osx)
(setq shell-file-name "bash")
(setenv "BASH_ENV" "~/.dotfiles/.bashrc")

;; append custom themes
(setq dotspacemacs-themes (cons 'zerodark
                                (append dotspacemacs-themes '(wombat tsdh-dark misterioso whiteboard))))

;; use ripgrep
(evil-leader/set-key "/" 'spacemacs/helm-project-do-ag)
(setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case --hidden -g !vendor")

(spacemacs|use-package-add-hook dap-mode
  :post-config
  (dap-register-debug-template
   "loki-local"
   (list :type "go"
         :request "launch"
         :name "loki-local"
         :mode "debug"
         :program nil
         :buildFlags "-gcflags '-N -l'"
         :args "--config.file /Users/owendiehl/grafana/loki/cmd/loki/loki-local-config.yaml"
         :env nil
         :envFile nil))
  )

