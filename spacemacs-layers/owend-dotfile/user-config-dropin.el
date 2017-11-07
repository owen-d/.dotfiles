(spacemacs/toggle-smartparens-globally-on)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
;; ludwig mode cloned into ~/lib/emacs from https://github.com/fugue/ludwig-mode
(add-to-list 'load-path "~/lib/emacs/ludwig-mode/")
(autoload 'ludwig-mode "ludwig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lw\\'" . ludwig-mode))
