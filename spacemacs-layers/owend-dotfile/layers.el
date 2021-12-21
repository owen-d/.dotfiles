;;;; List of personal layers
(configuration-layer/declare-layers '(
                                      html
                                      csv
                                      yaml
                                      ;; git
                                      ;; themes-megapack
                                      (rust :variables
                                            rust-format-on-save t)
                                      (markdown :variables markdown-live-preview-engine 'vmd)
                                      (go :variables
                                          go-format-before-save t
                                          gofmt-command "goimports"
                                          godoc-at-point-function 'godoc-gogetdoc
                                          go-use-golangci-lint t)
                                      dap
                                      org
                                      auto-completion
                                      (haskell :variables
                                               haskell-completion-backend 'lsp
                                               lsp-haskell-process-path-hie "haskell-language-server-wrapper"
                                               ;; intero-package-version "0.1.40"
                                               ;; haskell-enable-hindent t
                                               ;; haskell-enable-hindent-style "johan-tibell"
                                               ;; haskell-stylish-on-save t
                                               ;; haskell-completion-backend 'intero
                                               )
                                      docker
                                      nixos
                                      jsonnet
                                      (lsp :variables
                                           lsp-prefer-flymake nil
                                           lsp-eldoc-render-all t
                                           ;; ;; should be auto enabled
                                           ;; lsp-ui-flycheck-enable t
                                           lsp-ui-doc-enable nil
                                           ;; lsp-rust-server 'rls
                                           lsp-rust-server 'rust-analyzer
                                           )
                                      terraform
                                      ))
