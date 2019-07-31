;;;; List of personal layers
(configuration-layer/declare-layers '(
                                      html
                                      csv
                                      yaml
                                      git
                                      (clojure :variables
                                               clojure-enable-fancify-symbols t)
                                      (javascript :variables
                                                  js2-basic-offset 2
                                                  js-indent-level 2)
                                      ;; themes-megapack
                                      rust
                                      (markdown :variables markdown-live-preview-engine 'vmd)
                                      (elm :variables
                                           elm-tags-exclude-elm-stuff nil
                                           elm-reactor-address "0.0.0.0"
                                           elm-sort-imports-on-save t)
                                      ;; try setting autobuild (works well with dep at the moment)
                                      ;; gocode set autobuild true
                                      (go :variables
                                          go-tab-width 4
                                          go-format-before-save t)
                                      org
                                      (typescript :variables
                                                  typescript-indent-level 2
                                                  typescript-fmt-on-save t)
                                      auto-completion
                                      (haskell :variables
                                               intero-package-version "0.1.40"
                                               haskell-enable-hindent t
                                               haskell-enable-hindent-style "johan-tibell"
                                               haskell-stylish-on-save t
                                               haskell-completion-backend 'intero)
                                      ;; pip install autoflake yapf isort
                                      (python :variables python-indent 4
                                              python-enable-yapf-format-on-save t
                                              python-sort-imports-on-save t
                                              python-indent-offset 4)
                                      docker
                                      nixos
                                      ))
