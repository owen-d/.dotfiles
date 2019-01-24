;;;; List of personal layers
(configuration-layer/declare-layers '(
                                      git
                                      markdown
                                      (clojure :variables
                                               clojure-enable-fancify-symbols t)
                                      javascript
                                      ;; themes-megapack
                                      rust
                                      (markdown :variables markdown-live-preview-engine 'vmd)
                                      (elm :variables
                                           elm-tags-exclude-elm-stuff nil
                                           elm-reactor-address "0.0.0.0"
                                           elm-sort-imports-on-save t)
                                      ;; make sure to install dif version of gocode for post 1.10 go https://github.com/mdempsky/gocode
                                      (go :variables
                                          go-tab-width 4)
                                      org
                                      typescript
                                      auto-completion
                                      (haskell :variables
                                               haskell-enable-hindent-style "johan-tibell"
                                               haskell-stylish-on-save t
                                               haskell-completion-backend 'intero)
                                      (python :variables python-indent 4
                                              python-indent-offset 4)
                                      docker
                                      nixos
                                      ))
