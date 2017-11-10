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
                                      (go :variables
                                          go-tab-width 4)
                                      ))
