;;;; List of personal layers
(configuration-layer/declare-layers '(
                                      git
                                      markdown
                                      clojure
                                      javascript
                                      themes-megapack
                                      rust
                                      (elm :variables
                                           elm-tags-exclude-elm-stuff nil)
                                      (go :variables
                                          go-tab-width 4)
                                      ))
