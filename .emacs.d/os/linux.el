; load haskell lint from cabal
(haskell-lint
  (concat
    (glob-last (glob-last "~/.cabal/share/" "x86_64-linux-ghc-.*") "hlint-*")
   "/hs-lint.el")
)

; load haskell ghc-mode from cabal
(haskell-ghc-mode
  (concat
    (glob-last (glob-last "~/.cabal/share/" "x86_64-linux-ghc-.*") "ghc-mod-*")
   "/ghc.el")
)
