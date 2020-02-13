module NixAST where

import Partial.Unsafe (unsafeCrashWith)

data NixAST

print :: NixAST -> String
print = \_ -> unsafeCrashWith "TODO: printNixExpr"
