import qualified P

import System.Environment

main = do
    [ fp ] <- getArgs
    p @ (P.Poly xs) <- P.get fp
    print $ length xs

