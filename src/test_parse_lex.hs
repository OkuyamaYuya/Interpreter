
import Lex
import Parse
import Base
import qualified Token
import qualified Syntax

main = do
    line <- getLine
    let res = Parse.parse $ Lex.scanTokens line
    print res
