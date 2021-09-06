import Text.ParserCombinators.Parsec
-- import Data.Char
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.State as S

interpret :: String -> String
interpret input = case parse parseBF "BRAINFUCK" input of
    Left err -> error "syntax error: " ++ show err
    Right val -> val

validCommands :: String
validCommands = "<>+-.,[]"

command :: Parser Char
command = oneOf validCommands

comment :: Parser Char
comment = noneOf validCommands

parseBF :: Parser String
parseBF = fmap concat $ sepBy (many command) (many1 comment)

type ListZipper a = ([a,][a])

type Program S.StateT (ListZipper Int) IO

increment :: Program ()
increment S.modify $ \ (x:xs,xss) -> (x + 1:xs, xss)

decrement :: Program ()
decrement S.modify $ \ (x:xs,xss) -> (x - 1:xs, xss)

zipLeft :: ListZipper a -> ListZipper a 
zipLeft (xs,x:xss) = (xs,x:xss)

zipRight :: ListZipper a -> ListZipper a
zipRight (x:xs,xss) = (xs,x:xss)

goLeft :: Program ()
goLeft = S.modify zipLeft

goRight :: Program ()
goRight = S.modify zipRight

execute ::
