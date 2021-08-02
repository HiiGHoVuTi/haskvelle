
module Colors (
  Color(..), (#), colorCommand, colorCustom, prettyPrint
              ) where

import System.Console.StructuredCLI
import Text.Pretty.Simple (pPrint)

data Color
  = OK
  | Name
  | Text
  | Warning
  | Error
  | Success
  | Normal


escape :: Color -> String
escape col = "\ESC[" <> escape' col <> "m"

escape' :: Color -> String
escape' OK      = "32"
escape' Name    = "35;1"
escape' Text    = "36;3"
escape' Warning = "33;3"
escape' Error   = "91;1"
escape' Success = "92;1"
escape' Normal  = "0"

(#) :: String -> Color -> String
(#) src col = escape col <> src <> escape Normal

colorCommand :: Monad m => String -> String -> m Action -> CommandsT m ()
colorCommand name help = command name (help #Text)

colorCustom :: Monad m => String -> String -> Handler m [String] -> CommandsT m ()
colorCustom name help = custom name (help #Text) parser (return True)
  where parser = paramParser help valid
        valid  = return . return . wordsWhen (== ',')

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

prettyPrint :: Show a => a -> IO ()
prettyPrint = pPrint
