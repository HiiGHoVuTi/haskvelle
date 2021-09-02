
module Colors (
  Color(..), (#), colorCommand, prettyPrint
              ) where

import CLI
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

colorCommand :: String -> String -> ([String] -> IO ()) -> Atom
colorCommand n h fn = Leaf
                         { name = n
                         , help = h #Text
                         , func = fn
                         }

prettyPrint :: Show a => a -> IO ()
prettyPrint = pPrint
