
module Colors (
  Color(..), (#), colorCommand, prettyPrint
              ) where

import CLI
import Text.Pretty.Simple (pPrint)

-- | The enum of all possible colors (ANSI terminal)
data Color
  = OK
  | Name
  | Text
  | Warning
  | Error
  | Success
  | Normal

-- | ANSI-escapes a color, coloring every subsequent character printed
escape :: Color -> String
escape col = "\ESC[" <> escape' col <> "m"

-- | A map of all ANSI Color-Codes
escape' :: Color -> String
escape' OK      = "32"
escape' Name    = "35;1"
escape' Text    = "36;3"
escape' Warning = "33;3"
escape' Error   = "91;1"
escape' Success = "92;1"
escape' Normal  = "0"

-- | An operator which colors a certain string, and sets back the terminal color to normal afterwards
(#) :: String -> Color -> String
(#) src col = escape col <> src <> escape Normal

-- | A utility for coloring velle commands' help text
colorCommand :: String -> String -> ([String] -> IO ()) -> Atom
colorCommand n h fn = Leaf
                         { name = n
                         , help = h #Text
                         , func = fn
                         }

-- | Renames the pPrint function
prettyPrint :: Show a => a -> IO ()
prettyPrint = pPrint
