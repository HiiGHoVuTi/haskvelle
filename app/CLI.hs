
{-# LANGUAGE RecordWildCards #-}

module CLI (
  run,
  helpT,
  exists,
  Atom(..),
  (>+),
           ) where

import Control.Monad

data Atom
  = Node
    { name :: String
    , help :: String
    , children :: [Atom]
    }
  | Leaf
    { name :: String
    , help :: String
    , func :: [String] -> IO ()
    }

(>+) :: Atom -> [Atom] -> Atom
(>+) Node {..} new = Node {children = children ++ new, ..}
(>+) Leaf {..} new = Node {children =             new, ..}


--

runA :: [String] -> Atom -> IO ()

runA [] _ = return ()
runA (x:rest) Node{..}
  | name == x = forM_ children (runA rest)
runA (x:rest) Leaf{..}
  | name == x = func rest
runA _ _ = return ()



existsA :: [String] -> Atom -> IO Bool

existsA [] _ = return False

existsA (x:rest) Node{..}
  | name == x = any (== True) <$> forM children (existsA rest)

existsA (x:_) Leaf{..}
  | name == x = return True

existsA _ _ = return False
--

run :: [Atom] -> [String] -> IO ()
run ats args = forM_ ats (runA args)

exists :: [Atom] -> [String] -> IO Bool
exists ats args = any (== True) <$> forM ats (existsA args)

helpT :: [Atom] -> IO ()
helpT a = do
  _ <- putStrLn "Velle:\n"
  forM_ a $ helpA "   "

helpA :: String -> Atom -> IO ()
helpA t Node{..} = do
  _ <- putStrLn $ t <> name
  _ <- forM_ children (helpA (t<>"   "))
  putStr"\n"
helpA t leaf = putStrLn $ t <> printHelp leaf

printHelp :: Atom -> String
printHelp Node{..} = name <> ": " <> help
printHelp Leaf{..} = name <> ": " <> help
