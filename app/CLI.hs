
{-# LANGUAGE RecordWildCards #-}

module CLI (
  run,
  helpT,
  exists,
  Atom(..),
  (>+),
           ) where

import Control.Monad

-- | The main type of the CLI, every command and namespace will be an Atom
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

-- | Concatenates a list of atoms to a parent atom, like children in a tree
(>+) :: Atom -> [Atom] -> Atom
(>+) Node {..} new = Node {children = children ++ new, ..}
(>+) Leaf {..} new = Node {children =             new, ..}


-- | Runs a single atom, parsing the input string
runA :: [String] -> Atom -> IO ()

runA [] _ = return ()
runA (x:rest) Node{..}
  | name == x = forM_ children (runA rest)
runA (x:rest) Leaf{..}
  | name == x = func rest
runA _ _ = return ()


-- | Checks whether the given string will parse in the given tree
existsA :: [String] -> Atom -> IO Bool

existsA [] _ = return False

existsA (x:rest) Node{..}
  | name == x = any (== True) <$> forM children (existsA rest)

existsA (x:_) Leaf{..}
  | name == x = return True

existsA _ _ = return False

-- | Runs the given list of atoms on the string, calling runA
run :: [Atom] -> [String] -> IO ()
run ats args = forM_ ats (runA args)

-- | Checks whether any of the atoms will parse the given strings, calling existsA
exists :: [Atom] -> [String] -> IO Bool
exists ats args = any (== True) <$> forM ats (existsA args)

-- | Prints the help message of a list of Atoms to the screen, calling helpA
helpT :: [Atom] -> IO ()
helpT a = do
  _ <- putStrLn "Velle:\n"
  forM_ a $ helpA "   "

-- | Prints the help message of a single atom, using tab-formatting
helpA :: String -> Atom -> IO ()
helpA t Node{..} = do
  _ <- putStrLn $ "\n" <> t <> name
  forM_ children (helpA (t<>"   "))
helpA t leaf = putStrLn $ t <> printHelp leaf

-- | Utility for obtaining formatted name/help pairs for any given Atom
printHelp :: Atom -> String
printHelp Node{..} = name <> ": " <> help
printHelp Leaf{..} = name <> ": " <> help
