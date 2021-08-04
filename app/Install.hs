
module Install (
  install
              ) where

import System.Console.StructuredCLI
import System.Directory
import Colors

install' :: Commands ()
install' = colorCommand "install" "goes to the installation menu" $ return NewLevel

install :: Commands () -> Commands ()
install others = install' >+ do
  others
  totalInstallation


totalInstallation :: Commands ()
totalInstallation = colorCommand "whole-installation" "installs manivelle all at once" $ do
  putStrLn ("Are you running velle directly from the directory it is in ? [y/N] " #Warning)
  rep <- getLine
  case rep of
    "y" -> putStr"\n"
    _ -> error ("Please do." #Error)
  appData <- getAppUserDataDirectory "velle"
  _ <- createDirectory appData
  -- to refactor using turtle
  _ <- appendFile (appData<>"/main.cfg") ("user-preferences {\n  shell = \"bash\"\n}\n")
  putStrLn ("Created the .velle global directory !" #OK)
  _ <- copyFile ("./haskvelle"<>exeExtension) (appData<>"/velle"<>exeExtension)
  putStrLn ("Moved the executable to the correct folder !" #OK)
  _ <- createDirectory (appData<>"/repos")
  putStrLn ("Created a folder for future repositories" #OK)
  putStrLn ("What is your Github username ? (leave empty if none)" #Text)
  rep' <- getLine
  _ <- appendFile (appData<>"/main.cfg") ("github {\n  username = " <> show rep' <> "\n}\n")
  putStrLn ("Your username is " #Warning <> show rep' #Name
            <> ", if it is incorrect, you can change it in .the global "# Warning
            <> ".velle/main.cfg" #Name <> " file" #Warning)
  putStrLn ("Please add this file to PATH, you should be all set: "<>appData<>"/")
  putStrLn ("Once done, you'll have access to the"#Success <> " velle "#Name <> "command !"#Success)
  return$ LevelUp 1
