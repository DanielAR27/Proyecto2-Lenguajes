-- Main.hs
module Main where

import System.IO
import User
import Password
import UI

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering
    clearScreen
    mainLoop

mainLoop :: IO ()
mainLoop = do
    displayMainMenu
    option <- getLine
    case option of
        "1" -> do
            registerUser
            mainLoop
        "2" -> do
            success <- loginUser
            if success
                then passwordMenu
                else mainLoop
        "3" -> putStrLn "¡Gracias por usar el administrador de contraseñas!"
        _   -> do
            putStrLn "Opción inválida. Intente de nuevo."
            mainLoop