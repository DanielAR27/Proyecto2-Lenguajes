-- Main.hs
module Main where

import System.IO
import User (registerUser, loginUser, waitForEnter, confirmAction)
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
                then do
                    passwordMenu
                    mainLoop  -- Volver al menú principal después de cerrar sesión
                else mainLoop
        "3" -> do
            clearScreen
            confirmed <- confirmAction "¿Está seguro que desea salir del programa? (s/n): "
            if confirmed
                then do
                    clearScreen
                    putStrLn "¡Gracias por usar el administrador de contraseñas!"
                    putStrLn "Hasta luego!"
                    waitForEnter
                else mainLoop
        _   -> do
            clearScreen
            putStrLn "Opción inválida. Intente de nuevo."
            waitForEnter
            mainLoop