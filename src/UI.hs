-- UI.hs
module UI where

import System.IO
import System.Process

clearScreen :: IO ()
clearScreen = do
    _ <- system "cls || clear"  -- Funciona en Windows y Unix
    return ()

displayMainMenu :: IO ()
displayMainMenu = do
    putStrLn "=================================================="
    putStrLn "=       ADMINISTRADOR SEGURO DE CONTRASEÑAS      ="
    putStrLn "=================================================="
    putStrLn "=                                                ="
    putStrLn "=  1. Registrarse                                ="
    putStrLn "=  2. Iniciar Sesión                             ="
    putStrLn "=  3. Salir                                      ="
    putStrLn "=                                                ="
    putStrLn "=================================================="
    putStrLn "Seleccione una opción: "

displayPasswordMenu :: String -> IO ()
displayPasswordMenu username = do
    clearScreen
    putStrLn $ "Usuario: " ++ username
    putStrLn "=================================================="
    putStrLn "=            GESTIÓN DE CONTRASEÑAS             ="
    putStrLn "=================================================="
    putStrLn "=  1. Ver contraseñas                           ="
    putStrLn "=  2. Agregar contraseña                        ="
    putStrLn "=  3. Modificar contraseña                      ="
    putStrLn "=  4. Eliminar contraseña                       ="
    putStrLn "=  5. Copiar usuario al portapapeles            ="
    putStrLn "=  6. Copiar contraseña al portapapeles         ="
    putStrLn "=  7. Cerrar sesión                             ="
    putStrLn "=================================================="
    putStrLn "Seleccione una opción: "