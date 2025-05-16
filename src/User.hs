-- User.hs
module User where

import System.IO
import System.Directory
import Control.Exception
import Data.List
import Data.Char (toLower)
import FileManager
import Crypto
import UI

data User = User {
    username :: String,
    pin :: String
} deriving (Show, Read)

-- Registra un nuevo usuario
registerUser :: IO ()
registerUser = do
    clearScreen
    putStrLn "=== REGISTRO DE USUARIO ==="
    putStrLn "Ingrese nombre de usuario: "
    username <- getLine
    putStrLn "Ingrese PIN (solo números): "
    pin <- getHiddenInput
    
    if not (all isDigit pin)
        then do
            clearScreen
            putStrLn "El PIN debe contener solo números. Intente de nuevo."
            waitForEnter
            registerUser
        else do
            ensureDataDirExists
            userExists <- doesFileExist $ dataDir ++ username ++ ".txt"
            if userExists
                then do
                    clearScreen
                    putStrLn "El usuario ya existe. Intente con otro nombre."
                    waitForEnter
                    registerUser
                else do
                    let user = User username pin
                    let encryptedPin = encrypt pin
                    writeFile (dataDir ++ username ++ ".txt") encryptedPin
                    clearScreen
                    putStrLn $ "Usuario " ++ username ++ " registrado exitosamente."
                    waitForEnter

-- Inicia sesión con un usuario existente
loginUser :: IO Bool
loginUser = do
    clearScreen
    putStrLn "=== INICIO DE SESIÓN ==="
    putStrLn "Ingrese nombre de usuario: "
    username <- getLine
    putStrLn "Ingrese PIN: "
    pin <- getHiddenInput

    ensureDataDirExists
    let filepath = dataDir ++ username ++ ".txt"
    userExists <- doesFileExist filepath
    if not userExists
        then do
            clearScreen
            putStrLn "Usuario no encontrado. Intente de nuevo."
            waitForEnter
            return False
        else do
            content <- readFile filepath
            case lines content of
                (pinLine:_) -> do
                    let storedPin = decrypt pinLine
                    if pin == storedPin
                        then do
                            clearScreen
                            putStrLn $ "Bienvenido, " ++ username ++ "!"
                            saveCurrentUser username
                            waitForEnter
                            return True
                        else do
                            clearScreen
                            putStrLn "PIN incorrecto. Acceso denegado."
                            waitForEnter
                            return False
                [] -> do
                    clearScreen
                    putStrLn "El archivo del usuario está vacío o dañado."
                    waitForEnter
                    return False

-- Función para confirmar una acción (s/n)
confirmAction :: String -> IO Bool
confirmAction prompt = do
    putStrLn prompt
    response <- getLine
    let resp = if null response then 'n' else toLower (head response)
    return (resp == 's')

-- Oculta la entrada del PIN para mayor seguridad
getHiddenInput :: IO String
getHiddenInput = do
    hSetEcho stdin False
    input <- getLine
    hSetEcho stdin True
    putStrLn ""
    return input

waitForEnter :: IO ()
waitForEnter = do
    putStrLn "Presione Enter para continuar..."
    _ <- getLine
    return ()

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'