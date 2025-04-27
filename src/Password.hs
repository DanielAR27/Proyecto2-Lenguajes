-- Password.hs
module Password where

import System.IO
import System.Directory
import System.Process
import Data.List
import Data.List.Split
import Control.Exception
import qualified Control.Exception as E
import FileManager (loadPasswords, savePasswords, getCurrentUser,
                    getFirstLine, formatPasswordLine, parsePasswordLine)
import User (waitForEnter, getHiddenInput)
import Types (PasswordEntry(..))
import Crypto
import UI

-- Menú principal de gestión de contraseñas
-- Menú principal de gestión de contraseñas modificado
passwordMenu :: IO ()
passwordMenu = do
    currentUser <- getCurrentUser
    displayPasswordMenu currentUser
    option <- getLine
    case option of
        "1" -> do
            viewPasswords
            passwordMenu
        "2" -> do
            -- Intentar con el método original
            putStrLn "Intentando agregar contraseña con método estándar..."
            addPassword 
            passwordMenu
        "22" -> do  -- Opción oculta para método alternativo
            putStrLn "Usando método alternativo para agregar contraseña..."
            addPasswordDirect
            passwordMenu
        "3" -> do
            modifyPassword
            passwordMenu
        "4" -> do
            deletePassword
            passwordMenu
        "5" -> do
            copyUsernameToClipboard
            passwordMenu
        "6" -> do
            copyPasswordToClipboard
            passwordMenu
        "7" -> return ()
        _   -> do
            putStrLn "Opción inválida. Intente de nuevo."
            passwordMenu

-- Muestra las contraseñas del usuario actual
viewPasswords :: IO ()
viewPasswords = do
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    clearScreen
    putStrLn "=== CONTRASEÑAS GUARDADAS ==="
    putStrLn "------------------------------------------------"
    putStrLn "| ID |       TÍTULO       |      USUARIO       |"
    putStrLn "------------------------------------------------"
    displayPasswordList passwords 1
    waitForEnter

-- Muestra la lista de contraseñas con formato de tabla
displayPasswordList :: [PasswordEntry] -> Int -> IO ()
displayPasswordList [] _ = putStrLn "------------------------------------------------"
displayPasswordList (entry:entries) id = do
    let maskedUsername = maskUsername (username entry)
        idStr = padRight 3 ' ' (show id)
        titleStr = padRight 18 ' ' (title entry)
        userStr = padRight 18 ' ' maskedUsername
    putStrLn $ "| " ++ idStr ++ "| " ++ titleStr ++ "| " ++ userStr ++ "|"
    displayPasswordList entries (id + 1)

-- Oculta parcialmente el nombre de usuario para mostrar en la tabla
maskUsername :: String -> String
maskUsername username
    | length username <= 3 = username
    | otherwise = take 2 username ++ replicate (length username - 3) '*' ++ [last username]

-- Función auxiliar para rellenar cadenas con espacios
padRight :: Int -> Char -> String -> String
padRight n c s
    | length s >= n = s
    | otherwise = s ++ replicate (n - length s) c

-- Añade una nueva contraseña
-- Función mejorada para añadir una nueva contraseña
addPassword :: IO ()
addPassword = do
    currentUser <- getCurrentUser
    if null currentUser
        then do
            clearScreen
            putStrLn "Error: No hay usuario activo. Por favor inicie sesión primero."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== AGREGAR NUEVA CONTRASEÑA ==="
            putStrLn "Título (ej. Facebook, Gmail, etc.): "
            title <- getLine
            putStrLn "Nombre de usuario: "
            username <- getLine
            putStrLn "Contraseña: "
            password <- getHiddenInput

            passwords <- loadPasswords currentUser
            let newEntry = PasswordEntry title username password
            let updatedPasswords = passwords ++ [newEntry]

            -- Usamos un manejador de excepciones más específico
            result <- E.try (do
                -- Intentamos guardar con un pequeño retraso
                putStrLn "Guardando contraseña..."
                savePasswords currentUser updatedPasswords
                return True) :: IO (Either E.IOException Bool)
                
            case result of
                Left e -> do
                    putStrLn $ "Error al guardar la contraseña: " ++ show e
                    putStrLn "Intentando método alternativo..."
                    
                    -- Método alternativo usando directamente writeFile
                    altResult <- E.try (do
                        let filepath = "src/data/" ++ currentUser ++ ".txt"
                        pinLine <- getFirstLine filepath
                        let passwordLines = map formatPasswordLine updatedPasswords
                        let encryptedLines = map encrypt passwordLines
                        let content = pinLine ++ "\n" ++ intercalate "\n" encryptedLines
                        writeFile filepath content
                        return True) :: IO (Either E.IOException Bool)
                        
                    case altResult of
                        Left e2 -> do
                            putStrLn $ "Error con método alternativo: " ++ show e2
                            putStrLn "Verifique que el usuario haya iniciado sesión correctamente."
                        Right _ -> putStrLn "Contraseña guardada exitosamente con método alternativo."
                    
                Right _ -> putStrLn "Contraseña guardada exitosamente."
            waitForEnter

-- Función adicional para guardar contraseñas directamente sin usar FileManager
directSavePassword :: String -> PasswordEntry -> IO Bool
directSavePassword username newEntry = do
    let filepath = "src/data/" ++ username ++ ".txt"
    let tempPath = "src/data/" ++ username ++ ".new"
    
    fileExists <- doesFileExist filepath
    if not fileExists
        then do
            putStrLn $ "Error: El archivo del usuario " ++ username ++ " no existe."
            return False
        else do
            -- Leer el archivo actual y todas las contraseñas
            contents <- readFile filepath
            let allLines = lines contents
            
            if null allLines
                then do
                    putStrLn "Error: El archivo del usuario está vacío."
                    return False
                else do
                    let pinLine = head allLines
                    let passwordLines = drop 1 allLines
                    let currentPasswords = map parsePasswordLine passwordLines
                    let updatedPasswords = currentPasswords ++ [newEntry]
                    
                    -- Formatear las contraseñas actualizadas
                    let newPasswordLines = map (encrypt . formatPasswordLine) updatedPasswords
                    
                    -- Escribir en un archivo temporal
                    handle <- openFile tempPath WriteMode
                    hPutStrLn handle pinLine
                    mapM_ (hPutStrLn handle) newPasswordLines
                    hClose handle
                    
                    -- Reemplazar el archivo original con el temporal
                    removeFile filepath
                    renameFile tempPath filepath
                    return True

-- Versión alternativa para agregar contraseña que usa el método directo
addPasswordDirect :: IO ()
addPasswordDirect = do
    currentUser <- getCurrentUser
    if null currentUser
        then do
            clearScreen
            putStrLn "Error: No hay usuario activo. Por favor inicie sesión primero."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== AGREGAR NUEVA CONTRASEÑA (MÉTODO DIRECTO) ==="
            putStrLn "Título (ej. Facebook, Gmail, etc.): "
            title <- getLine
            putStrLn "Nombre de usuario: "
            username <- getLine
            putStrLn "Contraseña: "
            password <- getHiddenInput

            let newEntry = PasswordEntry title username password
            
            putStrLn "Guardando contraseña..."
            success <- directSavePassword currentUser newEntry
            
            if success
                then putStrLn "Contraseña guardada exitosamente."
                else putStrLn "Error al guardar la contraseña. Verifique que el usuario haya iniciado sesión correctamente."
            
            waitForEnter

-- Modifica una contraseña existente
modifyPassword :: IO ()
modifyPassword = do
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser

    if null passwords
        then do
            clearScreen
            putStrLn "No hay contraseñas guardadas para modificar."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== MODIFICAR CONTRASEÑA ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña a modificar (0 para cancelar): "
            idStr <- getLine
            let id = read idStr :: Int
            if id == 0 || id > length passwords
                then putStrLn "Operación cancelada o ID inválido."
                else do
                    let entry = passwords !! (id - 1)
                    putStrLn $ "Modificando: " ++ title entry
                    putStrLn "Nuevo título (dejar en blanco para mantener el actual): "
                    newTitle <- getLine
                    putStrLn "Nuevo usuario (dejar en blanco para mantener el actual): "
                    newUsername <- getLine
                    putStrLn "Nueva contraseña (dejar en blanco para mantener la actual): "
                    newPassword <- getHiddenInput

                    let updatedEntry = PasswordEntry
                            (if null newTitle then title entry else newTitle)
                            (if null newUsername then username entry else newUsername)
                            (if null newPassword then password entry else newPassword)

                    let updatedPasswords = take (id - 1) passwords ++ [updatedEntry] ++ drop id passwords

                    result <- E.try (savePasswords currentUser updatedPasswords) :: IO (Either E.IOException ())
                    case result of
                        Left e -> putStrLn $ "Error al modificar la contraseña: " ++ show e
                        Right _ -> putStrLn "Contraseña modificada exitosamente."
            waitForEnter

-- Elimina una contraseña
deletePassword :: IO ()
deletePassword = do
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser

    if null passwords
        then do
            clearScreen
            putStrLn "No hay contraseñas guardadas para eliminar."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== ELIMINAR CONTRASEÑA ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña a eliminar (0 para cancelar): "
            idStr <- getLine
            let id = read idStr :: Int
            if id == 0 || id > length passwords
                then putStrLn "Operación cancelada o ID inválido."
                else do
                    let updatedPasswords = take (id - 1) passwords ++ drop id passwords
                    result <- E.try (savePasswords currentUser updatedPasswords) :: IO (Either E.IOException ())
                    case result of
                        Left e -> putStrLn $ "Error al eliminar la contraseña: " ++ show e
                        Right _ -> putStrLn "Contraseña eliminada exitosamente."
            waitForEnter

-- Copia el nombre de usuario al portapapeles
copyUsernameToClipboard :: IO ()
copyUsernameToClipboard = do
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    
    if null passwords
        then do
            clearScreen
            putStrLn "No hay contraseñas guardadas para copiar usuario."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== COPIAR USUARIO AL PORTAPAPELES ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña para copiar el usuario (0 para cancelar): "
            idStr <- getLine
            let id = read idStr :: Int
            if id == 0 || id > length passwords
                then putStrLn "Operación cancelada o ID inválido."
                else do
                    let entry = passwords !! (id - 1)
                    _ <- system $ "echo " ++ username entry ++ " | clip"  -- Windows
                    -- Para Linux: _ <- system $ "echo " ++ username entry ++ " | xclip -selection clipboard"
                    putStrLn "Usuario copiado al portapapeles."
            waitForEnter

-- Copia la contraseña al portapapeles
copyPasswordToClipboard :: IO ()
copyPasswordToClipboard = do
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    
    if null passwords
        then do
            clearScreen
            putStrLn "No hay contraseñas guardadas para copiar contraseña."
            waitForEnter
        else do
            clearScreen
            putStrLn "=== COPIAR CONTRASEÑA AL PORTAPAPELES ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña para copiar la contraseña (0 para cancelar): "
            idStr <- getLine
            let id = read idStr :: Int
            if id == 0 || id > length passwords
                then putStrLn "Operación cancelada o ID inválido."
                else do
                    let entry = passwords !! (id - 1)
                    _ <- system $ "echo " ++ password entry ++ " | clip"  -- Windows
                    -- Para Linux: _ <- system $ "echo " ++ password entry ++ " | xclip -selection clipboard"
                    putStrLn "Contraseña copiada al portapapeles."
            waitForEnter