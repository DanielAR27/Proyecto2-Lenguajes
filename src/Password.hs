-- Password.hs
module Password where

import System.IO
import System.Directory
import System.Process
import Data.List
import Data.Char (isDigit)
import Data.List.Split
import Control.Exception
import qualified Control.Exception as E
import FileManager (loadPasswords, savePasswords, getCurrentUser,
                    getFirstLine, formatPasswordLine, parsePasswordLine)
import User (waitForEnter, getHiddenInput, confirmAction)
import Types (PasswordEntry(..))
import Crypto
import UI

-- Menú principal de gestión de contraseñas
passwordMenu :: IO ()
passwordMenu = do
    clearScreen  -- Limpia la pantalla antes de mostrar el menú
    currentUser <- getCurrentUser
    displayPasswordMenu currentUser
    option <- getLine
    case option of
        "1" -> do
            viewPasswords
            passwordMenu
        "2" -> do
            -- Intentar con el método original
            addPassword 
            passwordMenu
        "22" -> do  -- Opción oculta para método alternativo
            clearScreen
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
        "7" -> do
            clearScreen
            confirmed <- confirmAction "¿Está seguro que desea cerrar sesión? (s/n): "
            if confirmed
                then do
                    putStrLn "Cerrando sesión..."
                    waitForEnter
                    return ()
                else passwordMenu
        _   -> do
            clearScreen
            putStrLn "Opción inválida. Intente de nuevo."
            waitForEnter
            passwordMenu

-- Muestra las contraseñas del usuario actual
viewPasswords :: IO ()
viewPasswords = do
    clearScreen
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    
    -- Calcular los anchos máximos para cada columna
    let maxTitleWidth = maximum (map (length . title) passwords ++ [15])  -- Mínimo 15 caracteres
        maxUsernameWidth = maximum (map (length . maskUsername . username) passwords ++ [15])  -- Mínimo 15 caracteres
        idWidth = 4  -- "ID" más un espacio extra
        titleWidth = maxTitleWidth + 2  -- Título más espacios
        usernameWidth = maxUsernameWidth + 2  -- Usuario más espacios
        passwordWidth = 12  -- Fijo para contraseñas
        totalWidth = idWidth + titleWidth + usernameWidth + passwordWidth + 5  -- +5 por los separadores
    
    let headerLine = "| ID | " ++ padRight titleWidth ' ' "TÍTULO" ++ "| " ++ 
                    padRight usernameWidth ' ' "USUARIO" ++ "| " ++ 
                    padRight passwordWidth ' ' "CONTRASEÑA" ++ "|"
        lineLength = length headerLine
        
    putStrLn "=== CONTRASEÑAS GUARDADAS ==="
    putStrLn (replicate lineLength '-')
    putStrLn headerLine
    putStrLn (replicate lineLength '-')
        
    displayPasswordList passwords 1 idWidth titleWidth usernameWidth passwordWidth
    
    putStrLn (replicate lineLength '-')
    waitForEnter

-- Muestra la lista de contraseñas con formato de tabla dinámico
displayPasswordList :: [PasswordEntry] -> Int -> Int -> Int -> Int -> Int -> IO ()
displayPasswordList [] _ _ _ _ _ = return ()
displayPasswordList (entry:entries) id idWidth titleWidth usernameWidth passwordWidth = do
    let maskedUsername = maskUsername (username entry)
        maskedPassword = maskPassword (password entry)
        idStr = padRight (idWidth-1) ' ' (show id)
        titleStr = padRight titleWidth ' ' (title entry)
        userStr = padRight usernameWidth ' ' maskedUsername
        passStr = padRight passwordWidth ' ' maskedPassword
    putStrLn $ "| " ++ idStr ++ "| " ++ titleStr ++ "| " ++ userStr ++ "| " ++ passStr ++ "|"
    displayPasswordList entries (id + 1) idWidth titleWidth usernameWidth passwordWidth

-- Oculta parcialmente el nombre de usuario para mostrar en la tabla
maskUsername :: String -> String
maskUsername username
    | length username <= 3 = username
    | otherwise = 
        let len = length username
            remaining = len - 3
            first = take 2 username
            last' = [last username]
        in first ++ replicate remaining '*' ++ last'

-- Oculta completamente la contraseña para mostrar en la tabla
-- Todas las contraseñas se muestran con 8 asteriscos
maskPassword :: String -> String
maskPassword password = replicate 8 '*'

-- Función auxiliar para rellenar cadenas con espacios hasta una longitud dada
padRight :: Int -> Char -> String -> String
padRight n c s
    | length s >= n = s
    | otherwise = 
        let remaining = n - length s
        in s ++ replicate remaining c

-- Añade una nueva contraseña con confirmación
addPassword :: IO ()
addPassword = do
    clearScreen
    currentUser <- getCurrentUser
    if null currentUser
        then do
            putStrLn "Error: No hay usuario activo. Por favor inicie sesión primero."
            waitForEnter
        else do
            putStrLn "=== AGREGAR NUEVA CONTRASEÑA ==="
            putStrLn "Título (ej. Facebook, Gmail, etc.): "
            title <- getLine
            putStrLn "Nombre de usuario: "
            username <- getLine
            putStrLn "Contraseña: "
            password <- getHiddenInput
            
            clearScreen
            putStrLn "=== RESUMEN DE LA NUEVA CONTRASEÑA ==="
            putStrLn $ "Título: " ++ title
            putStrLn $ "Usuario: " ++ username
            let pwdLength = length password
            putStrLn $ "Contraseña: " ++ replicate pwdLength '*'
            
            confirmed <- confirmAction "¿Está seguro de agregar esta contraseña? (s/n): "
            
            if not confirmed
                then do
                    putStrLn "Operación cancelada."
                    waitForEnter
                else do
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
    clearScreen
    currentUser <- getCurrentUser
    if null currentUser
        then do
            putStrLn "Error: No hay usuario activo. Por favor inicie sesión primero."
            waitForEnter
        else do
            putStrLn "=== AGREGAR NUEVA CONTRASEÑA (MÉTODO DIRECTO) ==="
            putStrLn "Título (ej. Facebook, Gmail, etc.): "
            title <- getLine
            putStrLn "Nombre de usuario: "
            username <- getLine
            putStrLn "Contraseña: "
            password <- getHiddenInput
            
            clearScreen
            putStrLn "=== RESUMEN DE LA NUEVA CONTRASEÑA ==="
            putStrLn $ "Título: " ++ title
            putStrLn $ "Usuario: " ++ username
            putStrLn $ "Contraseña: " ++ replicate (length password) '*'
            
            confirmed <- confirmAction "¿Está seguro de agregar esta contraseña? (s/n): "
            
            if not confirmed
                then do
                    putStrLn "Operación cancelada."
                    waitForEnter
                else do
                    let newEntry = PasswordEntry title username password
                    
                    putStrLn "Guardando contraseña..."
                    success <- directSavePassword currentUser newEntry
                    
                    if success
                        then putStrLn "Contraseña guardada exitosamente."
                        else putStrLn "Error al guardar la contraseña. Verifique que el usuario haya iniciado sesión correctamente."
                    
                    waitForEnter

-- Modifica una contraseña existente con confirmación y validación mejorada
modifyPassword :: IO ()
modifyPassword = do
    clearScreen
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser

    if null passwords
        then do
            putStrLn "No hay contraseñas guardadas para modificar."
            waitForEnter
        else do
            putStrLn "=== MODIFICAR CONTRASEÑA ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña a modificar (0 para cancelar): "
            idStr <- getLine
            
            -- Validación de entrada
            if not (all isDigit idStr) || null idStr
                then do
                    putStrLn "Error: Debe ingresar un número válido."
                    waitForEnter
                else do
                    let id = read idStr :: Int
                    if id == 0
                        then do
                            putStrLn "Operación cancelada."
                            waitForEnter
                        else if id > length passwords
                            then do
                                putStrLn $ "Error: ID no encontrado. Solo hay " ++ show (length passwords) ++ " contraseñas."
                                waitForEnter
                            else do
                                clearScreen
                                let entry = passwords !! (id - 1)
                                putStrLn $ "=== MODIFICAR CONTRASEÑA: " ++ title entry ++ " ==="
                                putStrLn $ "Título actual: " ++ title entry
                                putStrLn "Nuevo título (dejar en blanco para mantener el actual): "
                                newTitle <- getLine
                                putStrLn $ "Usuario actual: " ++ username entry
                                putStrLn "Nuevo usuario (dejar en blanco para mantener el actual): "
                                newUsername <- getLine
                                let pwdLength = length (password entry)
                                putStrLn $ "Contraseña actual: " ++ replicate pwdLength '*'
                                putStrLn "Nueva contraseña (dejar en blanco para mantener la actual): "
                                newPassword <- getHiddenInput

                                let updatedEntry = PasswordEntry
                                        (if null newTitle then title entry else newTitle)
                                        (if null newUsername then username entry else newUsername)
                                        (if null newPassword then password entry else newPassword)
                                
                                clearScreen
                                putStrLn "=== RESUMEN DE CAMBIOS ==="
                                putStrLn $ "Título: " ++ title entry ++ " -> " ++ title updatedEntry
                                putStrLn $ "Usuario: " ++ username entry ++ " -> " ++ username updatedEntry
                                let pwdEntryLength = length (password entry)
                                let pwdUpdatedLength = length (password updatedEntry)
                                putStrLn $ "Contraseña: " ++ replicate pwdEntryLength '*' ++ " -> " ++ 
                                           replicate pwdUpdatedLength '*'
                                
                                confirmed <- confirmAction "¿Está seguro de modificar esta contraseña? (s/n): "
                                
                                if not confirmed
                                    then do
                                        putStrLn "Operación cancelada."
                                        waitForEnter
                                    else do
                                        let updatedPasswords = take (id - 1) passwords ++ [updatedEntry] ++ drop id passwords

                                        result <- E.try (savePasswords currentUser updatedPasswords) :: IO (Either E.IOException ())
                                        case result of
                                            Left e -> putStrLn $ "Error al modificar la contraseña: " ++ show e
                                            Right _ -> putStrLn "Contraseña modificada exitosamente."
                                        waitForEnter

-- Elimina una contraseña con confirmación y validación mejorada
deletePassword :: IO ()
deletePassword = do
    clearScreen
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser

    if null passwords
        then do
            putStrLn "No hay contraseñas guardadas para eliminar."
            waitForEnter
        else do
            putStrLn "=== ELIMINAR CONTRASEÑA ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña a eliminar (0 para cancelar): "
            idStr <- getLine
            
            -- Validación de entrada
            if not (all isDigit idStr) || null idStr
                then do
                    putStrLn "Error: Debe ingresar un número válido."
                    waitForEnter
                else do
                    let id = read idStr :: Int
                    if id == 0
                        then do
                            putStrLn "Operación cancelada."
                            waitForEnter
                        else if id > length passwords
                            then do
                                putStrLn $ "Error: ID no encontrado. Solo hay " ++ show (length passwords) ++ " contraseñas."
                                waitForEnter
                            else do
                                clearScreen
                                let entry = passwords !! (id - 1)
                                putStrLn "=== CONFIRMAR ELIMINACIÓN ==="
                                putStrLn $ "Título: " ++ title entry
                                putStrLn $ "Usuario: " ++ username entry
                                
                                confirmed <- confirmAction "¿Está seguro de eliminar esta contraseña? (s/n): "
                                
                                if not confirmed
                                    then do
                                        putStrLn "Operación cancelada."
                                        waitForEnter
                                    else do
                                        let updatedPasswords = take (id - 1) passwords ++ drop id passwords
                                        result <- E.try (savePasswords currentUser updatedPasswords) :: IO (Either E.IOException ())
                                        case result of
                                            Left e -> putStrLn $ "Error al eliminar la contraseña: " ++ show e
                                            Right _ -> putStrLn "Contraseña eliminada exitosamente."

-- Copia el nombre de usuario al portapapeles con validación mejorada
copyUsernameToClipboard :: IO ()
copyUsernameToClipboard = do
    clearScreen
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    
    if null passwords
        then do
            putStrLn "No hay contraseñas guardadas para copiar usuario."
            waitForEnter
        else do
            putStrLn "=== COPIAR USUARIO AL PORTAPAPELES ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña para copiar el usuario (0 para cancelar): "
            idStr <- getLine
            
            -- Validación de entrada
            if not (all isDigit idStr) || null idStr
                then do
                    putStrLn "Error: Debe ingresar un número válido."
                    waitForEnter
                else do
                    let id = read idStr :: Int
                    if id == 0
                        then do
                            putStrLn "Operación cancelada."
                            waitForEnter
                        else if id > length passwords
                            then do
                                putStrLn $ "Error: ID no encontrado. Solo hay " ++ show (length passwords) ++ " contraseñas."
                                waitForEnter
                            else do
                                let entry = passwords !! (id - 1)
                                _ <- system $ "echo " ++ username entry ++ " | clip"  -- Windows
                                -- Para Linux: _ <- system $ "echo " ++ username entry ++ " | xclip -selection clipboard"
                                putStrLn "Usuario copiado al portapapeles."
                                waitForEnter

-- Copia la contraseña al portapapeles con validación mejorada
copyPasswordToClipboard :: IO ()
copyPasswordToClipboard = do
    clearScreen
    currentUser <- getCurrentUser
    passwords <- loadPasswords currentUser
    
    if null passwords
        then do
            putStrLn "No hay contraseñas guardadas para copiar contraseña."
            waitForEnter
        else do
            putStrLn "=== COPIAR CONTRASEÑA AL PORTAPAPELES ==="
            viewPasswords
            putStrLn "Ingrese el ID de la contraseña para copiar la contraseña (0 para cancelar): "
            idStr <- getLine
            
            -- Validación de entrada
            if not (all isDigit idStr) || null idStr
                then do
                    putStrLn "Error: Debe ingresar un número válido."
                    waitForEnter
                else do
                    let id = read idStr :: Int
                    if id == 0
                        then do
                            putStrLn "Operación cancelada."
                            waitForEnter
                        else if id > length passwords
                            then do
                                putStrLn $ "Error: ID no encontrado. Solo hay " ++ show (length passwords) ++ " contraseñas."
                                waitForEnter
                            else do
                                let entry = passwords !! (id - 1)
                                _ <- system $ "echo " ++ password entry ++ " | clip"  -- Windows
                                -- Para Linux: _ <- system $ "echo " ++ password entry ++ " | xclip -selection clipboard"
                                putStrLn "Contraseña copiada al portapapeles."
                                waitForEnter