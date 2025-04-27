-- FileManager.hs
module FileManager where

import System.IO
import System.Directory
import Control.Monad (when)
import Control.Exception
import Data.List
import Data.List.Split
import Crypto
import Types (PasswordEntry(..))
import qualified Control.Exception as E

-- Directorio base para almacenar los datos
dataDir :: String
dataDir = "src/data/"

-- Asegura que el directorio de datos exista
ensureDataDirExists :: IO ()
ensureDataDirExists = createDirectoryIfMissing True dataDir

-- Guarda el usuario actual en un archivo temporal
saveCurrentUser :: String -> IO ()
saveCurrentUser username = do
    ensureDataDirExists
    withFile (dataDir ++ "current_user.tmp") WriteMode $ \handle -> 
        hPutStr handle username

-- Obtiene el usuario actual desde el archivo temporal
getCurrentUser :: IO String
getCurrentUser = do
    ensureDataDirExists
    exists <- doesFileExist (dataDir ++ "current_user.tmp")
    if exists
        then do
            E.catch 
                (do
                    withFile (dataDir ++ "current_user.tmp") ReadMode $ \handle -> do
                        contents <- hGetContents handle
                        length contents `seq` return (head (lines contents ++ [""])))
                (\e -> do
                    let _ = e :: E.IOException
                    return "")
        else return ""

-- Carga las contraseñas del usuario actual
loadPasswords :: String -> IO [PasswordEntry]
loadPasswords username = do
    ensureDataDirExists
    let filepath = dataDir ++ username ++ ".txt"
    exists <- doesFileExist filepath
    if not exists
        then return []
        else E.catch
            (do
                contents <- readFileStrict filepath  -- Usando readFile' que fuerza la lectura completa
                let ls = lines contents
                if length ls <= 1
                    then return []  -- Solo hay PIN, no hay contraseñas
                    else do
                        let passwordLines = drop 1 ls  -- Ignorar la primera línea (PIN)
                        return $ map parsePasswordLine passwordLines)
            (\e -> do
                let _ = e :: E.IOException
                putStrLn $ "Error al cargar contraseñas: " ++ show e
                return [])

-- Lee un archivo completo de forma estricta
readFileStrict :: FilePath -> IO String
readFileStrict path = do
    withFile path ReadMode $ \h -> do
        contents <- hGetContents h
        length contents `seq` return contents

-- Parsea una línea de contraseña desde el archivo
parsePasswordLine :: String -> PasswordEntry
parsePasswordLine line =
    let decryptedLine = decrypt line
        parts = splitOn "&&" decryptedLine
    in if length parts >= 3
        then PasswordEntry (parts !! 0) (parts !! 1) (parts !! 2)
        else if length parts == 2
            then PasswordEntry (parts !! 0) (parts !! 1) ""
            else PasswordEntry "" "" ""  -- Caso de error

-- Guarda las contraseñas del usuario con enfoque de archivo temporal
savePasswords :: String -> [PasswordEntry] -> IO ()
savePasswords username passwords = do
    ensureDataDirExists
    let filepath = dataDir ++ username ++ ".txt"
    let tempPath = dataDir ++ username ++ ".tmp"
    
    exists <- doesFileExist filepath
    if not exists
        then error $ "El archivo del usuario " ++ username ++ " no existe"
        else do
            -- Leemos el PIN del archivo original
            pinLine <- E.catch 
                (withFile filepath ReadMode $ \h -> hGetLine h)
                (\e -> do
                    let _ = e :: E.IOException
                    error $ "Error al leer el PIN: " ++ show e)
            
            -- Creamos un archivo temporal con el contenido actualizado
            E.catch
                (do
                    withFile tempPath WriteMode $ \h -> do
                        hPutStrLn h pinLine
                        let passwordLines = map formatPasswordLine passwords
                        let encryptedLines = map encrypt passwordLines
                        mapM_ (hPutStrLn h) encryptedLines
                        
                    -- Esperamos que el archivo se cierre completamente antes de continuar
                    hFlush stdout
                    
                    -- Reemplazamos el archivo original con el temporal
                    removeFile filepath
                    renameFile tempPath filepath)
                (\e -> do
                    let _ = e :: E.IOException
                    -- Si hay un error, limpiamos el archivo temporal
                    doesFileExist tempPath >>= \ex -> when ex (removeFile tempPath)
                    error $ "Error al guardar contraseñas: " ++ show e)

-- Formatea una entrada de contraseña para guardar en el archivo
formatPasswordLine :: PasswordEntry -> String
formatPasswordLine entry =
    title entry ++ "&&" ++ username entry ++ "&&" ++ password entry

-- Obtiene la primera línea de un archivo (el PIN cifrado)
getFirstLine :: FilePath -> IO String
getFirstLine filepath = do
    exists <- doesFileExist filepath
    if not exists
        then error $ "El archivo " ++ filepath ++ " no existe"
        else E.catch
            (withFile filepath ReadMode $ \h -> do
                eof <- hIsEOF h
                if eof
                    then error $ "El archivo " ++ filepath ++ " está vacío"
                    else hGetLine h)
            (\e -> do
                let _ = e :: E.IOException
                error $ "Error al leer la primera línea: " ++ show e)