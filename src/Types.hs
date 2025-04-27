module Types where

data PasswordEntry = PasswordEntry {
    title :: String,
    username :: String,
    password :: String
} deriving (Show, Read)