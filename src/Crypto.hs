-- Crypto.hs
module Crypto where

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

-- Función simple para cifrar texto usando Base64 y un desplazamiento César
encrypt :: String -> String
encrypt text = T.unpack $ TE.decodeUtf8 $ B64.encode $ BS.pack $ map (fromIntegral . (+ 3) . ord) text

-- Función para descifrar texto cifrado
decrypt :: String -> String
decrypt text = 
    let decodedBytes = B64.decode $ TE.encodeUtf8 $ T.pack text
    in case decodedBytes of
        Left _ -> ""
        Right bytes -> map (chr . (subtract 3) . fromIntegral) $ BS.unpack bytes