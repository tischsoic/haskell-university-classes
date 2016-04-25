import Data.Char

caesarCipher :: Int -> String -> String
caesarCipher shift xs = map (caesarCipherOneChar shift) xs

caesarCipherOneChar :: Int -> Char -> Char
caesarCipherOneChar shift c =
    chr.(+32).( `mod` (126 - 32)).(+shift).(subtract 32).ord $ c
-- caesarCipherOneChar shift c = do
--   let charCode = ord c
--   let charCodeCountFromZero = charCode - 32
--   let charCodeOverflow = mod charCodeCountFromZero 126
--   let shiftedCharCode = charCodeOverflow + 32
--   let shiftedChar = chr shiftedChar
--   return shiftedChar


-- 32 - 126

unCaesarCipher :: Int -> String -> String
unCaesarCipher shift xs = map (caesarCipherOneChar (-shift)) xs


