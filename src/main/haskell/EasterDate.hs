module EasterDate where

easterDate :: Int -> String
easterDate year = easter
   where goldenNumber   = year `mod` 19 + 1
         century        = floor (fromIntegral year / 100) + 1
         leapCorrection = (floor (3 / 4 * fromIntegral century)) - 12
         moonCorrection = (floor ((8 * fromIntegral century + 5) / 25)) - 5
         sunday         = (floor (5 / 4 * fromIntegral year)) - leapCorrection - 10
         epact'         = (11 * (goldenNumber + 20 + moonCorrection) - leapCorrection) `mod` 30
         epact          | epact' == 24                      = 25
                        | epact' == 25 && goldenNumber > 11 = 26
                        | otherwise                         = epact'
         fullMoon'      = 44 - epact
         fullMoon       | fullMoon' < 21 = fullMoon' + 30
                        | otherwise      = fullMoon'
         easter'        = fullMoon + 7 - (sunday + fullMoon) `mod` 7
         easter         | easter' > 31 = "April " ++ show (easter' - 31) ++ ", " ++ show year
                        | otherwise    = "March " ++ show easter' ++ ", " ++ show year
