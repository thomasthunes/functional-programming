module Oblig0 where

import qualified Data.Set as Set

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String


encode :: Key -> String -> String
encode k = undefined

decode :: Key -> String -> String
decode = undefined

count :: String -> FrequencyTable
count s = undefined

caesar :: Alphabet -> Integer -> Key
caesar alphabet shift = undefined

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = undefined

initialGuess :: FrequencyTable -> FrequencyTable-> Key
initialGuess model observation = undefined

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observation = undefined

neighbourKeys :: Key -> [Key]
neighbourKeys key = undefined

swapEntries :: (Char,Char) -> (Char, Char) -> Key -> Key
swapEntries (c1, e1) (c2, e2) key = undefined


greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = undefined

loadDictionary :: FilePath -> IO Dictionary
loadDictionary fil = undefined

countValidWords :: Dictionary -> String -> Integer
countValidWords dict = undefined

greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey = undefined

