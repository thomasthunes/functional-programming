module Oblig0 where

import qualified Data.Set as Set
import Data.Set (fromList)
import Data.Set (member)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (lookup)
import Data.List (sortOn)
import Data.List (elemIndex)
import Data.List (elem)
import Data.List (find, nub, maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Int

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

allChars = "\n\r !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

-- Oppgave 1: Substitusjonschiffer
encode :: Key -> String -> String
encode k p = map (substitute k) p
    where
        substitute :: Key -> Char -> Char
        substitute k c = fromMaybe c (lookup c k)

-- Help function for decoding
invert :: Key -> Key
invert key = [(v, k) | (k, v) <- key]

decode :: Key -> String -> String
decode key = encode (invert key)

caesar :: Alphabet -> Integer -> Key
caesar alphabet shift = zip alphabet (rotateAlphabet alphabet shift)
  where
    -- Hjelpefunksjon for å rotere alfabetet
    rotateAlphabet :: Alphabet -> Integer -> Alphabet
    rotateAlphabet alpha n = drop ((fromIntegral n) `mod` len) (cycle alpha)
      where
        len = length alpha


updateValue :: Char -> Double -> [(Char, Double)] -> [(Char, Double)]
updateValue _ _ [] = []  -- Base case: empty list
updateValue key newValue ((k, v):rest)
    | key == k = (k, v + newValue) : updateValue key newValue rest  -- Match the key, update the value
    | otherwise = (k, v) : updateValue key newValue rest  -- Keep other key-value pairs unchanged


count' :: String -> FrequencyTable -> FrequencyTable
count' [] freq = freq
count' (char:rest) freq = 
  if any (\(c, _) -> c == char) freq
    then count' rest (updateValue char 1 freq) --((char, 2.0) : freq)
    else count' rest (freq ++ [(char, 1.0)])--((char, 1.0) : freq)

count :: String -> FrequencyTable
count s = calculateFrequency (count' s []) (fromIntegral (length s))

-- count s = map (\(k, v) -> ((k, v / (fromIntegral (length s))))) (count' s [])

calculateFrequency :: FrequencyTable -> Double -> FrequencyTable
calculateFrequency freq len = map (\(k, v) -> ((k, v / len))) freq

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = do
  str <- (readFile file)
  let freq = count str --in
  return freq

initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess model observation = 
  zip (map (\(k,v) -> k) (sortByDesc model)) (map (\(k,v) -> k) (sortByDesc observation))

sortByDesc :: FrequencyTable -> FrequencyTable
sortByDesc freq = sortOn (\(k, v) -> negate v) freq

freqInModel :: Char -> FrequencyTable -> Double
freqInModel c model = 
  let value = fromMaybe (-1) $ (lookup c model) in
  if value == -1.0
    then 1/10000
    else value

freqInObservation :: Char -> FrequencyTable -> Double
freqInObservation c observation = 
  let value = fromMaybe (-1) $ (lookup c observation) in
  if value == -1.0
    then 0.0
    else value


chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observation = 
  sum [(((freqInObservation c observation) - (freqInModel c model))^2)/ freqInModel c model | c <- allChars] 


-- cooperated with mikkel gunhildsbu
swapEntries :: (Eq a) => (a,b) -> (a,b) -> [(a, b)] -> [(a, b)]
swapEntries (c1, e1) (c2, e2) key = map switch key
  where
    switch (c, v)
      | c == c1 = (c, e2)
      | c == c2 = (c, e1)
      | otherwise = (c, v)


neighbourKeysHelp :: Key -> Key -> [Key]
neighbourKeysHelp _ [] = []
neighbourKeysHelp old_k (k:ey) = [swapEntries k k2 old_k | k2 <- ey] ++ neighbourKeysHelp old_k ey

neighbourKeys :: Key -> [Key]
neighbourKeys key = neighbourKeysHelp key key 


greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey 
  |(chiSquared model (count (decode initKey cipherText))) <= (chiSquared model (count (decode bestNeighKey cipherText))) = initKey
  | otherwise = greedy model cipherText bestNeighKey
    where
      bestNeighKey = snd (minimum [(chiSquared model (count (decode neighKey cipherText)), neighKey) | neighKey <- neighbourKeys initKey])


loadDictionary :: FilePath -> IO Dictionary
loadDictionary file = do
  str <- readFile file
  let dict = fromList (words str)
  return dict

countValidWords :: Dictionary -> String -> Integer
countValidWords dict s = toInteger (length (filter (\word -> Set.member word dict) (words s)))


greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey
  |countValidWords dict (decode initKey cipherText) >= countValidWords dict (decode bestNeighKey cipherText) = initKey
  |otherwise = greedyDict dict cipherText bestNeighKey
    where
      bestNeighKey = snd (maximum [(countValidWords dict (decode neigKey cipherText), neigKey) | neigKey <- neighbourKeys initKey])


