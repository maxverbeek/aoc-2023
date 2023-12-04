import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Array (Array, (!))
import qualified Data.Array as Array

data Card = Card Int [Int] [Int] deriving Show

-- Parse a single card
cardParser :: Parser Card
cardParser = do
  string "Card"
  spaces
  cardNumber <- read <$> many1 digit
  char ':'
  spaces
  firstList <- many1 (read <$> many1 digit <* spaces)
  char '|'
  spaces
  secondList <- many1 (read <$> many1 digit <* spaces)
  return (Card cardNumber firstList secondList)

-- Parse a list of cards
cardsParser :: Parser [Card]
cardsParser = many cardParser

intersectCount :: [Int] -> [Int] -> Int
intersectCount list1 list2 = length (filter (`elem` list2) list1)

cardPoints :: Card -> Int
cardPoints (Card _ winning owned) = points $ intersectCount winning owned
  where
  points intersections
    | intersections <= 0 = 0
    | otherwise = 2 ^ (intersections - 1)


cardIdToCardsWon :: Card -> [Int]
cardIdToCardsWon (Card id winning owned) = take intersections [(id+1)..]
  where intersections = intersectCount winning owned

part1 :: [Card] -> Int
part1 cards = sum $ map cardPoints cards

part2 :: [Card] -> Int
part2 cards = sum $ Array.elems memo
  where
    -- build the memo array starting at the end (hence the double reverse)
    -- so that we are sure that all indeces beyond ID are filled.
    memo :: Array Int Int
    memo = Array.listArray (1, length cards) $ reverse $ map count $ reverse cards

    -- Because we iterate in reverse, we have already added all counts of
    -- future cards, so we can look them up instantly from the array.
    count :: Card -> Int
    count (Card id winning owned) = 1 + sum [ memo ! n | n <- take i [(id+1)..] ]
      where i = intersectCount winning owned

main :: IO ()
main = do
  input <- getContents
  case parse cardsParser "" input of
    Left err -> print err
    Right cards -> do
      putStrLn $ "part 1: " ++ show (part1 cards)
      putStrLn $ "part 2: " ++ show (part2 cards)
