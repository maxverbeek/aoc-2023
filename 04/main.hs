import Text.Parsec
import Text.Parsec.String (Parser)
import Debug.Trace

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
part2 cards = sum $ map p2aux cards
  where
    p2aux :: Card -> Int
    p2aux (Card id winning owned) = 1 + (sum $ map p2aux newcards)
      where
      newcards = take intersections (drop id cards)
      intersections = intersectCount winning owned


main :: IO ()
main = do
  input <- getContents
  case parse cardsParser "" input of
    Left err -> print err
    Right cards -> do
      print $ part1 cards
      print $ part2 cards
