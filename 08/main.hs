import Data.Map qualified as Map
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

data Node = Node String String deriving (Show)

type NodesMap = Map.Map String Node

-- Parse a single line into a Node
parseNode :: Parser (String, Node)
parseNode = do
  name <- many1 letter
  spaces
  _ <- char '='
  spaces
  values <- between (char '(') (char ')') $ do
    first <- many1 letter
    _ <- char ','
    spaces
    second <- many1 letter
    return (Node first second)
  return (name, values)

-- Parse the direction line
parseDirections :: Parser String
parseDirections = many1 letter

-- Parse lines with direction string and nodes
parseInput :: Parser (String, NodesMap)
parseInput = do
  directions <- parseDirections
  _ <- endOfLine
  _ <- endOfLine
  nodes <- endBy parseNode endOfLine
  return (directions, Map.fromList [(name, node) | (name, node) <- nodes])

main :: IO ()
main = do
  input <- getContents
  case parse parseInput "" input of
    Left err -> print err
    Right (directions, nodesMap) -> do
      putStrLn $ "part 1: " ++ show (part1 directions nodesMap)
      putStrLn $ "part 2: " ++ show (part2 directions nodesMap)

part1 :: String -> NodesMap -> Int
part1 directions nodes = walk "AAA" (cycle directions) nodes
  where
    walk :: String -> String -> NodesMap -> Int
    walk "ZZZ" _ _ = 0
    walk loc (d : ds) nodes = case Map.lookup loc nodes of
      Just (Node dl dr) -> 1 + walkdirection d dl dr
      Nothing -> error $ "node not in map: " ++ loc
      where
        walkdirection 'L' dl _ = walk dl ds nodes
        walkdirection 'R' _ dr = walk dr ds nodes

part2 :: String -> NodesMap -> Int
part2 directions nodes = let startnodes = filter (\n -> last n == 'A') $ Map.keys nodes in walkmany (trace (show startnodes) startnodes) (cycle directions) nodes
  where
    walkmany :: [String] -> String -> NodesMap -> Int
    walkmany locations (d : ds) nodes
      | all (\l -> last l == 'Z') (trace (show locations) locations) = 0
      | otherwise = 1 + walkmany locations' ds nodes
      where
        locations' = case mapM (`Map.lookup` nodes) locations of
          Just node -> map step node
          Nothing -> error "couldn't find some nodes"
        step (Node dl dr)
          | d == 'L' = dl
          | d == 'R' = dr
