import Data.Map qualified as Map
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

data Node = Node String String deriving (Show)

type NodesMap = Map.Map String Node

-- Parse a single line into a Node
parseNode :: Parser (String, Node)
parseNode = do
  name <- many1 alphaNum
  spaces
  _ <- char '='
  spaces
  values <- between (char '(') (char ')') $ do
    first <- many1 alphaNum
    _ <- char ','
    spaces
    second <- many1 alphaNum
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

walk :: String -> (String -> Bool) -> String -> NodesMap -> Int
walk loc stop (d : ds) nodes
  | stop loc = 0
  | otherwise = case Map.lookup loc nodes of
      Just (Node dl dr) -> 1 + walkdirection d dl dr
      Nothing -> error $ "node not in map: " ++ loc
  where
    walkdirection 'L' dl _ = walk dl stop ds nodes
    walkdirection 'R' _ dr = walk dr stop ds nodes

part1 :: String -> NodesMap -> Int
part1 directions = walk "AAA" ("ZZZ" ==) (cycle directions)

part2 :: String -> NodesMap -> Int
part2 directions nodes = lcm' $ map duration startnodes
  where
    stop = (== 'Z') . last
    startnodes = filter ((== 'A') . last) $ Map.keys nodes
    duration loc = walk loc stop (cycle directions) nodes
    lcm' (x : xs) = foldr lcm x xs
