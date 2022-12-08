import Control.Monad
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

sizeLimit = 100000

maxCapacity = 70000000 - 30000000

type Name = String

type Size = Int

data Inode = File Name Size | Folder Name [Inode] deriving (Show)

data InodeContext = InodeContext Name [Inode] deriving (Show)

type Context = (Inode, [InodeContext])

root :: Inode
root = Folder "/" []

-- Helpers

inodeName :: Inode -> String
inodeName (File name _) = name
inodeName (Folder name _) = name

goUp :: Context -> Maybe Context
goUp (inode, InodeContext name children : rs) = Just (Folder name (inode : children), rs)
goUp (inode, []) = Nothing

goDown :: Name -> Context -> Maybe Context
goDown name (Folder folderName children, ctx) =
  let (ls, inode : rs) = break ((== name) . inodeName) children
   in Just (inode, InodeContext folderName (ls ++ rs) : ctx)
goDown name (file, _) = Nothing

goToRoot :: Context -> Maybe Context
goToRoot (inode, []) = Just (inode, [])
goToRoot ctx = goUp ctx >>= goToRoot

goTo :: Name -> Context -> Maybe Context
goTo ".." = goUp
goTo "/" = goToRoot
goTo name = goDown name

changeNode :: (Inode -> Maybe Inode) -> Context -> Maybe Context
changeNode f (node, ctx) = case f node of
  Just newNode -> Just (newNode, ctx)
  Nothing -> Nothing

addChild :: Inode -> Inode -> Maybe Inode
addChild child (Folder name children) = Just $ Folder name (child : children)
addChild child (File _ _) = Nothing

addFolder :: String -> Inode -> Maybe Inode
addFolder name = addChild (Folder name [])

addFile :: Int -> String -> Inode -> Maybe Inode
addFile size name = addChild (File name size)

allSizes :: Inode -> [(Inode, Int)]
allSizes file@(File _ size) = [(file, size)]
allSizes folder@(Folder _ children) =
  let childrenSize = concatMap allSizes children
   in (folder, sum . map snd $ filter (isFile . fst) childrenSize) : childrenSize

allFolderSizes :: Inode -> [(Inode, Int)]
allFolderSizes = filter (isFolder . fst) . allSizes

isFolder :: Inode -> Bool
isFolder (File _ _) = False
isFolder (Folder _ _) = True

isFile :: Inode -> Bool
isFile = not . isFolder

getNode :: Context -> Inode
getNode (inode, _) = inode

createFileSystem :: [Context -> Maybe Context] -> Maybe Inode
createFileSystem = fmap getNode . goToRoot <=< foldl (>>=) (Just (root, []))

-- Parser

filename :: Parser String
filename = many1 (letter <|> char '.' <|> char '/')

cd :: Parser (Context -> Maybe Context)
cd = goTo <$> (string "cd " *> filename) <* newline

ls :: Parser [Context -> Maybe Context]
ls = string "ls" *> newline *> many (showDir <|> showFile)

showDir :: Parser (Context -> Maybe Context)
showDir = changeNode . addFolder <$> (string "dir " *> filename) <* newline

showFile :: Parser (Context -> Maybe Context)
showFile = changeNode <$> (addFile <$> (read <$> many1 digit) <*> (string " " *> filename)) <* newline

prompt :: Parser [Context -> Maybe Context]
prompt = string "$ " *> (ls <|> ((: []) <$> cd))

commands :: Parser [Context -> Maybe Context]
commands = concat <$> many prompt

parseInput :: Parser (Maybe Inode)
parseInput = createFileSystem <$> commands <* eof

part1 :: Parser Int
part1 = maybe 0 sumFolderWithSizeLimit <$> parseInput
  where
    sumFolderWithSizeLimit = sum . filter (< sizeLimit) . map snd . allFolderSizes

part2 :: Parser Int
part2 = maybe 0 sizeFolderToDelete <$> parseInput

sizeFolderToDelete :: Inode -> Int
sizeFolderToDelete root =
  let folderSizes = map snd $ allFolderSizes root
      sizeNeeded = head folderSizes - maxCapacity
   in minimum $ filter (> sizeNeeded) folderSizes

main :: IO ()
main = parseAndSolve "inputs/day7" part1 part2
