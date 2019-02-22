import Control.Monad(ap, replicateM, filterM, liftM)
import Data.IORef
import Control.Applicative
import Options.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BCh
 
data Command = Command
  { l    :: Bool,
    c    :: Bool,
    m    :: Bool,
    w    :: Bool,
    f    :: Input}
   
data Input
    = FileInput FilePath
    | StdInput
 
readCommand :: Parser Command
readCommand = Command
    <$> switch
        ( long "lines"
       <> short 'l'
       <> help "--вывести количество строк" )
    <*> switch
        ( long "bytes"
       <> short 'c'
       <> help "--вывести количество байт" )
    <*> switch
        ( long "chars"
       <> short 'm'
       <> help "--вывести количество символов" )
    <*> switch
        ( long "words"
       <> short 'w'
       <> help "--вывести количество слов" )
    <*> ( (FileInput <$> argument str ( metavar "FILE" <> help "Input file" ))
       <|> (StdInput <$ pure ()))
 
 
runCommand :: Command -> IO ()
runCommand (cm@(Command l c m w (FileInput f))) = do
    str <- BCh.readFile f
    writeAns cm str
runCommand (cm@(Command l c m w StdInput)) = do
    str <- BCh.getContents
    writeAns cm str
 
writeIf :: Bool -> Int -> IO ()
writeIf p x = if p then print x else return ()
   
writeAns :: Command -> BCh.ByteString -> IO ()  
writeAns (Command l c m w _) str = do
    if (not l && not c && not m && not w) then do
        writeIf True (wcl str)
        writeIf True (wcw str)
        writeIf True (wcm str)
        writeIf True (wcc str)
    else do
        writeIf l (wcl str)
        writeIf w (wcw str)
        writeIf m (wcm str)
        writeIf c (wcc str)
 
wcc ::  BCh.ByteString -> Int
wcc s = BCh.length s

wcm ::  BCh.ByteString  -> Int
wcm s = BCh.length s
 
wcl ::  BCh.ByteString -> Int
wcl s = length $ BCh.lines s
 
wcw ::  BCh.ByteString -> Int
wcw s = length $ BCh.words s
 
main :: IO ()
main = runCommand =<< execParser opts
  where
    opts = info (readCommand <**> helper)
      ( fullDesc
     <> header "[OPTIONS] <filename>` где `[OPTIONS]` могут быть:" )
