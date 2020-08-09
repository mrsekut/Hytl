module CLI
  ( cli
  )
where

import           Options.Applicative
import           Repl                  (astRepl, evalRepl)

import           Compiler
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import qualified Data.Text.Lazy.IO     as LT
import           Lexer.Lexer
import           Parser.Parser
import           System.FilePath.Posix (replaceExtension)


data Command = Repl Bool | Compiler FilePath



{- CLI -}

cli :: IO ()
cli = cliOption =<< execParser parseInfo
 where
  cliOption :: Command -> IO ()
  cliOption cmd = case cmd of
    Repl     True     -> astRepl
    Repl     False    -> evalRepl
    Compiler filepath -> compileFile filepath

  parseInfo :: ParserInfo Command
  parseInfo = parseCommand `withInfo` "Hello Hytl"

  parseCommand :: Parser Command
  parseCommand =
    subparser
      $  command "repl"    (replP `withInfo` "Start repl in the interpreter")
      <> command "compile" (compileP `withInfo` "Compile Hytl code")

  withInfo :: Parser a -> String -> ParserInfo a
  withInfo opts desc = info (helper <*> opts) $ progDesc desc



{- Sub Command -}

replP :: Parser Command
replP = Repl <$> switch
  (long "ast" <> short 'a' <> help "Start the interpreter that outputs AST")

compileP :: Parser Command
compileP =
  Compiler <$> argument str (metavar "[FILENAME]" <> help "Input `.hytl` file")



{- compile -}

compileFile :: FilePath -> IO ()
compileFile filePath = do
  let distPath = replaceExtension filePath ".ll"
  src <- TIO.readFile filePath
  let result = (parse . lexer . T.unpack) src
  LT.writeFile distPath (Compiler.compile result)

