module CLI
  ( cli
  )
where

import           Repl                           ( astRepl
                                                , evalRepl
                                                )
import           Options.Applicative

import           System.Environment             ( getArgs )
import           System.FilePath.Posix          ( replaceExtension )
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy.IO             as LT
import qualified Data.Text                     as T
import           Compiler
import           Data.Text.Internal
import           Lexer.Lexer
import           Parser.Parser


data CLI = CLI { ast :: Bool
               , compile :: FilePath
               }


cli :: IO ()
cli = cliOption =<< execParser opts
  where opts = info (config <**> helper) (fullDesc <> progDesc "Hytl REPL")

config :: Parser CLI
config =
  CLI
    <$> switch (long "ast" <> short 'a' <> help "Target for the greeting")
    <*> strOption
          (long "compile" <> short 'c' <> metavar "FILENAME" <> help
            "Input file"
          )


cliOption :: CLI -> IO ()
-- cliOption (CLI True _) = astRepl
cliOption (CLI False s) = compileFile s
-- cliOption (CLI False _) = evalRepl


compileFile :: FilePath -> IO ()
compileFile filePath = do
  let distPath = replaceExtension filePath ".ll"
  src <- TIO.readFile filePath
  let result = (parse . lexer . T.unpack) src
  LT.writeFile distPath (Compiler.compile result)
