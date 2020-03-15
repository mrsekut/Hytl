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
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.IO             as LT
import           Compiler
import           Data.Text.Internal
import qualified Data.Text                     as TX
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
cliOption (CLI True  _) = astRepl
cliOption (CLI False s) = comp s
cliOption (CLI False _) = evalRepl


comp :: FilePath -> IO ()
comp filePath = do
    let distPath = replaceExtension filePath ".ll"
    src <- T.readFile filePath
    let fs     = TX.head src -- FIXME: \nをlexerで処理できるようにする
    -- let result = (parse . lexer) fs
    let result = (parse . lexer) [fs]
    LT.writeFile distPath (Compiler.compile result)
