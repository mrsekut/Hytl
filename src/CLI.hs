module CLI
    ( cli
    )
where

import           Repl                           ( astRepl
                                                , evalRepl
                                                )
import           Options.Applicative


newtype CLI = CLI { ast :: Bool }


cli :: IO ()
cli = astOption =<< execParser opts
    where opts = info (config <**> helper) (fullDesc <> progDesc "Hytl REPL")

config :: Parser CLI
config =
    CLI <$> switch (long "ast" <> short 'a' <> help "Target for the greeting")


astOption :: CLI -> IO ()
astOption (CLI False) = evalRepl
astOption _           = astRepl
