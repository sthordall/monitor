module EngineOptionsParser
  ( parseOptions
  ) where

import Data.Semigroup ((<>))
import Monitor
import Options.Applicative

optsParser :: Parser EngineOptions
optsParser = EngineOptions
  <$> switch
      (  long "monitor"
      <> short 'm'
      <> help "Start monitoring with RESTful API server" )
  <*> option auto
      (  long "port"
      <> showDefault
      <> value 3000
      <> metavar "MONITOR_PORT"
      <> help "Defines port number RESTful API server is accepting connections on" )
  <*> option auto
      (  long "delay"
      <> showDefault
      <> value 30
      <> metavar "DELAY_BETWEEN_CHECKS_SECONDS"
      <> help "Defines delay between checks in seconds" )
  <*> strOption
      (  long "path"
      <> short 'p'
      <> metavar "PATH"
      <> help "Path where script to be located" )

optsInfo :: ParserInfo EngineOptions
optsInfo = info (optsParser <**> helper)
  (  fullDesc
  <> progDesc "Runs a set of scripts and aggregates results" )

parseOptions :: IO EngineOptions
parseOptions = execParser optsInfo
