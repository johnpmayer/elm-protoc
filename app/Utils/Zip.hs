
module Utils.Zip where

import qualified Codec.Archive.Zip as Zip
import qualified Network.HTTP.Client as Client

import Utils.Http as Http

extract :: FilePath -> Http.Handler ()
extract destination request manager = 
  let
    zipOpts = [Zip.OptDestination destination]
  in do
    response <- Client.httpLbs request manager
    let archive = Zip.toArchive (Client.responseBody response)
    Zip.extractFilesFromArchive zipOpts archive
