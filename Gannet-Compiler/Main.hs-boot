module Main (
	ymlFileName
) where 
-- import System.Environment

-- import System.Console.GetOpt
-- import Data.Maybe ( fromMaybe )
-- import System.IO.Unsafe

{-
Gannet main:
    -read td file
    -tokenize
    -symbolize
    -packetize
    -numerify    
    -bytecodize
    -write tdc file

Command-line arguments:
    -h: help
    -S: show the compiled code
    -v: be verbose
    -V: version
    -w: warnings
    -P5: Perl5
    -P6: Perl6
    -S: Scheme
    -H: Haskell
There might be others later, e.g. for "virtual" services        
-}

{-
To get at ymlfile I should do the command line parsing separately and return
-}
ymlFileName :: String
