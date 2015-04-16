import Data.List.Split
import Data.List
--import Data.Maybe

import System.Environment
import System.Console.GetOpt

import GHC.Conc (numCapabilities)

import GPC.Parser (parseSource)
import GPC.CodeGen (genCode)
import GPC.Interpreter (genGPIR)
import GPC.TypeScopeChecker (runTypeChecker)
import GPC.AST (Program(..), SrcPos(..),getTaskName)
import GPC.SimplifyAST (simplifyAST)

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s 

parseResult :: String -> String -> Integer -> Either String (Program SrcPos) -> IO ()
parseResult f_name f_path  threads prog = case prog of
    Left err -> putStrLn err
    Right ast ->  do
        let task_name = getTaskName ast 
        case runTypeChecker ast of
            Left err -> putStrLn $ show err
            Right _ -> case genGPIR f_name (simplifyAST ast) threads of
               Left err -> putStrLn err
               Right gpir -> do 
                    outputCode (f_path ++"/"++ f_name ++ ".td") $ ";" ++ task_name ++ ".yml\n" ++ (genCode gpir)
    
-- |Check if threads flag is set and try to obtain number
--  of threads to use
--getThreads :: [String] -> Maybe Integer
--getThreads args = do   
--    arg <- listToMaybe $ filter (threadFlag `isPrefixOf`) args
--    threads <- threadFlag `stripPrefix` arg 
--    return $ ((read threads) :: Integer)
--  where threadFlag = "--threads="    


main = do
        args <- getArgs
        (opts,inp) <- compilerOpts args        
        progName <- getProgName
        if  (elem Help opts)||(inp==[])   -- (opts !! 0) ==  
            then
                do showHelp progName
            else     
                do                
                    let
                        file:_=inp                

--        let file = head args -- the .gpc file, but can have any extension
                        chunks = splitOn "/" file
                        filePrefix = (head $ splitOn "." (last chunks))
                        filePath 
                            | length chunks > 1 = intercalate "/" (init chunks)
                            | otherwise = "."
            -- Set the number of threads in the system to the number specified
            -- in the arguments, if none is present detect number of threads in the system
            -- and use them
                        threads
                            | not $ null (filter hasThreads opts) = 
                                let 
                                    NThreads nth = head (filter hasThreads opts)
                                in 
                                    (read nth)::Integer
                            | otherwise = nsysthreads
                    (parseResult filePrefix filePath threads) . parseSource =<< readFile file            
--            threads = case getThreads args of
--                        Nothing -> fromIntegral numCapabilities
--                        Just n -> n

--        if length args <= 0
--            then putStrLn ("Usage " ++ progName ++ " file" ++ "[--threads=n]")
--            else    (parseResult filePrefix filePath threads) . parseSource =<< readFile file
        
data Flag =  NThreads String | Help deriving (Show,Eq,Ord)

options :: [OptDescr Flag]
options = [ 
    Option ['n']  ["nthreads"]  (ReqArg NThreads "4") "Number of threads to be used"
    , Option ['h','?'] ["help"]            (NoArg Help)            "show some help"        
        ]
 
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts args = 
   case getOpt Permute options args of
      (opts,inp,[]) -> return (opts,inp)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: gpcc [-n NTHREADS] GPC source file (.cc)"    
  

showHelp progName = do
    putStrLn "The GPRM Parallel C compiler"  
    putStrLn "Compiles C++-compatible task code into GPIR"
    putStrLn $ "Without -n, the number of threads is "++(show nsysthreads)
    putStrLn $ "Usage: "++progName++" [-n NTHREADS|--nthreads=NTHREADS] GPC source file (.cc)"        
    
hasThreads :: Flag->Bool
hasThreads (NThreads _) = True
hasThreads _ = False 
    
-- I don't trust Haskell's numCapabilities, so we use at least 2 threads    
nsysthreads = max (fromIntegral numCapabilities) 2         