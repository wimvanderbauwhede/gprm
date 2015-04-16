{-# LANGUAGE CPP #-}
-- |The symbolizer transforms a TokenTree into a SymbolTree. 
-- This is the core of the compiler. The SymbolTree is the Abstract Syntax Tree
-- Gannet symbol Kinds are identified based on the token type, position in the expression or from the context.
-- Token types are Builtin, Quote, Service or Label. A service is the first elt in the S-expression list
-- A label is any element in the S-expression that is not a Builtin, Quote or Service

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Symbolizer (
    symbolize
) where
import Gannet.SBA.Types
--import Gannet.SBA.SystemConfiguration
--import Gannet.Symbolizer.InferTypes
import Gannet.State.SymbolTree
import Gannet.State.Context
--import Gannet.State.Scope
--import Gannet.Numerifier
import Gannet.Symbolizer.Internals
--import Gannet.Warning (warning)

import Control.Monad.State
--import System.IO
--import qualified Data.Map as Hash
--import Text.ParserCombinators.Parsec hiding (State)


-- | The Bool argument indicates if the symbols are numerified or not.
-- The returned Context contains the DataStore.  
-- We use the State monad to pass around the SymbolTree        
symbolize :: TokenTree -> (SymbolTree,Context)
symbolize tt = (st,ctxt)
    where
        (st,ctxt) = (unwrapST (t2sm (tt,emptyC)) emptySL)

-- | A monadic tree walker to convert the TokenTree into a SymbolTree
t2sm :: (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
t2sm (tt,octxt)
    | length tl ==0 = do -- recursion is finished
        let
            ncurrent:ncallerstack= callerstack ctxt
            nletc:nouterc=(letc ctxt):(outerc ctxt)
            -- OBSOLETE                                                                          
            ncurrentlambda:nlambdastack
                | currentlambda ctxt == subtaskc ctxt = lambdastack ctxt
                | otherwise = (currentlambda ctxt):(lambdastack ctxt)
            nctxt = ctxt
            nnctxt=nctxt{ letc=nletc,
                        outerc=nouterc,
                        current=ncurrent,
                        callerstack=ncallerstack,
                        currentlambda=ncurrentlambda,
                        lambdastack=nlambdastack
                        }
        return (tt,nnctxt)    
    | isTokenList x = do -- it's a list
        let
            nsubtaskc= (subtaskc ctxt)+1
--            TokenList (fx:xs)=x
        tct <-  let                
                    (st2,nctxt) = unwrapST (t2sm (x,ctxt{subtaskc=nsubtaskc})) emptySL -- this creates a new ST based on the TT x
                in                        
                    appendST st2 (tt2,nctxt) 
        t2sm tct     
    | otherwise =         -- It's a token
        let
            (xsym2,nctxt) = createSymCtxt x tl ctxt -- ("createSymCtxt: "++(show x)++"\n")
        in    
            do            
                case (stringInToken x) of
                    "label" -> do
                        tct3 <- parseLabel (tt2,ctxt)
                        t2sm tct3
                    _ -> do
                        tct1 <- (appendST xsym2 (tt2,nctxt)) -- ("\tappendST: "++(show xsym2)++"\n")
                        t2sm tct1                                                                             
    where
        --(TokenList tl)=tt -- it seems tt here can never be a Token
        tl=returnTokenList tt
        -- it seems tl can be [xtl] or not                
        ctxt = octxt 
        x:xs=tl
        tt2=(TokenList xs)

returnTokenList (TokenList tl)=tl
returnTokenList tt= [tt]


--------------------------------------------------------------------------------
-- Label handling
--------------------------------------------------------------------------------

parseLabel tct = 
    do
        return tct1
    where
        ((TokenList ltl),ctxt)=tct                
        lbl:[tl]=ltl 
        Token glbl=lbl
        nctxt=ctxt{reflabel=glbl}
        tct1=(tl,nctxt)    
       
