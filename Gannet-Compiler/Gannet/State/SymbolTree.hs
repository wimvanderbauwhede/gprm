{-# LANGUAGE CPP #-}

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.State.SymbolTree (
    unwrapST,
    unwrapSymbolTree,    
    appendST
) where
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration
--import Gannet.State.Scope
import Gannet.State.Context
--import Gannet.Numerifier
--import Gannet.Symbolizer.InferTypes

import Control.Monad.State 
--import qualified Data.Map as Hash

-- | unwrap the SymbolTree, i.e. get it out of the State monad
unwrapST :: State SymbolTree (TokenTree,Context) -> SymbolTree -> (SymbolTree,Context)
unwrapST lst st0 =
    let
        ((_,ctxt),st) = runState lst st0
    in 
        (st,ctxt)

unwrapSymbolTree :: State SymbolTree (TokenTree,Context) -> (SymbolTree,Context)       
unwrapSymbolTree lst =
    let
        ((_,ctxt),st) = runState lst emptySL
    in 
        (st,ctxt)

{-
I'd like to modify the SLH here: 
If ms is a symbol, do this; if it's a symbollist, do that
-}

-- | Update the SymbolList header
updateSLH :: SymbolTree -> SymbolTree -> Context -> (SymbolTree,Context) -- looks like we could use a monad here
updateSLH st ms ctxt =
    let
        nsl= getSL st         
        slh = getSLH st
        slhl = label slh
        (nslh,_)= case ms of    
            Symbol gs -> 
                if (kind gs)==K_S 
                    then
                        let
                -- set to to current from the context in (name st2)
                            to=GannetLabelS $ lookupFQN $ current ctxt
                -- set return_to to the first elt in the callerstack:
                            _:caller:_=(callerstack ctxt)
                            from=GannetLabelS caller
                            ntagged
                                | (reflabel ctxt) /= emptyGT = True
                                | otherwise = False
                            nlabel = gs{kind=K_R,datatype=datatype slhl,name=(GannetTokenL to)}
                        in
                            (slh{lto=to,lfrom=from,label=nlabel,tagged=ntagged},ctxt)
                    else
                            (slh,ctxt)       
            SymbolList (_,_) ->(slh,ctxt)
        (nsl2,nslh2,ms2,ctxt2) = (nsl,nslh,ms,ctxt)           
    in
        (SymbolList (nsl2++[ms2],nslh2),ctxt2)

        
{-

tct2 <- appendST qsym tct1

appendST appends a SymbolTree (i.e. either a Symbol or a list of Symbols)
to an (invisible ) SymbolTree. You pass it a SymbolTree and
a (TokenTree,Context) tuple. It will modify the Context as required;
appendST does not modify the TokenTree, that happens in t2sm. 

It's just a convenient way of passing the TokenTree around in the monad.


-}

-- | Append a Symbol or SymbolList to the SymbolTree        
appendST :: SymbolTree -> (TokenTree,Context) -> State SymbolTree (TokenTree,Context)
appendST st2 (tt,ctxt) = state (\st ->
        let
            (st3,ctxt2)=updateSLH st st2 ctxt
            ctxt3=ctxt2{reflabel=emptyGT}
        in
            ((tt,ctxt3),st3)    
    )

