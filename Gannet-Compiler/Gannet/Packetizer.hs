{-# LANGUAGE CPP #-}
-- | Functions for turning a SymbolTree into a Gannet PacketList.
-- Functions for emitting human-readable and bytecode strings.

-- WV20110609 for old config handling, revert to before r4987 and undef NEW

module Gannet.Packetizer (
    packetize,
    gplToWords,
--    writeData    
) where
import Gannet.SBA.Types
--import Gannet.SBA.SystemConfiguration
import Gannet.Numerifier
import Gannet.Bytecodizer
--import Gannet.State.Scope
import Gannet.State.Context
--import Gannet.Warning (warning)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8

import Control.Monad.State 
import qualified Data.Map as Hash

packetize :: SymbolTree -> Context -> Bool -> PacketList
packetize st _ num = pl  
    where
        stw=SymbolList ([st],emptySLH)
        st_r=(stw,emptyGS)
        pl = unwrapPL (st2plm st_r num) emptyGPL
        

-- type () = Integer -- Hash.Map Integer [GannetSymbol]
--zero = () -- Hash.empty
  
unwrapPL :: State PacketList () -> PacketList -> PacketList
unwrapPL lst pl0 =
    let
        (_,pl) = runState lst pl0 
    in 
        pl

updatePL :: PacketList -> SymbolTree -> GannetSymbol -> Bool -> ((),PacketList)
updatePL pl st r num = 
    let
        slh = getSLH st
        ref = label slh
        nref = ref
        nnref=numerify nref num         
        p_to = newnumService (lto slh) num -- NEW: this is the nodeId, so use lookupNodeIdFromFQN
        pn = case Hash.lookup r pl of
            Just p -> 
                let 
                    (nph,nppl) = p -- warning p (show p)  
                    (nppl2,nref2,plen2)
                        | length nppl == 0 = ([],nnref,0)
                        | kind (last nppl)==K_Q = 
                            let
                                gss:rest=nppl
                                nppl1=gss{count=(count gss)-1}:rest
                            in
                                (init nppl1,nnref{quoted=1},(plength nph)-1)
                        | otherwise = (nppl,nnref,plength nph)
                in 
                    if (length nppl2 == 0) || (last nppl2 /= nref2{quoted=1}) 
--                    if last nppl2 /= nref2{quoted=1} 
                        then
                            (nph{plength=(plen2+1)},nppl2++[nref2])
                        else
                            (nph{plength=plen2},nppl2)
            Nothing -> (emptyGH,[nnref])
        pl2 = Hash.insert r pn pl
        p_ret  
            | num = GannetLabelI 0
            | otherwise =  GannetLabelS "gateway"
        -- WV12082008: this works, but why does numerify not work?
--        refnamestr = (\ref -> let (GannetTokenL (GannetLabelS namestr)) = name ref in namestr) nref
--        nnref2=nref{name=newnumServiceGT refnamestr num}
        ph = MkGannetHeader P_code 0 0 0 p_to p_ret nullGS nnref        
    in
        ((),Hash.insert nnref (ph,[]) pl2)

appendPL :: SymbolTree -> GannetSymbol -> Bool -> State PacketList ()         
appendPL x r num = state (\pl->(updatePL pl x r num))        
        


{-
This one takes a PacketList and a key, and a GannetSymbol to be added at the
end of the packet payload
-}
updateP :: PacketList ->  SymbolTree -> GannetSymbol -> PacketList
updateP pl x r =
    let
        Symbol gs = x 
        pn= case Hash.lookup r pl of
                Just p -> 
                    let 
                        (nph,nppl) = p 
                        (nppl2,ngs,plen2)
                            | nppl == [] = ([],gs,0)
                            | kind (last nppl)==K_Q = 
                                let
                                    gss:rest=nppl
                                    nppl1=gss{count=(count gss)-1}:rest
                                in
                                    (init nppl1,gs{quoted=1},(plength nph)-1)                            
                            | otherwise = (nppl,gs,plength nph)
                        nppl3
                            | (kind ngs)==K_R && (ext ngs)==1 && (quoted ngs)==0 && (count ngs)==(subtask r) =
                                let
                                    gss:rest=nppl2                                    
                                in
                                    gss{count=(count gss)+1}:rest
                            | otherwise = nppl2
                        egs=extendGS ngs
                    in 
                        (nph{plength=(plen2+(length egs))},nppl3++egs)
                Nothing -> (emptyGH,extendGS gs)
    in
        Hash.insert r pn pl    

appendP :: SymbolTree -> GannetSymbol -> State PacketList () 
appendP x r = state (\pl->((),updateP pl x r))


-- transform Symbol Tree into Packet List Monad
-- GannetSymbol (r in st_r) is the key in the PacketList hashtable 
                      
st2plm :: (SymbolTree,GannetSymbol) -> Bool -> State PacketList ()
st2plm st_r num
    | length sl==0 = do return () -- we're done: the symbol tree list is empty
    | isSL x = -- it's a list of symbols
        do
            let
                rn = numerify r num
            appendPL x rn num
            st2plm (x,rn) num 
            st2plm st_r'  num
    | otherwise = -- it's a symbol
        do
            let 
                Symbol xgs = x 
            appendP (Symbol (numerify xgs num)) r'
            st2plm st_r' num
    where
        (SymbolList stl,r)=st_r
        (sl,slh)=stl
        x:xs=sl -- x is the first elt in the list of symbol trees, can be a symbol or a list because we consume the list one by one
        st'=SymbolList (xs,slh) -- st1 is a new SymbolTree where the list is the remainder of the original list
        -- create a reference for the packet. 
        r'=numerify (label slh) num 
        st_r'=(st',r') -- this is the next iteration  

-- | Either turn a PacketList into a pretty-print string or into a bytecode string for writing to a .tdc file.        

gplToWords :: PacketList -> Bool -> BS.ByteString       
-- | Either turn a PacketList into a pretty-print string or into a bytecode string for writing to a .tdc file.        
gplToWords gpl numeric = 
    let 
        p = case Hash.lookup emptyGS gpl of 
            Just p -> p
            Nothing -> error $ "No rootref in "++show gpl
        (_,ppl) = p
        rootref= head ppl
        (rph,_) = case Hash.lookup rootref gpl of 
            Just (ph,ppl) -> (ph,ppl)         
            Nothing -> error $ "Rootref "++show rootref++"not in "++show (Hash.toList gpl)
        gpl1 = gpl
        rootref2 = emptyGS{kind=K_R, name=name (return_as rph)}
        rph2=rph{ptype=P_reference,plength=1,to=to rph}
        rppl2 = [ return_as rph2 ]
        gpl12=Hash.delete emptyGS gpl1
        gpl2    =Hash.insert rootref2 (rph2,rppl2) gpl12    
        labels = Hash.keys gpl2
        npackets
            | numeric==True = intToBytes $ toInteger (length labels)
            | otherwise = BSC8.pack ("NPackets: "++show (length labels))
        strlist=    map (\label -> (gpToWords ((\label gpl2 -> case Hash.lookup label gpl2 of Just p -> p) label gpl2) numeric)) labels        
    in    
        BS.append npackets (BS.concat strlist)
        
gpToWords :: GannetPacket -> Bool -> BS.ByteString
gpToWords gp numeric
    | numeric==True = BS.concat $ bytecodize gp
    | otherwise = 
        let 
            (gph,gppl) = gp 
        in
             BS.append (BSC8.pack (show gph)) (BS.concat (map addNL gppl))

addNL :: GannetSymbol -> BS.ByteString
addNL gs =  BSC8.pack $ (show gs)++"\n"
