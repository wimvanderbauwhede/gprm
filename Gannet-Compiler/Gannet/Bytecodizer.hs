-- |Turn Gannet Symbols and Packet headers into bytecode.

-- TODO: Going from Integer to Word8 and then back to Int or Integer for chr is inefficient. 
-- Work with Integer or Int all the way.
module Gannet.Bytecodizer(
    bytecodize,
    intToBytes
) where
import Gannet.SBA.Types
import Gannet.IEEE754
--import Gannet.Warning (warning)

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Char

-- | Turn Gannet Packet into bytes                
bytecodize :: GannetPacket -> [BS.ByteString]
bytecodize gp =
    let
        (gph,gppl)=gp
    in 
        (bytecodize_header gph)++(bytecodize_payload gppl)

{-
32 bits:
FB_Packet_type=3
FB_Prio=3
FB_Redir=2 
FB_Length=8
FB_To=8
FB_Return_to=8
-}

bytecodize_header :: GannetHeader -> [BS.ByteString]
bytecodize_header gph =
    let
        pt::Word8 
        pt = fromIntegral $ fromEnum (ptype gph)
        rd:: Word8
        rd = fromIntegral $ redir gph
        pr :: Word8
        pr = fromIntegral $ prio gph
        GannetLabelI pto = (to gph)
        tl:: Word8
        tl= lo pto
        GannetLabelI rto=(return_to gph)
        rtl :: Word8
        rtl=lo rto
        prrd :: Word8
        prrd = pr*8+rd
        pl= toInteger $ plength gph
        ll::Word8 
        ll =lo pl
        lh::Word8
        lh = hi pl ll
        th::Word8
        th= hi pto tl
        rth::Word8
        rth=hi rto rtl
        byteword1 :: BS.ByteString
        --byteword1=BS.map (chr . fromIntegral) [pt,prrd,lh,ll,th,tl,rth,rtl]
        byteword1=BS.pack [pt,prrd,lh,ll,th,tl,rth,rtl]
        byteword2 :: BS.ByteString
        byteword2=bytecodize_gs (ack_to gph)
        byteword3 :: BS.ByteString
        byteword3=bytecodize_gs (return_as gph)
    in  
        [byteword1,byteword2,byteword3]


setNSymbols  :: [GannetSymbol] -> [GannetSymbol]
setNSymbols [] = []
setNSymbols ppl =
        ssymbol:operands
    where
        operator:operands = ppl    
        nsymbols = countNSymbols operands
        ssymbol
            | kind operator == K_S =operator{subtask=nsymbols}
            | otherwise = operator
                             
countNSymbols operands = toInteger $ length $ filter (\op -> kind op /= K_X)  operands
   
bytecodize_payload :: [GannetSymbol] -> [BS.ByteString]
bytecodize_payload ppl = map bytecodize_gs (setNSymbols ppl)

bytecodize_gs :: GannetSymbol -> BS.ByteString
bytecodize_gs gs = 
    let 
        wordlist =
            case kind gs of
                K_X -> case datatype gs of
                    T_i -> let 
                                GannetTokenB (GannetBuiltinI i) =name gs
                            in
                                int_to_bytes i
                    T_f -> let
                                GannetTokenB (GannetBuiltinF f) =name gs
                            in
                                flt_to_bytes f
                        
                    T_s -> let
                                GannetTokenB (GannetBuiltinS s) =name gs
                            in
                                str_to_bytes s
                _ -> gs_to_bytes gs
    in
        --BS.map (chr . fromIntegral) wordlist
        BS.pack wordlist

nbytes=8
all1s=18446744073709551616

-- [(56,48,40,32,)24,16,8,0]
bytes= take nbytes (iterate (\x->(x-8)) ((nbytes-1)*8) )

{-
32 bits:
FB_Kind=3 <<5
FB_Datatype=1 <<4
FB_Ext=1 <<3
FB_Quoted=1 <<2
FB_Task=2 << 0
FB_Subtask=16
FB_Name=8

There is a problem: (fromEnum (datatype gs)) is more than 1 bit, so we must reduce this to 1 bit
That means we can encode the T_i/T_f difference, but not T_b or, worse, T_s.
What I can do is use K_Q|T_i to indicate K_B|T_s. The beauty is that we need not change anything to the code!

WV15122008: to add Mode and Reg:
 For ACC/BUFFER/CACHE

F_Reg=7 (3 bits)
FS_Reg=10
F_Mode=3 # 0=normal,1=var/acc,2=buf/stream (2 bits)
FS_Mode=14

 We have Subtask == Mode|1Bit|Reg|DataAddress (2|1|3|10) == Mode|1Bit|Reg|2Bits|NArgs

 K_S:(Datatype):(Ext):(Quoted):Task:Mode|1Bit|Reg|2Bits|NArgs:SCId|Opcode
 K_D:(Datatype):(Ext):Quoted:Task:Mode|1Bit|Reg|10Bits:Name


-}
gs_to_bytes :: GannetSymbol -> [Word8]
gs_to_bytes gs =
    let
        reg_field=(reg gs)
        (_,regval)=reg_field
        modeval =fromIntegral $ fromEnum (mode gs)
-- for both 32- and 64-bit, Subtask is 16 bits
-- However, 32-bit K_B's use up the LSB!
-- So purely for consistency, for K_B I should set the Subtask to 0?         
        stval
            | (kind gs)==K_S || (kind gs)==K_R || (kind gs)==K_C || (kind gs)==K_D
                = (subtask gs) + modeval*16384+(fromIntegral regval)*1024
            | otherwise = subtask gs                        
        st :: Word16 -- FIXME: this is surely not correct for 64 bits?
        st = fromIntegral $ stval
        
        stl :: Word8
        stl
            | kind gs /= K_B = fromIntegral $ mod st 256
            | kind gs == K_B && datatype gs == T_s = fromIntegral st -- used for padding so < 256 
            | otherwise = 0 -- FIXME: ugly hack!!
        sth= fromIntegral $ div (st - fromIntegral stl) 256    
        sname :: Integer
        sname = case name gs of
            GannetTokenL (GannetLabelI li) -> li
            GannetTokenB (GannetBuiltinI bi) -> bi
            GannetTokenS (GannetLabelI sli) -> sli
            _ -> error $ "Name in " ++ (show gs) ++ " not properly numerified\n"
        sn :: Word32
        sn = fromIntegral sname 
        n3 :: Word8
        n3=fromIntegral $ (sn `shiftR` 24) .&. 255
        n2 :: Word8
        n2=fromIntegral $ (sn `shiftR` 16) .&. 255
        n1 :: Word8
        n1=fromIntegral $ (sn `shiftR` 8) .&. 255
        n0 :: Word8
        n0=fromIntegral $ sn .&. 255

        e=(ext gs).&.1
        k
            | kind gs /= K_Unknown =fromEnum (kind gs)
            | otherwise = 0
        dt=fromEnum (datatype gs)   
        q -- This is very late to quote strings, but better late than never :-)
            | kind gs == K_B = 1
            | otherwise = (quoted gs).&.1
        t=task gs               
        kte=fromIntegral $ (k.&.15)*16+(dt.&.0x7)*2+e -- kte is a byte 4|3|1      
        qt=fromIntegral $ (shiftL q 6)+t -- qt is a byte 2|6
    in
        [kte,qt,sth,stl,n3,n2,n1,n0]
-- Neat, what?
-- though in a more HW-ish language it would just be (n & (0xFF<<x))>>x)
-- and ((0xFFFFFFFF+n) & (0xFF << x))>>x
int_to_bytes :: Integer -> [Word8]
int_to_bytes n 
    | n>0 = map (fromIntegral . (\x->(shiftR (n .&. (shiftL 255 x)) x))) bytes
    | n==0 = replicate 4 (fromIntegral 0)
    | otherwise = map (fromIntegral . (\x->(shiftR ((all1s+n) .&. (shiftL 255 x)) x))) bytes


-- can of worms: the GannetBuiltinF type is Double, not Float, so I would need 
--  a conditional there as well.
flt_to_bytes :: Double -> [Word8]        
flt_to_bytes x = 
    let
        fltw = encodeIEEE754 x
    in
        reverse (int_to_bytes fltw)

str_to_bytes :: String -> [Word8]
str_to_bytes str = 
    let
        bytes = map (fromIntegral . ord) str
        nnulls = nbytes - (length bytes)
    in
        if nnulls>0
            then bytes++(replicate nnulls 0)
            else bytes

lo :: Integer -> Word8
lo w16 = fromIntegral $ mod w16 256

hi :: Integer -> Word8 -> Word8            
hi w16 lo= fromIntegral $ div (w16 - fromIntegral lo) 256

-- We assume that the Integer is actually Word16.
-- This is only used for the number of packets in a .tdc
-- just map (chr . fromIntegral) (int_to_bytes i) would do fine I think.
intToBytes :: Integer -> BS.ByteString
intToBytes i =
    let
        il = lo i
        ih = hi i il
        nzeros=nbytes-2
        byte0::Word8
        byte0=0
    in
        BS.append (BS.pack [ih,il]) (BS.replicate nzeros byte0)
