module Gannet.SBA.Constants (
------ constants
c_CODE_SZ,
c_REC_STACK_SZ,
c_N_REGS,
c_NBITS_NOT_EXT,
c_FS_SNId,
c_FW_SNId,
c_FS_SCLId,
c_FW_SCLId,
c_FS_SCId,
c_FW_SCId,
c_FS_Opcode,
c_FW_Opcode,
c_WORDSZ,
-- field operations
shiftSNId,
shiftSCLId,
shiftSCId,
shiftOpcode,
getSNId,
getSCLId,
getSCId,
getOpcode
) where

import Data.Bits

shiftSNId snid =  (snid .&. (fromInteger c_FW_SNId)) `shiftL` (fromInteger c_FS_SNId)
shiftSCLId sclid =  (sclid .&. (fromInteger c_FW_SCLId)) `shiftL` (fromInteger c_FS_SCLId)
shiftSCId scid =  (scid .&. (fromInteger c_FW_SCId)) `shiftL` (fromInteger c_FS_SCId)
shiftOpcode opc =  (opc .&. (fromInteger c_FW_Opcode)) `shiftL` (fromInteger c_FS_Opcode)
            
getSNId num = (num `shiftR` (fromInteger c_FS_SNId)) .&. c_FW_SNId            
getSCLId num = (num `shiftR` (fromInteger c_FS_SCLId)) .&. c_FW_SCLId
getSCId num = (num `shiftR` (fromInteger c_FS_SCId)) .&. c_FW_SCId
getOpcode num = (num `shiftR` (fromInteger c_FS_Opcode)) .&. c_FW_Opcode

-- TODO: change for 64-bit?
c_CODE_SZ = 256
c_REC_STACK_SZ = 16
c_N_REGS = 8

--c_SHIFT_SCId = 32



-- Max size of signed integer for not-extended builtin
-- 64-bit Word
c_NBITS_NOT_EXT = 0x7FFFFFFFFFFF


-- Shifts and masks for SCId and Opcode


-- 64-bit Word
c_FW_Opcode = 0xFF
c_FS_Opcode = 0
c_FW_SCId = 0xFF
c_FS_SCId = 8  
--c_FW_SCLId = 0xFF
--c_FS_SCLId = 16
--c_FW_SNId = 0xFF
--c_FS_SNId = 24
--Ashkan changed it
c_FW_SCLId = 0xF
c_FS_SCLId = 16
c_FW_SNId = 0xFFF
c_FS_SNId = 20

 
c_WORDSZ :: Int
-- 64-bit Word
c_WORDSZ = 64

