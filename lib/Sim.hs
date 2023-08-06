module Sim (loadProgram, advance, rewind) where

import Data.Bits
import Data.Int (Int32)
import Data.Vector.Unboxed (Vector (..))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (write)

data Lc2200 = MkLC
  { regFile :: Vector Int32
  , memory  :: Vector Int32 -- LC-2200 is 4-byte addressable, not byte-addressable.
  , pc      :: Int
  }

instance Show Lc2200 where
  show lc = "MkLC\n{ regFile = " ++ show (regFile lc) 
          ++ "\n, memory[pc:pc+10] = " ++ show (V.slice (pc lc) 10 (memory lc)) -- don't print the whole memory
          ++ "\n, pc = " ++ show (pc lc)
          ++ "\n}"

data Reg =
  Zero
  | At
  | V0
  | A0
  | A1
  | A2
  | T0
  | T1
  | T2
  | S0
  | S1
  | S2
  | K0
  | Sp
  | Fp
  | Ra
  deriving (Show, Eq, Enum)

-- | Represents a state change for a given clock cycle. 
data StateChange =
  RegisterUpdate Reg Int32 -- reg old_val
  | MemoryUpdate Int Int32 -- index old
  | Jump Int -- oldpc
  | Subroutine Reg Int Int32 -- for jalr, which simultaneously jumps and updates registers. SR ra old_pc old_ra
  | Halt -- signals to debugger to stop
  deriving Show
  
-- Theoretical memory optimization idea:
-- If we can detect a loop, we can capture a block of state changes
-- and store it as `Loop #iters [stateChange1..stateChangen]` 

loadProgram :: Vector Int32 -> Lc2200
loadProgram program = MkLC
  { regFile = V.replicate 16 0
    -- Witnessing this much deep-copying puts me in great pain.
    -- But in the name of science, I will stick to the pure-functional meme.
  , memory = program V.++ V.replicate remaining_space 0 
  , pc = 0
  }
  where
    remaining_space = 2 ^ 16 - (V.length program)

advance :: (Lc2200, [StateChange]) -> (Lc2200, [StateChange])
advance (lc, history) =
  case op of
    0x0 -> -- Add
      alter_reg_file (uncurry (+)) (ry_data, rz_data)
    0x1 -> -- Nand
      alter_reg_file (complement . uncurry (.&.)) (ry_data, rz_data)
    0x2 -> -- Addi
      alter_reg_file (uncurry (+)) (ry_data, imm20)
    0x3 -> -- Lw 
      alter_reg_file (memory lc V.!) (pc lc + off)
    0x4 -> -- Sw
      write_mem
    0x5 -> -- Br
      (lc { pc = pc lc + off }, Jump (pc lc) : history) 
    0x6 -> -- Jalr
      (lc { pc = fromIntegral ry_data
          , regFile = V.modify (\regs -> write regs rx (fromIntegral $ pc lc)) (regFile lc) 
          }
      , Subroutine (toEnum rx) (pc lc) rx_data : history)
    0x7 -> -- Halt
      (lc, Halt : history)
    0x8 -> -- Blt
      let (pc_off, hist) = if rx_data < ry_data
                           then (off, Jump (pc lc) : history)
                           else (0, history)
      in
      (lc { pc = pc lc + pc_off }, hist)
    0xA -> -- LEA
      alter_reg_file (fromIntegral . uncurry (+)) (pc lc, off)
    _ -> error "Invalid opcode."
  where
    -- Parse the blob!
    -- access memory and efficiently wrap pc so it doesn't reach out-of-bounds.
    inst = memory lc V.! (pc lc .&. (1 `shiftL` 16 - 1))
    (op, rx, ry, rz, imm20) = splitInst inst
    off = fromIntegral imm20 :: Int

    rx_data = regFile lc V.! rx
    ry_data = regFile lc V.! ry
    rz_data = regFile lc V.! rz

    -- `rx` is always the recipient of register writes.
    alter_reg_file operation args =
      (lc { pc = pc lc + 1
          , regFile = V.modify (\regs -> write regs rx result) (regFile lc) 
          }
      , RegisterUpdate (toEnum rx) rx_data : history)
      where
       result = operation args

    write_mem =
      let 
        index = fromIntegral $ ry_data + imm20
        old_value = memory lc V.! index
      in
      (lc { pc = pc lc + 1
          , memory = V.modify (\mem -> write mem index rx_data) (memory lc)
          }
      , MemoryUpdate index old_value : history) 


splitInst :: Int32 -> (Int32, Int, Int, Int, Int32)
splitInst inst = (op, rx, ry, rz, imm20)
  where 
    op = (inst .&. 0xf0000000) `shiftR` 28
    rx = fromIntegral $ (inst .&. 0x0f000000) `shiftR` 24
    ry = fromIntegral $ (inst .&. 0x00f00000) `shiftR` 20
    rz = fromIntegral $ inst .&. 0x0000000f
    imm20 = let u_imm = inst .&. 0x000fffff in
      if u_imm .&. (1 `shiftL` 19) == 0 -- sign-extension
      then u_imm
      else -u_imm

stepBack :: Lc2200 -> StateChange -> Lc2200
stepBack lc event =
  case event of
    RegisterUpdate reg prev_val ->
      lc { pc = pc lc - 1
         , regFile = V.modify (\regs -> write regs (fromEnum reg) prev_val) (regFile lc)
         }
    MemoryUpdate idx prev_val ->
      lc { pc = pc lc - 1
         , memory = V.modify (\mem -> write mem idx prev_val) (memory lc)
         }
    Jump old_pc ->
      lc { pc = old_pc }
    Subroutine ra old_pc old_ra ->
      lc { pc = old_pc
         , regFile = V.modify (\regs -> write regs (fromEnum ra) old_ra) (regFile lc)
         }
    Halt ->
      lc -- Halting does not change lc at all, so this just consumes the event.

rewind :: (Lc2200, [StateChange]) -> Int -> (Lc2200, [StateChange])
rewind (lc, history) steps = (foldl stepBack lc changes, remaining_history)
  where
    (changes, remaining_history) = splitAt steps history

