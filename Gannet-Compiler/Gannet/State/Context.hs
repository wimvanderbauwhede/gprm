-- | State information required for compilation
module Gannet.State.Context ( 
    Context(..), 
    RegInfo(..),
    AddrCounters,
    emptyC,
    inLambda,
) where
import Gannet.State.Scope
import Gannet.SBA.Constants
import Gannet.SBA.Types
import Gannet.SBA.SystemConfiguration -- for constants

import qualified Data.Map as Hash

{-
We need a lambdastack because we need the subtask of the lambda as 
the count of any L inside that lambda. 
So we need currentlambda and lambdastack, with lambdastack containing all 
parents of the current lambda.
So if we detect a lambda, we say: 
currentlambda=subtaskc
When we leave a lambda, we do:
currentlambda=lambdastack.pop
i.e 
ncurrentlambda:nlambdastack=(lambdastack ctxt)
ctxt{currentlambda=ncurrentlambda,lambdastack=nlambdastack}
When we enter a new lambda, we do:
lambdastack.push currentlambda
i.e. 
nlambdastack=(currentlambda ctxt):(lambdastack ctxt)
ncurrentlambda=subtaskc
And if currentlambda==0, it's not a lambda.
so the initial values are: currentlambda=0, lambdastack=[]

We can have inLambda for convenience:
inLambda ctxt = (currentlambda ctxt)/=0
-}

{-
For proper direct addressing, we obviously need an address counter per service.
Subtasks "tainted" by LAMBDA, i.e. to be stored at APPLY, need to use the APPLY counter.
All counters must start at an offset which determines the size of the stack for recursion.
So instead of subtaskc::Integer, we need subtaskc::[Integer] if contiguous or 
subtaskc::AddrCounters
with 
type AddrCounters = Hash.Map Integer Integer

The initial value of AddrCounters is determined by the available services (from Gannet.SBA.SystemConfiguration)
and the recursion-stack size (which might vary per service)
In fact, we would need to add the available memory per service as well, so we can check if the program will fit
For now, let's make these values global for all services

Maybe a better approach is to inventorise and remap the subtasks for every service after the tree has been built
That way we can use a continuous count for determining scope etc
We simply use the AddrCounters to run through all symbols and remap the Subtask field based on the Name field

WV 03122008: So where do we do this? I guess after Numerify and obviously before Bytecodize/Packetize
It turns out I've already done this, in Symbolizer.Internals there is remapSubtask which is called from createSymCtxt

-}

data Context = MkContext { 
                        letc::Integer, -- ^ LET block subtask count
                        outerc::[Integer], -- ^ Stack of enclosing LETs
                        current::String, -- ^ Service name of current block
                        callerstack::[String], -- ^ Caller stack of current block 
                        currentlambda::Integer, -- ^ Subtask count of closest LAMBDA
                        lambdastack::[Integer], -- ^ Stack of enclosing LAMBDAs
                        currentassign::GannetToken,
                        assignstack::[GannetToken],
                        assignvarstacks::AssignStack,
                        taskc::Int, -- ^ Task count. Is constant for the compilation
                        subtaskc::Integer, -- ^ Running subtask count. This should be split per service 
                        varc::Integer, -- ^ Running variable (D,L,A) count
                        scopetype::Int, -- ^ let=0,lambda=1
                        scope::ScopeTable, -- ^ See "Gannet.State.Scope"
                        reflabel::GannetToken,
                        reflabelt::RefLabelTable,
                        numeric::Bool, -- ^ For numerification
                        mon::[String],
                        addrcounters::AddrCounters,
                        registers::Registers,
                        regvartable::RegVarTable,
                        varbindings::VarBindings, -- ^ For type inference
                        prevsym::GannetSymbol -- ^ To deal with BUF, VAR etc
                        }                        

-- | Initial values for Context 
-- NOTE that @varc=1024@, totally 'ad hoc'
-- emptyDataStore
emptyC :: Context
--emptyC = (MkContext 1 [0] "_" ["GATEWAY"] 0 [] emptyGT [] Hash.empty 1 1 1024 0 emptyScope emptyGT emptyLT False [] emptyAddrCounters emptyRegisters emptyRVT emptyVarBindings emptyGS) -- subtaskc was 0
emptyC = (MkContext 1 [0] "_" ["GATEWAY"] 0 [] emptyGT [] Hash.empty 1 1 0 0 emptyScope emptyGT emptyLT False [] emptyAddrCounters emptyRegisters emptyRVT emptyVarBindings emptyGS) -- subtaskc was 0

type AddrCounters = Hash.Map Integer Integer

-- FIXME: this creates and Address Counter per service name, not per service id
-- we need a lookup token->id
initAddrCounters :: AddrCounters -> [Integer] -> AddrCounters
initAddrCounters addrcounters service_ids 
    | service_ids==[] = addrcounters
    | otherwise = 
        let
            service_id:xs=service_ids
            offset=c_REC_STACK_SZ -- constant, but Haskell doesn't allow all-caps constants
            ac = Hash.insert service_id offset addrcounters
        in
            initAddrCounters ac xs

emptyAddrCounters = initAddrCounters Hash.empty serviceids -- FQN: service node ids


-- WV21112008 For BUF/STREAM/VAR we must add a lookup table registers::Registers
-- This lookup table should be populated with #registers for every service 
-- we have servicetoken => (nregs,var_reg_table)
-- where var_reg_table is a Hash from GannetToken to Integer

-- | Number of unused registers per service
type Registers  = Hash.Map Integer Integer 

initRegisters :: Registers -> [Integer] -> Registers
initRegisters registers service_ids 
    | service_ids==[] = registers
    | otherwise = 
        let
            sid:xs=service_ids
--            nregs=c_N_REGS -- constant, but Haskell doesn't allow all-caps constants
            ac = Hash.insert sid (c_N_REGS-1) registers
        in
            initRegisters ac xs

emptyRegisters = initRegisters Hash.empty serviceids

type RegVarTable = Hash.Map GannetToken RegInfo 
emptyRVT :: RegVarTable
emptyRVT = Hash.empty

data RegInfo = MkRegInfo {
                    register::Integer, 
                    service_id::Integer
                }

{-
Now how do we use the Registers?

Every time I encounter a Register assignment (BUF|VAR|CACHE 'v (S ...))
I read the Register for the corresponding Service. 
If the regcounter is not zero, we take the value and bind the varname to that value in regvarlist;
otherwise we die as the it means all registers have been used.
We must also enter the varname in the ScopeTable. This might be a problem as I think the scopetable really only treats
LET blocks and LAMBDAs as scopes. The easy way out is to make a RegScopeTable.

Now the lookup: We look in RegScopeTable before looking LET scope table. If we find a varname we get its reg num

The cleanup: when we leave the block enclosing the assignment, all registers should be reset to the initial value
That means that the Registers table contains the registers active for a given Subtask for a given Service, not simply for a given Service. 

-}


type  RefLabelTable = Hash.Map GannetToken GannetSymbol
emptyLT :: RefLabelTable
emptyLT = Hash.empty

-- Why is this here? TODO: Find a better place
inLambda :: Context -> Integer
inLambda ctxt = currentlambda ctxt

type AssignStack = Hash.Map GannetToken [Integer]

type VarBindings = Hash.Map GVarName SymbolTree
emptyVarBindings = Hash.empty
