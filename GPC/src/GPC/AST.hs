

{- GPC abstract syntax tree -}
module GPC.AST(    
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Literal(..)
    , Ident(..)
    , Type(..)
    , BlockStmt(..)
    , Assign(..)
    , FunCall(..)
    , ClassName
    , Objects(..)
    , MethodCall(..)
    , Var(..)
    , ConstructObjs(..)
    , LibName
    , Pointer(..)
    , SrcPos(..)
    , getTaskName
    ) where


data Program a = Program [TopLevel a] deriving (Show,Eq)

data SrcPos = SrcPos Int Int deriving (Show)
instance Eq SrcPos where
    _ == _ = True

-- | Top Level Expressions
data TopLevel a =
        Func (Type a) (Ident a, Ident a) [(Type a, Ident a)] (BlockStmt a)  -- ^ Return Type, (Task Name, Function Name), Arguments, Code
      | TLObjs (Objects a) -- ^ External objects
      | TLConstructObjs (ConstructObjs a) -- ^ External object constructor calls
      | TLAssign (Assign a) -- ^ Top level assignment
       deriving (Show, Eq)

-- | Objects
data Objects a = Objects {
   nameSpace :: [Ident a],
   objVar :: Var a 
} deriving (Show, Eq)

-- | Variable
data Var a = 
          VarArrayElem (Ident a) (Expr a) -- ^ Element of Array
        | VarIdent (Ident a) -- ^ Ordinary identifier
         deriving (Show, Eq)

-- | Constructing Objects
data ConstructObjs a = ConstructObjs [Ident a] (Var a) [Expr a] deriving (Show, Eq)

-- | Statement
data Stmt a = 
        AssignStmt (Assign a) -- ^ Type Name, assignment
      | Seq (BlockStmt a) -- ^ Evaluate statements in sequential order
      | BStmt (BlockStmt a) -- ^ Statements in enclosed block
      | FunCallStmt (FunCall a) -- ^ Call Funcion
      | MethodStmt (MethodCall a) -- ^ Call Object method
      | If (Expr a) (Stmt a)  -- ^ If statement
      | IfElse (Expr a) (Stmt a) (Stmt a) -- ^ If Else statement
      | Return (Expr a) -- ^ Return value from current function
      | ForLoop (Ident a) (Expr a) (Expr a) (Expr a) (BlockStmt a) -- ^ Start, Stop, Step, statements, static for loops
       deriving (Show, Eq)

data Assign a = Assign (Type a) (Ident a) (Expr a) deriving (Show, Eq) -- ^ Variable assignment
data FunCall a = FunCall (Ident a) [Expr a] deriving (Show, Eq) -- ^ Function call layout
data MethodCall a = MethodCall { 
      mVar :: Var a, 
      mName :: Ident a, 
      mArgs ::  [Expr a] 
} deriving (Show, Eq) -- ^ Method call layout

-- | Expression
data Expr a =
      ExpBinOp (BinOps a) (Expr a) (Expr a) -- ^ Binary operation with 2 sub-expressions
    | ExpUnaryOp (UnaryOps a) (Expr a) -- ^ Unary operation with sub-expression
    | ExpFunCall (FunCall a) -- ^ Function Call
    | ExpMethodCall (MethodCall a) -- ^ C++ Object method call
    | ExpIdent (Ident a) -- ^ Identifier  
    | ExpLit (Literal a) -- ^ Constant/Literal value
     deriving (Show, Eq)


-- | Binary Operators
data BinOps a =
      Add a -- ^ Addition
    | Sub a -- ^ Subtraction
    | Mul a -- ^ Multiplication
    | Div a -- ^ Division
    | And a -- ^ Boolean AND
    | Or a  -- ^ Boolean OR
    | Mod a -- ^ Modulo
    | Less a -- ^ Less than 
    | LessEq a -- ^ Less than or equal to
    | Greater a -- ^ Greater than
    | GreaterEq a -- ^ Greather than or equal to
    | Equals a -- ^ Equals
    | NEquals a -- ^ Not Equal
    | ShiftL a -- ^ Logical Shift Left
    | ShiftR a -- ^ Logical Shift Right
    | BAnd a -- ^ Bitwise AND
    | BXor a -- ^ Bitwise XOR
    | BOr a -- ^ Bitwise OR
     deriving (Show, Eq)


-- | Unary Operators
data UnaryOps a =
      Not a -- ^ Boolean NOT
    | Neg a -- ^ Number negation
    | BNot a -- ^ Bitwise NOT
     deriving (Show, Eq)

-- | Literal/Constants
data Literal a =
      Str a String -- ^ String
    | Ch a Char -- ^ Char
    | Number a (Either Integer Double) -- ^ Numbers, either Int/Double
    | Bl a Bool -- Boolean
     deriving (Eq)

instance Show (Literal a) where
    show (Str _ s) = s
    show (Ch _ c) = show c
    show (Number _ (Left i)) = show i
    show (Number _ (Right d)) = show d
    show (Bl _ b) = show b


-- | C++ Library
type LibName a = Ident a

-- | C++ Class Name  
type ClassName a = Ident a

-- | Identifier
data Ident a = Ident a String 

instance Show (Ident a) where
    show (Ident _ s) = s

instance Ord (Ident a) where
    (Ident _ s1) `compare` (Ident _ s2) = s1 `compare` s2

instance Eq (Ident a) where
    (Ident _ s1) == (Ident _ s2) = s1 == s2


data Pointer a = Pointer (Ident a) Integer deriving (Show, Eq) -- |Pointer to array elem with offset

-- | Types
data Type a = PointerType (Type a) -- Pointer to a Type
          | NormalType a Bool String  -- ^ Type with annotation, True/False is type is a Kernel Type, and Type name
          deriving (Eq) 

instance Show (Type a) where
    show (PointerType p) = "\"" ++ filter (/= '"') (show p ++ "*") ++ "\""
    show (NormalType _ True s) = show $ "Kernel " ++ s 
    show (NormalType _ False s) = show s



-- | Block of Statements
data BlockStmt a = BlockStmt [Stmt a] deriving (Show, Eq)

getTaskName :: Program SrcPos -> String
getTaskName ast = let
--        isFunc :: TopLevel -> Bool
        isFunc (Func _ _ _ _) = True
        isFunc _ = False    

        Program tls = ast
        funcs = filter isFunc tls
        f = head funcs
        Func _ (task_id, _) _ _ = f
        Ident _ task_name = task_id
    in
        task_name