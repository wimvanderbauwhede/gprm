module GPC.TypelessAST where


data Program = Program [TopLevel] deriving (Show,Eq)


-- | Top Level Expressions
data TopLevel =
        Func Ident Ident [Ident] BlockStmt  -- ^ Return Type, Name, Arguments, Code
      | TLConstructObjs ConstructObjs -- ^ External object constructor calls
      | TLAssign Assign -- ^ Top level assignment
      | TLObjs Objects  -- ^ External objects

       deriving (Show, Eq)

-- | Objects
data Objects = Objects {
   nameSpace :: [Ident],
   objVar :: Var 
} deriving (Show, Eq)

-- | Variable
data Var = 
          VarArrayElem Ident Expr  -- ^ Element of Array
        | VarIdent Ident -- ^ Ordinary identifier
         deriving (Eq)

instance Show Var where
    show (VarArrayElem (Ident i) expr) = i ++ show expr
    show (VarIdent (Ident i)) = i

-- | Constructing Objects
data ConstructObjs = ConstructObjs [Ident] Var [Expr] deriving (Show, Eq)

-- | Statement
data Stmt = 
        AssignStmt Assign-- ^ Type Name, assignment
      | Seq BlockStmt -- ^ Evaluate statements in sequential order
      | BStmt BlockStmt  -- ^ Statements in enclosed block
      | FunCallStmt FunCall -- ^ Call Funcion
      | MethodStmt MethodCall -- ^ Call Object method
      | If Expr Stmt  -- ^ If statement
      | IfElse Expr Stmt Stmt -- ^ If Else statement
      | Return Expr -- ^ Return value from current function
      | ForLoop Ident Expr Expr Expr BlockStmt -- ^ Start, Stop, Step, statements, static for loops
       deriving (Show, Eq)

isReturn :: Stmt -> Bool
isReturn (Return e) = True
isReturn _ = False

data Assign = Assign Ident Expr  deriving (Show, Eq) -- ^ Variable assignment
data FunCall = FunCall Ident [Expr] deriving (Show, Eq) -- ^ Function call layout
data MethodCall = MethodCall { 
      mVar :: Var, 
      mName :: Ident, 
      mArgs ::  [Expr] 
} deriving (Show, Eq) -- ^ Method call layout

-- | Expression
data Expr =
      ExpBinOp BinOps Expr Expr -- ^ Binary operation with 2 sub-expressions
    | ExpUnaryOp UnaryOps Expr -- ^ Unary operation with sub-expression
    | ExpFunCall FunCall  -- ^ Function Call
    | ExpMethodCall MethodCall -- ^ C++ Object method call
    | ExpIdent Ident -- ^ Identifier  
    | ExpLit Literal -- ^ Constant/Literal value
     deriving (Show, Eq)


-- | Binary Operators
data BinOps =
      Add  -- ^ Addition
    | Sub  -- ^ Subtraction
    | Mul  -- ^ Multiplication
    | Div  -- ^ Division
    | And  -- ^ Boolean AND
    | Or   -- ^ Boolean OR
    | Mod  -- ^ Modulo
    | Less  -- ^ Less than 
    | LessEq  -- ^ Less than or equal to
    | Greater  -- ^ Greater than
    | GreaterEq  -- ^ Greather than or equal to
    | Equals  -- ^ Equals
    | NEquals  -- ^ Not Equal
    | ShiftL  -- ^ Logical Shift Left
    | ShiftR  -- ^ Logical Shift Right
    | BAnd  -- ^ Bitwise AND
    | BXor  -- ^ Bitwise XOR
    | BOr  -- ^ Bitwise OR
     deriving (Show, Eq)


-- | Unary Operators
data UnaryOps =
      Not -- ^ Boolean NOT
    | Neg  -- ^ Number negation
    | BNot -- ^ Bitwise NOT
     deriving (Show, Eq)

-- | Literal/Constants
data Literal  =
      Str String -- ^ String
    | Ch Char -- ^ Char
    | Number (Either Integer Double) -- ^ Numbers, either Int/Double
    | Bl Bool -- Boolean
     deriving (Eq, Ord)

instance Show Literal where
    show (Str s) = s
    show (Ch c) = show c
    show (Number (Left i)) = show i
    show (Number (Right d)) = show d
    show (Bl b) = show b
 

-- | C++ Library
type LibName = Ident

-- | C++ Class Name  
type ClassName = Ident

-- | Identifier
data Ident = Ident String deriving (Eq, Ord)

instance Show (Ident) where
    show (Ident s) = show s

-- | Block of Statements
data BlockStmt = BlockStmt [Stmt] deriving (Show, Eq)
