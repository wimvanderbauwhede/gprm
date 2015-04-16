
module GPC.SimplifyAST(simplifyAST) where


import qualified GPC.AST as T
import qualified GPC.TypelessAST as S

simplifyAST :: T.Program a -> S.Program
simplifyAST (T.Program tls) = S.Program $ map simplifyTLS tls

simplifyTLS :: T.TopLevel a -> S.TopLevel
simplifyTLS (T.Func _ (tname,fname) args bstmt) = 
    S.Func (simplifyIdent tname) (simplifyIdent fname) (map (simplifyIdent. snd) args) (simplifyBStmt bstmt)
    
simplifyTLS (T.TLConstructObjs (T.ConstructObjs ids vars exprs)) = 
    S.TLConstructObjs (S.ConstructObjs (map simplifyIdent ids) (simplifyVar vars) 
                      (map simplifyExpr exprs))

simplifyTLS (T.TLAssign a) = S.TLAssign $ simplifyAssign a

simplifyTLS (T.TLObjs (T.Objects ns var)) = S.TLObjs (S.Objects (map simplifyIdent ns) (simplifyVar var))

simplifyBStmt :: T.BlockStmt a -> S.BlockStmt
simplifyBStmt (T.BlockStmt stmts) = S.BlockStmt (map simplifyStmt stmts)

simplifyStmt :: T.Stmt a -> S.Stmt
simplifyStmt (T.AssignStmt a) = S.AssignStmt $ simplifyAssign a
simplifyStmt (T.Seq bstmt) = S.Seq $ simplifyBStmt bstmt
simplifyStmt (T.BStmt bstmt) = S.BStmt $ simplifyBStmt bstmt
simplifyStmt (T.FunCallStmt fc) = S.FunCallStmt $ simplifyFunCall fc
simplifyStmt (T.MethodStmt  mc) = S.MethodStmt  $ simplifyMethodCall mc
simplifyStmt (T.If expr stmt) = S.If (simplifyExpr expr) (simplifyStmt stmt)
simplifyStmt (T.IfElse expr s1 s2) = S.IfElse (simplifyExpr expr) (simplifyStmt s1)
                                              (simplifyStmt s2)
simplifyStmt (T.Return expr) = S.Return $ simplifyExpr expr
simplifyStmt (T.ForLoop var e1 e2 e3 bStmt) = 
    S.ForLoop (simplifyIdent var) (simplifyExpr e1) (simplifyExpr e2) (simplifyExpr e3) 
              (simplifyBStmt bStmt)

simplifyExpr :: T.Expr a -> S.Expr
simplifyExpr (T.ExpBinOp bop e1 e2) = S.ExpBinOp (simplifyBinOp bop) (simplifyExpr e1) 
                                        (simplifyExpr e2)
simplifyExpr (T.ExpUnaryOp unOp e) = S.ExpUnaryOp (simplifyUnOp unOp) (simplifyExpr e)
simplifyExpr (T.ExpFunCall fc) = S.ExpFunCall $ simplifyFunCall fc
simplifyExpr (T.ExpMethodCall mc) = S.ExpMethodCall $ simplifyMethodCall mc
simplifyExpr (T.ExpIdent name) = S.ExpIdent $ simplifyIdent name
simplifyExpr (T.ExpLit l) = S.ExpLit $ simplifyLit l

simplifyAssign :: T.Assign a -> S.Assign
simplifyAssign (T.Assign _ name expr) = S.Assign (simplifyIdent name) (simplifyExpr expr)

simplifyFunCall :: T.FunCall a -> S.FunCall
simplifyFunCall (T.FunCall name exprs) = S.FunCall (simplifyIdent name) (map simplifyExpr exprs)

simplifyMethodCall :: T.MethodCall a -> S.MethodCall
simplifyMethodCall (T.MethodCall var name exprs) = S.MethodCall (simplifyVar var) (simplifyIdent name)
                                                              (map simplifyExpr exprs) 
simplifyBinOp :: T.BinOps a -> S.BinOps
simplifyBinOp bop = case bop of
    T.Add _       -> S.Add
    T.Sub _       -> S.Sub
    T.Mul _       -> S.Mul
    T.Div _       -> S.Div
    T.And _       -> S.And
    T.Or  _       -> S.Or
    T.Mod _       -> S.Mod
    T.Less _      -> S.Less
    T.LessEq _    -> S.LessEq
    T.Greater _   -> S.Greater
    T.GreaterEq _ -> S.GreaterEq
    T.Equals _    -> S.Equals
    T.NEquals _   -> S.NEquals
    T.ShiftL _    -> S.ShiftL
    T.ShiftR _    -> S.ShiftR
    T.BAnd _      -> S.BAnd
    T.BXor _      -> S.BXor
    T.BOr _       -> S.BOr

simplifyUnOp :: T.UnaryOps a -> S.UnaryOps
simplifyUnOp op = case op of
    T.Not  _ -> S.Not
    T.Neg  _ -> S.Neg
    T.BNot _ -> S.BNot

simplifyVar :: T.Var a -> S.Var
simplifyVar v = case v of
    T.VarArrayElem name expr -> S.VarArrayElem (simplifyIdent name) (simplifyExpr expr)
    T.VarIdent name -> S.VarIdent (simplifyIdent name)

simplifyIdent :: T.Ident a -> S.Ident
simplifyIdent (T.Ident _ s) = S.Ident s

simplifyLit :: T.Literal a -> S.Literal
simplifyLit l = case l of
    T.Str _ s    -> S.Str s
    T.Ch _ c     -> S.Ch  c
    T.Number _ n -> S.Number n
    T.Bl _ b     -> S.Bl b


