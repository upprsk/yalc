(FlatModule "main"
  (FuncDecl "getchar"
    sym: (Symbol "getchar" const (Value func() s32))
    (Attribute "extern")
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    #nullptr#)
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (VarStmt "i"
        sym: (Symbol "i" local (Value s64))
        #nullptr#
        (NegExpr s64
          (IntExpr s64 1)
          #nullptr#)))))
