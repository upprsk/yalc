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
      (WhileStmt
        (IdExpr bool "true"
          sym: (Symbol "true" const (Value bool)))
        (BlockStmt
          (VarStmt "c"
            sym: (Symbol "c" local (Value s32))
            #nullptr#
            (CallExpr s32
              (IdExpr func() s32 "getchar"
                sym: (Symbol "getchar" const (Value func() s32)))))
          (IfStmt
            (CastExpr bool
              (IdExpr "_")
              (IdExpr s32 "c"
                sym: (Symbol "c" local (Value s32))))
            (BlockStmt)
            #nullptr#))))))
