(FlatModule "main"
  (FuncDecl "getchar"
    sym: (Symbol "getchar" const (Value func() s32))
    (Attribute "extern")
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    #nullptr#)
  (FuncDecl "discard_until_eof"
    sym: (Symbol "discard_until_eof" const (Value func()))
    (BlockStmt
      (VarStmt "c"
        sym: (Symbol "c" local (Value s32))
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))
        #nullptr#)
      (WhileStmt
        (GreaterExpr bool
          (IdExpr s32 "c"
            sym: (Symbol "c" local (Value s32)))
          (IntExpr s32 0))
        (BlockStmt
          (AssignStmt
            (IdExpr s32 "c"
              sym: (Symbol "c" local (Value s32)))
            (CallExpr s32
              (IdExpr func() s32 "getchar"
                sym: (Symbol "getchar" const (Value func() s32)))))))))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (ExprStmt
        (CallExpr void
          (IdExpr func() "discard_until_eof"
            sym: (Symbol "discard_until_eof" const (Value func()))))))))
