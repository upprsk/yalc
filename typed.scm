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
      (VarStmt "c"
        sym: (Symbol "c" local (Value u8))
        #nullptr#
        (CastExpr u8
          (IdExpr type "u8"
            sym: (Symbol "u8" const (Value type u8)))
          (CallExpr s32
            (IdExpr func() s32 "getchar"
              sym: (Symbol "getchar" const (Value func() s32)))))))))
