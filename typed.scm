(FlatModule "main"
  (FuncDecl "f_a"
    sym: (Symbol "f_a" (Value func()))
    (BlockStmt))
  (FuncDecl "f_b"
    sym: (Symbol "f_b" (Value func() s32))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (BlockStmt))
  (FuncDecl "f_c"
    sym: (Symbol "f_c" (Value func() (s32, u32)))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (FuncRet u32 ""
      (IdExpr type "u32"
        sym: (Symbol "u32" (Value type u32))))
    (BlockStmt
      (VarStmt "first"
        (IdExpr type "s32"
          sym: (Symbol "s32" (Value type s32)))
        #nullptr#)
      (VarStmt "second"
        (IdExpr type "u32"
          sym: (Symbol "u32" (Value type u32)))
        (IntExpr u32 10))
      (ReturnStmt
        (IdExpr s32 "first"
          sym: (Symbol "first" (Value s32)))
        (IdExpr u32 "second"
          sym: (Symbol "second" (Value u32))))))
  (FuncDecl "f"
    sym: (Symbol "f" (Value func(s32, s32) (s32, u32)))
    (FuncParam s32 "x"
      sym: (Symbol "x" (Value s32))
      #nullptr#)
    (FuncParam s32 "y"
      sym: (Symbol "y" (Value s32))
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (FuncRet u32 "thing_2"
      (IdExpr type "u32"
        sym: (Symbol "u32" (Value type u32))))
    (BlockStmt
      (ReturnStmt
        (IdExpr s32 "x"
          sym: (Symbol "x" (Value s32)))
        (CastExpr u32
          (IdExpr "_")
          (IdExpr s32 "y"
            sym: (Symbol "y" (Value s32))))))))
