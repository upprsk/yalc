(FlatModule "main"
  (VarDecl "global_var"
    sym: (Symbol "global_var" (Value s32))
    (IdExpr type "s32"
      sym: (Symbol "s32" (Value type s32)))
    #nullptr#)
  (VarDecl "global_var_2"
    sym: (Symbol "global_var_2" (Value s64))
    #nullptr#
    (IntExpr s64 0))
  (DefDecl "ZERO"
    sym: (Symbol "ZERO" (Value comptime_int 0))
    #nullptr#
    (IntExpr comptime_int 0))
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
        (AddExpr u32
          (IdExpr u32 "ZERO"
            sym: (Symbol "ZERO" (Value comptime_int 0)))
          (IntExpr u32 10)))
      (ReturnStmt
        (AddExpr s32
          (IdExpr s32 "first"
            sym: (Symbol "first" (Value s32)))
          (IdExpr s32 "ZERO"
            sym: (Symbol "ZERO" (Value comptime_int 0))))
        (IdExpr u32 "second"
          sym: (Symbol "second" (Value u32))))))
  (FuncDecl "f"
    sym: (Symbol "f" (Value func(u32, s32) (u32, s32)))
    (FuncParam u32 "x"
      sym: (Symbol "x" (Value u32))
      (IdExpr type "u32"
        sym: (Symbol "u32" (Value type u32))))
    (FuncParam s32 "y"
      sym: (Symbol "y" (Value s32))
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (FuncRet u32 ""
      (IdExpr type "u32"
        sym: (Symbol "u32" (Value type u32))))
    (FuncRet s32 "thing_2"
      (IdExpr type "s32"
        sym: (Symbol "s32" (Value type s32))))
    (BlockStmt
      (VarStmt "xy"
        (IdExpr type "u32"
          sym: (Symbol "u32" (Value type u32)))
        (AddExpr u32
          (IdExpr u32 "x"
            sym: (Symbol "x" (Value u32)))
          (CastExpr u32
            (IdExpr "_")
            (IdExpr s32 "y"
              sym: (Symbol "y" (Value s32))))))
      (ReturnStmt
        (IdExpr u32 "xy"
          sym: (Symbol "xy" (Value u32)))
        (AddExpr s32
          (IdExpr s32 "global_var"
            sym: (Symbol "global_var" (Value s32)))
          (CastExpr s32
            (IdExpr "_")
            (IdExpr s64 "global_var_2"
              sym: (Symbol "global_var_2" (Value s64)))))))))
