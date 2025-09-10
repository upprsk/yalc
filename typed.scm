(FlatModule "main"
  (FuncDecl "add_ptrs"
    sym: (Symbol "add_ptrs" const (Value func(*const s32, *const s32) s32))
    (FuncParam *const s32 "a"
      sym: (Symbol "a" local const (Value *const s32))
      (PtrExpr type const
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))))
    (FuncParam *const s32 "b"
      sym: (Symbol "b" local const (Value *const s32))
      (PtrExpr type const
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    (BlockStmt))
  (FuncDecl "add_all"
    sym: (Symbol "add_all" const (Value func([]const s32) s32))
    (FuncParam []const s32 "s"
      sym: (Symbol "s" local const (Value []const s32))
      (SliceExpr type const
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    (BlockStmt))
  (DefDecl "B"
    sym: (Symbol "B" const (Value s32 10))
    (IdExpr type "s32"
      sym: (Symbol "s32" const (Value type s32)))
    (IntExpr s32 10))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (VarStmt "a"
        sym: (Symbol "a" local (Value s32))
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))
        (IntExpr s32 10))
      (VarStmt "a_ptr"
        sym: (Symbol "a_ptr" local (Value *s32))
        #nullptr#
        (RefExpr *s32
          (IdExpr s32 "a"
            sym: (Symbol "a" local (Value s32)))
          #nullptr#))
      (VarStmt "b_ptr"
        sym: (Symbol "b_ptr" local (Value *const s32))
        #nullptr#
        (RefExpr *const s32
          (IdExpr s32 "B"
            sym: (Symbol "B" const (Value s32 10)))
          #nullptr#))
      (VarStmt "x"
        sym: (Symbol "x" local (Value s32))
        #nullptr#
        (DerefExpr s32
          (IdExpr *s32 "a_ptr"
            sym: (Symbol "a_ptr" local (Value *s32)))
          #nullptr#))
      (VarStmt "y"
        sym: (Symbol "y" local (Value s32))
        #nullptr#
        (DerefExpr s32
          (IdExpr *const s32 "b_ptr"
            sym: (Symbol "b_ptr" local (Value *const s32)))
          #nullptr#))
      (VarStmt "_"
        #nullptr#
        (AddExpr s32
          (IdExpr s32 "x"
            sym: (Symbol "x" local (Value s32)))
          (IdExpr s32 "y"
            sym: (Symbol "y" local (Value s32))))))))
