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
    (BlockStmt
      (ReturnStmt
        (AddExpr s32
          (DerefExpr s32
            (IdExpr *const s32 "a"
              sym: (Symbol "a" local const (Value *const s32)))
            #nullptr#)
          (DerefExpr s32
            (IdExpr *const s32 "b"
              sym: (Symbol "b" local const (Value *const s32)))
            #nullptr#)))))
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
  (FuncDecl "noice"
    sym: (Symbol "noice" const (Value func() (s32, s32)))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    (BlockStmt
      (ReturnStmt
        (IntExpr s32 0)
        (IntExpr s32 1))))
  (DefDecl "C"
    sym: (Symbol "C" const (Value comptime_int 10))
    #nullptr#
    (IntExpr comptime_int 10))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (MultiVarStmt
        names:
        (a
          sym: (Symbol "a" local (Value s32)))
        (b
          sym: (Symbol "b" local (Value s32)))
        (c
          sym: (Symbol "c" local (Value s64)))
        inits:
        (CallExpr (s32, s32)
          (IdExpr func() (s32, s32) "noice"
            sym: (Symbol "noice" const (Value func() (s32, s32)))))
        (AddExpr comptime_int
          (IntExpr s64 10)
          (IdExpr s64 "C"
            sym: (Symbol "C" const (Value comptime_int 10)))))
      (VarStmt "x"
        sym: (Symbol "x" local (Value s32))
        #nullptr#
        (AddExpr s32
          (IntExpr s32 10)
          (IdExpr s32 "a"
            sym: (Symbol "a" local (Value s32)))))
      (VarStmt "y"
        sym: (Symbol "y" local (Value s32))
        #nullptr#
        (AddExpr s32
          (IntExpr s32 20)
          (IdExpr s32 "b"
            sym: (Symbol "b" local (Value s32)))))
      (VarStmt "z"
        sym: (Symbol "z" local (Value s64))
        #nullptr#
        (AddExpr s64
          (IntExpr s64 30)
          (IdExpr s64 "c"
            sym: (Symbol "c" local (Value s64)))))
      (VarStmt "z"
        sym: (Symbol "z" local (Value s32))
        #nullptr#
        (CallExpr s32
          (IdExpr func(*const s32, *const s32) s32 "add_ptrs"
            sym: (Symbol "add_ptrs" const (Value func(*const s32, *const s32) s32)))
          (RefExpr *s32
            (IdExpr s32 "x"
              sym: (Symbol "x" local (Value s32)))
            #nullptr#)
          (RefExpr *s32
            (IdExpr s32 "y"
              sym: (Symbol "y" local (Value s32)))
            #nullptr#)))
      (VarStmt "w"
        sym: (Symbol "w" local (Value s32))
        #nullptr#
        (AddExpr s32
          (AddExpr s32
            (IdExpr s32 "a"
              sym: (Symbol "a" local (Value s32)))
            (IdExpr s32 "b"
              sym: (Symbol "b" local (Value s32))))
          (CastExpr
            (IdExpr "_")
            (IdExpr s64 "c"
              sym: (Symbol "c" local (Value s64)))))))))
