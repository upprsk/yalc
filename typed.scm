(FlatModule "main"
  (FuncDecl "getchar"
    sym: (Symbol "getchar" const extern (Value func() s32))
    (Attribute "extern")
    (FuncRet s32 ""
      (IdExpr type "s32"
        sym: (Symbol "s32" const (Value type s32))))
    #nullptr#)
  (DefDecl "string"
    sym: (Symbol "string" const distinct (Value type string (distinct of []const u8)))
    (Attribute "distinct")
    #nullptr#
    (SliceExpr type const
      (IdExpr type "u8"
        sym: (Symbol "u8" const (Value type u8)))))
  (FuncDecl "test_slice"
    sym: (Symbol "test_slice" const (Value func([]const u8)))
    (FuncParam []const u8 "s"
      sym: (Symbol "s" local const (Value []const u8))
      (SliceExpr type const
        (IdExpr type "u8"
          sym: (Symbol "u8" const (Value type u8)))))
    (BlockStmt))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (VarStmt "string_var"
        sym: (Symbol "string_var" local (Value string (distinct of []const u8)))
        (IdExpr type "string"
          sym: (Symbol "string" const distinct (Value type string (distinct of []const u8))))
        #nullptr#)
      (VarStmt "slice_var"
        sym: (Symbol "slice_var" local (Value []const u8))
        (SliceExpr type const
          (IdExpr type "u8"
            sym: (Symbol "u8" const (Value type u8))))
        #nullptr#)
      (VarStmt "hey"
        sym: (Symbol "hey" local (Value string (distinct of []const u8)))
        (CallExpr void
          (IdExpr func(#error#) "typeof"
            sym: (Symbol "typeof" const (Value func(#error#))))
          (IdExpr string (distinct of []const u8) "string_var"
            sym: (Symbol "string_var" local (Value string (distinct of []const u8)))))
        #nullptr#)
      (ExprStmt
        (CallExpr void
          (IdExpr func(string (distinct of []const u8)) "test_string"
            sym: (Symbol "test_string" const (Value func(string (distinct of []const u8)))))
          (IdExpr string (distinct of []const u8) "hey"
            sym: (Symbol "hey" local (Value string (distinct of []const u8))))))
      (ExprStmt
        (CallExpr void
          (IdExpr func([]const u8) "test_slice"
            sym: (Symbol "test_slice" const (Value func([]const u8))))
          (IdExpr string (distinct of []const u8) "hey"
            sym: (Symbol "hey" local (Value string (distinct of []const u8))))))))
  (FuncDecl "test_string"
    sym: (Symbol "test_string" const (Value func(string (distinct of []const u8))))
    (FuncParam string (distinct of []const u8) "s"
      sym: (Symbol "s" local const (Value string (distinct of []const u8)))
      (IdExpr type "string"
        sym: (Symbol "string" const distinct (Value type string (distinct of []const u8)))))
    (BlockStmt)))
