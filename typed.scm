(FlatModule "main"
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (DefStmt "KB"
        sym: (Symbol "KB" const (Value comptime_int 1024))
        #nullptr#
        (IntExpr comptime_int 1024))
      (DefStmt "BUFSZ"
        sym: (Symbol "BUFSZ" const (Value comptime_int 1024))
        #nullptr#
        (IdExpr comptime_int "KB"
          sym: (Symbol "KB" const (Value comptime_int 1024))))
      (VarStmt "buf"
        sym: (Symbol "buf" local (Value [1024]u8))
        (ArrayExpr type
          (IdExpr comptime_int "BUFSZ"
            sym: (Symbol "BUFSZ" const (Value comptime_int 1024)))
          (IdExpr type "u8"
            sym: (Symbol "u8" const (Value type u8))))
        #nullptr#)
      (VarStmt "buf_0"
        sym: (Symbol "buf_0" local (Value u8))
        #nullptr#
        (IndexExpr u8
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          (IntExpr comptime_int 0)
          #nullptr#))
      (VarStmt "buf_1"
        sym: (Symbol "buf_1" local (Value []u8))
        #nullptr#
        (IndexExpr []u8 slicing
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          #nullptr#
          #nullptr#))
      (VarStmt "buf_2"
        sym: (Symbol "buf_2" local (Value []u8))
        #nullptr#
        (IndexExpr []u8 slicing
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          (IntExpr comptime_int 0)
          #nullptr#))
      (VarStmt "buf_3"
        sym: (Symbol "buf_3" local (Value []u8))
        #nullptr#
        (IndexExpr []u8 slicing
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          #nullptr#
          (IntExpr comptime_int 0)))
      (VarStmt "buf_4"
        sym: (Symbol "buf_4" local (Value []u8))
        #nullptr#
        (IndexExpr []u8 slicing
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          (IntExpr comptime_int 0)
          (IntExpr comptime_int 0)))
      (VarStmt "buf_5"
        sym: (Symbol "buf_5" local (Value u8))
        #nullptr#
        (IndexExpr u8
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          #nullptr#
          #nullptr#))
      (AssignStmt
        (IndexExpr u8
          (IdExpr [1024]u8 "buf"
            sym: (Symbol "buf" local (Value [1024]u8)))
          (IntExpr comptime_int 0)
          #nullptr#)
        (IntExpr u8 0)))))
