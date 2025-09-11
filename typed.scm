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
          sym: (Symbol "KB" const (Value comptime_int 1024)))))))
