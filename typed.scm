(FlatModule "main"
  (DefDecl "KB"
    sym: (Symbol "KB" const (Value comptime_int 1024))
    #nullptr#
    (IntExpr comptime_int 1024))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (IfStmt
        (IdExpr "this_is_some_expr")
        (BlockStmt)
        #nullptr#)))
  (FuncDecl "another_func"
    sym: (Symbol "another_func" const (Value func()))
    (BlockStmt))
  (DefDecl "BUFSZ"
    sym: (Symbol "BUFSZ" const (Value comptime_int 1024))
    #nullptr#
    (IdExpr comptime_int "KB"
      sym: (Symbol "KB" const (Value comptime_int 1024)))))
