(FlatModule "main"
  (DefDecl "KB"
    sym: (Symbol "KB" const (Value comptime_int 1024))
    #nullptr#
    (IntExpr comptime_int 1024))
  (DefDecl "Counter"
    sym: (Symbol "Counter" const (Value type Counter (struct {start: s32, end: s32})))
    #nullptr#
    (StructExpr type
       (type: Counter (struct {start: s32, end: s32}))
      (StructField s32 "start"
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))
        (IntExpr s32 0)
        init_value: (Value s32 0))
      (StructField s32 "end"
        (IdExpr type "s32"
          sym: (Symbol "s32" const (Value type s32)))
        #nullptr#)))
  (FuncDecl "main"
    sym: (Symbol "main" const (Value func()))
    (BlockStmt
      (DefStmt "Local_Struct"
        sym: (Symbol "Local_Struct" const (Value type Local_Struct (struct {a: s32, b: struct { ... }})))
        #nullptr#
        (StructExpr type
           (type: Local_Struct (struct {a: s32, b: struct { ... }}))
          (StructField s32 "a"
            (IdExpr type "s32"
              sym: (Symbol "s32" const (Value type s32)))
            #nullptr#)
          (StructField struct {ls: s32} "b"
            (StructExpr type
               (type: struct {ls: s32})
              (StructField s32 "ls"
                (IdExpr type "s32"
                  sym: (Symbol "s32" const (Value type s32)))
                #nullptr#))
            #nullptr#)))))
  (DefDecl "BUFSZ"
    sym: (Symbol "BUFSZ" const (Value comptime_int 1024))
    #nullptr#
    (IdExpr comptime_int "KB"
      sym: (Symbol "KB" const (Value comptime_int 1024)))))
