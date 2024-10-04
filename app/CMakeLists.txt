add_executable(yalc
  main.c
  args.c
  args.h
)

target_link_libraries(yalc PUBLIC yal)
