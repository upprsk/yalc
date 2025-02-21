# Yet Another Language

Simple hello world example using libc:

```yal
extern func printf(s: [*]const u8, ...);

func main() i32 {
    printf("Hello, World!\n");

    return 0;
}
```

## Variables and constants

- `var` declares a mutable variable.
- `def` defines a constant **compile-time know** value.

```yal
// a global variable, by default i32
var global = 12;

// a global variable with type annotation
var global_64: u64 = 420;

// a global constant, by default **untyped integer** (coerces to all other
// integers that fit the value).
def FOUR = 4;

// a global constant with type annotation
def WORD: u32 = 0xFFF_FFFF;

// type aliasing
def Int = i32;
def IpAddress4 = [4]u8;
```

## Functions

```yal
// function that adds two integers and returns.
func add(a: i32, b: i32) i32 {
    return a + b;
}

func main() i32 {
    // call a function
    var a = add(1, 1);

    // call a function at compile-time
    def b = add(1, 2);

    return 0;
}
```

### Bound functions

Go-style "methods". These are just normal functions where the first parameter
is the receiver. This just allows the use dot-syntax for accessing the function
like a method. Bound functions can be added to any type, even primitives.

```yal
func (c: i32) is_zero() bool {
    return c == 0;
}

func (c: *i32) inc() {
    c.* += 1;
}

func main() {
    var i = 0;
    if i.is_zero() {
        printf("is zero!\n");
        i.inc();
    }

    printf("i=%d\n", i);
}
```

### Generic functions

```yal
// Function that adds any two values of the same type and returns the sum.
func add[T](a: T, b: T) T {
    return a + b;
}

func main() i32 {
    // Type T is inferred as i32 because of the arguments.
    var a = add(1, 1);

    // Type T is inferred as u32 because of the cast of first argument.
    var b = add(1 as u32, 1);

    // Type T is set to u8 explicitly
    var c = add[u8](1, 1);

    return 0;
}
```

## Structs

Structs are just data. There are no classes. There is no inheritance. We do have
bound functions, because they are just way too convenient.

```yal
def Counter = struct {
    count: usize = 0,
    total: usize,
};

func (c: *Counter) next() ?usize {
    var v = c.count;
    if v == c.total { return nil; }

    c.count += 1;
    return v;
}

func main() i32 {
    var cnt: Counter = .{ .total = 10 };
    while var i = cnt.next(); i != nil {
        printf("i=%zu\n", i.*);
    }

    return 0;
}
```

## Pointers and Arrays

- `*T`: single item pointer.
- `*const T`: single item pointer to constant.
- `[*]T`: multi item pointer.
- `[*]const T`: multi item pointer to constant.
- `[]T`: slice (fat-pointer). Contains pointer and length, bounds checked at
  runtime.
- `[]const T`: slice (fat-pointer) to constant. Contains pointer and length,
  bounds checked at runtime.
- `[N]T`: array of `N` items.
- `[N]const T`: array of `N` constants.

```yal
func main() i32 {
    var a  = 10;              // i32
    var pa = &a;              // *i32
    var pca: *const i32 = &a; // *const i32

    var b = [2]i32{0, 1}; // [2]i32
    var pb = b.ptr;       // [*]i32

    // inferred size: [10]i32
    var c = [_]i32{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    // array of constants
    def d = [3]i32{1, 2, 3}; // [3]const i32
    var pd = d.ptr;          // [*]const i32

    // slice an array from the first element up to the last element
    // (non-inclusive).
    var e = c[1:-1];
    // slice the first 2 elements
    var f = c[:2];
    // slice the last 2 elements
    var g = c[-2:];

    return 0;
}
```

## Optional

Any type can be marked as optional, so that it may be `nil` and represent the
lack of a value. In order to access the value inside an optional, it must
be unwrapped by dereferencing.

```yal
func print_maybe_int(v: ?i32) {
    if v != nil {
        printf("got int: %d\n", v.*);
    } else {
        printf("no int\n");
    }
}

func main() {
    var maybe_int: ?i32 = 12;
    print_maybe_int(maybe_int); // got int: 12

    maybe_int = nil;
    print_maybe_int(maybe_int); // no int

    print_maybe_int(42);  // got int: 42
    print_maybe_int(nil); // no int
}
```

Optional pointers are a safe alternative (with no overhead) to null pointers. A
non-optional pointer can not be null.

```yal
func print_maybe_ptr(v: ?*i32) {
    if v != nil {
        // 2 derefs, one for optional and one for pointer
        printf("got int: %d@%p\n", v.*.*, v.*);
    } else {
        printf("no int\n");
    }
}

func main() {
    var a = 42;
    print_maybe_ptr(&a);  // got int 42@0x...

    var b: ?*i32 = &a;
    print_maybe_ptr(b);   // got int 42@0x...

    b = nil;
    print_maybe_ptr(b);   // no int
    print_maybe_ptr(nil); // no int
}
```

## Loops

While loops can be constructed with a declaration, that is called on every
iteration. This is very useful for iterators.

```yal
func main() {
    var it = new_iterator();
    while var i = it.next(); i != nil {
        printf("i=%d\n", i.*);
    }
}
```

For loops are for natively iterable things, like arrays and slices. Multi-item
pointers can not be iterated this way because they don't have a defined length.

```yal
func main() {
    var arr = [_]i32{1, 2};
    for item, idx in arr {
        // item is i32 and idx is usize
        printf("arr[%zu]=%d\n", idx, item);
    }
}
```
