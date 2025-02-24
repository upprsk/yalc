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

// a global variable initialized with the zero-value for integers
var zero_global: i32;
```

### Zero values

All values are always initialized to know defaults. This removes bugs with
using uninitialized values.

- Integers and floats: the zero-value is `0`
- `bool`: the zero-value is `false`
- Enumerations: the first field
- Structs: the zero-value of every field
- Pointers: There are two variants
  - non-nil pointers: no zero value, can't be left uninitialized
  - optional pointers: `nil`

There is one thing that leaves values uninitialized: `malloc`. The memory
returned by `malloc` (and friends) is not initialized, or initialized to
literal zeros.

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
func i32.is_zero(c: i32) bool {
    return c == 0;
}

func i32.inc(c: *i32) {
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

It would be possible to define a type `Self` when in a bound function that is
set to the type it is bound to. This would reduce duplication:

```yal
func u32.is_zero(v: Self) -> bool { return v == 0; }
```

This technique could be used to override operators for non-integer values.

```yal
def S = struct { a: i32 };
func S.__eq(s: S, o: S) bool {
    return s.a == o.a;
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

> A generic call looks exactly the same as indexing. This can't be solved at
> the parser level! The typing pass will determine that this is the case when
> it detects that a function is being indexed.

## Multiple returns

Functions can return many values.

```yal
func get_data(v: i32) (i32, i32) {
    return v + 1, v + 2;
}

func main() {
    var a, b = get_data(12);
    printf("%d, %d\n", a, b); // 13, 14
}
```

Multiple return values is the main mechanism for error handling:

```yal
func try_get(n: i32) (i32, bool) {
    if n < 18 { return 0, false; }

    return n, true;
}

func main() i32 {
    var v, ok = try_get(12);
    if !ok {
        printf("failed to get\n");
        return 1;
    }

    printf("got: %d\n", v);

    return 0;
}
```

## Named return values

Named return values are a delicate feature. First tried it in [Odin](https://odin-lang.org/)
with a lot of skepticism, but the feature has a lot of value when used
correctly.

```yal
func try_get_next(n: i32) (v: i32, ok: bool) {
    if n < 10 { return; } // returns default values for i32 and bool (0, false)

    // v and ok can be assigned to in the function
    n += 1;

    return n, true; // both can be passed as well
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

func Counter.next(c: *Counter) ?usize {
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

## Enumerations

```yal
def Color = enum {
    Red,
    Blue,
    Green,
    Yellow,
};

def main() {
    var color = Color.Green;
    color = .Yellow; // infers enum type from lhs
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

For loops are for natively iterable things like arrays and slices. Multi-item
pointers can not be iterated this way because they don't have a defined length.
Multi item pointers can be sliced to give them length and safe access and
iteration.

```yal
func main() {
    var arr = [_]i32{1, 2};
    for item, idx in arr {
        // item is i32 and idx is usize
        printf("arr[%zu]=%d\n", idx, item);
    }
}
```

## Defer

`defer` is a mechanism to run a function on scope exit. This is meant to move
resource deallocation be placed closer to the allocation, so that a path is
not forgotten.

```yal
func a() bool {
    var buffer: [*]u8 = malloc(1024);
    var buffer = buffer[:1024]; // make it a slice

    if !load_from_db(buffer) { return false; } // leak!

    print_stats(buffer);
    free(buffer.ptr);

    return true;
}

func a() bool {
    var buffer: [*]u8 = malloc(1024);
    defer free(buffer);

    var buffer = buffer[:1024]; // make it a slice

    if !load_from_db(buffer) { return false; } // no leak!

    print_stats(buffer);

    return true;
}
```

## Error handling (or_return and or_else)

`or_return` allows for early returns by simply using the last value as a check.
If it is a boolean, a false value indicates error. If it is an enum, a non-zero
variant indicates error.

```yal
func read_configuration_file() ([]u8, bool) {
    // ...
}

def ParseError = enum {
    Ok,          // the zero value
    NoError = 0, // two variants can have the same backing value
    InvalidSintax,
    DuplicateKey,
    WrongData,
    Err,
};

func parse_configuration(data: []const u8) (Conf, ParseError) {
    // ...
}

func check_configuration() (err: ParseError) {
    // run the code after `or_else` in case the last return value is false
    var data = read_configuration_file() or_else return .Err;
    defer if err != .Ok {
        free(data.ptr);
    }

    // return the `err` value in case it is non-zero
    var conf = parse_configuration(data) or_return;

    // ...
}
```
