use std::io::Write;
use std::io::Read;

fn partition(a: &mut [i32], low: i32, high: i32) -> i32 {
    let pivot = a[high as usize];
    let mut i = low - 1;

    for j in low..high {
        if a[j as usize] <= pivot {
            i += 1;
            a.swap(i as usize, j as usize);
        }
    }

    a.swap((i+1) as usize, high as usize);
    i + 1
}

fn quicksort(a: &mut [i32], low: i32, high: i32) {
    if low < high {
        let pivot = partition(a, low, high);
        quicksort(a, low, pivot-1);
        quicksort(a, pivot+1, high);
    }
}

fn read_values(path: &str, n: usize) -> Vec<i32> {
    let mut file = std::fs::File::open(path).expect("failed to open file");
    let mut data: Vec<i32> = Vec::new();
    for _ in 0..n {
        let mut buf = [0; 4];
        file.read(&mut buf).expect("failed to read");

        data.push(i32::from_ne_bytes(buf));
    }

    data
}

fn gen_file(path: &str, n: usize) {
    let mut file = std::fs::File::create(path).expect("failed to create file");
    for i in 0..n {
        // no random...
        let v = (n - i) as i32;
        file.write_all(&v.to_ne_bytes()).expect("failed to write");
    }
}

struct Args<'a> {
    input_file_path: &'a str,
    mode: i32,
}

fn print_help(self_path: &str) {
    println!("usage: {self_path} <mode> [args]");
    println!("    options:");
    println!("        -gen <count>: generate `count` numbers");
    println!("        -sort <count>: sort `count` numbers");
}


fn main() {
    let mut argv = std::env::args();
    let self_path = argv.next().unwrap();

    let mut args = Args{input_file_path: "numbers.bin", mode: 0};

    let mode = argv.next();
    if mode.is_none() {
        println!("error: missing arguments, expected mode");
        print_help(&self_path);
        std::process::exit(1);
    }

    let mode = mode.unwrap();
    if mode == "-gen" {
        args.mode = 1;
    } else if mode == "-sort" {
        args.mode = 2;
    } else {
        println!("error: invalid mode: {mode}");
        print_help(&self_path);
        std::process::exit(1);
    }

    let count_s = argv.next();
    if count_s.is_none() {
        println!("error: missing arguments, expected count after {mode}");
        print_help(&self_path);
        std::process::exit(1);
    }

    let count_s = count_s.unwrap();
    let count: i32 = match count_s.parse() {
        Ok(count) => count,
        Err(e) => {
            println!("error: invalid count '{count_s}': {e}");
            print_help(&self_path);
            std::process::exit(1);
        },
    };

    if count == 0 {
        println!("error: invalid count: {count_s}");
        std::process::exit(1);
    }

    if args.mode == 1 {
        gen_file(args.input_file_path, count as usize);
        std::process::exit(0);
    }

    let mut items = read_values(args.input_file_path, count as usize);
    println!("{items:?}");

    let len = items.len() as i32 - 1;
    quicksort(&mut items, 0, len);
    println!("{items:?}");
}
