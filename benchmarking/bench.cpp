#include <cstddef>
#include <iostream>
#include <memory>
#include <span>
#include <vector>

using namespace std::string_view_literals;

auto partition(std::span<int> a, int low, int high) -> int {
    auto pivot = a[high];
    auto i = low - 1;

    for (auto j = low; j < high; j++) {
        if (a[j] <= pivot) {
            i++;
            std::swap(a[i], a[j]);
        }
    }

    std::swap(a[i + 1], a[high]);
    return i + 1;
}

void quicksort(std::span<int> a, int low, int high) {
    if (low < high) {
        int pivot = partition(a, low, high);
        quicksort(a, low, pivot - 1);
        quicksort(a, pivot + 1, high);
    }
}

void print_items(std::span<int> items) {
    for (auto item : items) {
        std::cout << item << ", ";
    }

    std::cout << "\n";
}

auto read_values(char const* path, int count) -> std::vector<int> {
    auto f = std::unique_ptr<FILE, void (*)(FILE*)>{fopen(path, "rb"),
                                                    [](FILE* f) { fclose(f); }};
    if (f == nullptr) return {};

    std::vector<int> buffer;
    buffer.resize(count);

    int r = fread(buffer.data(), sizeof(int), count, f.get());
    if (r != count) {
        return {};
    }

    return buffer;
}

void gen_file(char const* path, int n) {
    auto f = std::unique_ptr<FILE, void (*)(FILE*)>{fopen(path, "wb"),
                                                    [](FILE* f) { fclose(f); }};

    for (int i = 0; i < n; i++) {
        int v = rand() % n;
        fwrite(&v, sizeof(int), 1, f.get());
    }
}

struct Args {
    char const* input_file_path;
    int         mode;
};

void print_help(char const* self) {
    std::cout << "usage: " << self << " <mode> [args]\n";
    std::cout << "    options:\n";
    std::cout << "        -gen <count>: generate `count` numbers\n";
    std::cout << "        -sort <count>: sort `count` numbers\n";
}

auto main(int argc, char** argv_) -> int {
    auto argv = std::span{argv_, static_cast<std::size_t>(argc)};
    auto self = argv[0];
    Args args = {.input_file_path = "numbers.bin", .mode = 0};

    argv = argv.subspan(1);
    if (argv.empty()) {
        std::cout << "error: missing arguments, expected mode\n";
        print_help(self);
        return 1;
    }

    char* mode = argv[0];
    if (mode == "-gen"sv) {
        args.mode = 1;
    } else if (mode == "-sort"sv) {
        args.mode = 2;
    } else {
        std::cout << "error: invalid mode: " << mode << "\n";
        print_help(self);
        return 1;
    }

    argv = argv.subspan(1);
    if (argv.empty()) {
        std::cout << "error: missing arguments, expected count after " << mode
                  << "\n";
        print_help(self);
        return 1;
    }

    char* count_s = argv[0];
    int   count = atoi(count_s);
    if (count == 0) {
        std::cout << "error: invalid count: " << count_s << "\n";
        return 1;
    }

    if (args.mode == 1) {
        gen_file(args.input_file_path, count);
        return 0;
    }

    auto items = read_values(args.input_file_path, count);
    print_items(items);

    quicksort(items, 0, count - 1);
    print_items(items);
}
