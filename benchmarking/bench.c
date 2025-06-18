#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void swap(int* l, int* r) {
    int tmp = *l;
    *l = *r;
    *r = tmp;
}

int partition(int* a, int low, int high) {
    int pivot = a[high];
    int i = low-1;

    for (int j = low; j < high; j++) {
        if (a[j] <= pivot) {
            i++;
            swap(&a[i], &a[j]);
        }
    }

    swap(&a[i+1], &a[high]);
    return i + 1;
}

void quicksort(int* a, int low, int high) {
    if (low < high) {
        int pivot = partition(a, low, high);
        quicksort(a, low, pivot-1);
        quicksort(a, pivot+1, high);
    }
}

void print_items(int* items, int count) {
    for (int i = 0; i < count; i++) {
        printf("%d, ", items[i]);
    }

    printf("\n");
}

int* read_values(char const* path, int count) {
    FILE* f = fopen(path, "rb");
    if (f == NULL) return NULL;

    int* buffer = malloc(sizeof(int) * count);

    int r = fread(buffer, sizeof(int), count, f);
    if (r != count) {
        fclose(f);
        free(buffer);
        return NULL;
    }

    fclose(f);
    return buffer;
}

void gen_file(char const* path, int n) {
    FILE* f = fopen(path, "wb");

    for (int i = 0; i < n; i++) {
        int v = rand() % n;
        fwrite(&v, sizeof(int), 1, f);
    }

    fclose(f);
}

typedef struct {
    char const* input_file_path;
    int mode;
} Args;

void print_help(char const* self) {
    printf("usage: %s <mode> [args]\n", self);
    printf("    options:\n");
    printf("        -gen <count>: generate `count` numbers\n");
    printf("        -sort <count>: sort `count` numbers\n");
}

int main(int argc, char** argv) {
    char* self = *argv;
    Args args = { .input_file_path = "numbers.bin", .mode = 0 };

    argv++;
    argc--;
    if (argc == 0) {
        printf("error: missing arguments, expected mode\n");
        print_help(self);
        return 1;
    }

    char* mode = *argv;
    if (strcmp(mode, "-gen") == 0) {
        args.mode = 1;
    } else if (strcmp(mode, "-sort") == 0) {
        args.mode = 2;
    } else {
        printf("error: invalid mode: %s\n", mode);
        print_help(self);
        return 1;
    }

    argv++;
    argc--;
    if (argc == 0) {
        printf("error: missing arguments, expected count after %s\n", mode);
        print_help(self);
        return 1;
    }

    char* count_s = *argv;
    int count = atoi(count_s);
    if (count == 0) {
        printf("error: invalid count: %s\n", count_s);
        return 1;
    }

    if (args.mode == 1) {
        gen_file("./numbers.bin", count);
        return 0;
    }

    int* items = read_values("./numbers.bin", count);
    if (items == NULL) count = 0;

    print_items(items, count);

    quicksort(items, 0, count-1);
    print_items(items, count);
}
