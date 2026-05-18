#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    // Check args
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    // Open file
    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    // Read header
    unsigned char header[32];
    fseek(fp, 0, SEEK_SET);
    if (fread(header, 1, 32, fp) < 32) {
        fclose(fp);
        return 1;
    }

    unsigned int header_size = header[4];
    unsigned int text_size = header[8] | (header[9] << 8) | (header[10] << 16) | (header[11] << 24);

    unsigned char *text = malloc(text_size);
    fseek(fp, header_size, SEEK_SET);
    fread(text, 1, text_size, fp);

    unsigned int pc = 0;
    while (pc < text_size) {
        if (text[pc] == 0xbb && pc + 2 < text_size) {
            printf("%04x: %02x%02x%02x        mov bx, %02x%02x\n", pc, text[pc], text[pc+1], text[pc+2], text[pc+2], text[pc+1]);
            pc += 3;
        } else if (text[pc] == 0xcd && pc + 1 < text_size) {
            printf("%04x: %02x%02x          int %02x\n", pc, text[pc], text[pc+1], text[pc+1]);
            pc += 2;
        } else if (text[pc] == 0x00 && pc + 1 < text_size && text[pc+1] == 0x00) {
            printf("%04x: %02x%02x          add [bx+si], al\n", pc, text[pc], text[pc+1]);
            pc += 2;
        } else {
            printf("%04x: %02x            ???\n", pc, text[pc]);
            pc += 1;
        }
    }

    free(text);
    fclose(fp);
    return 0;
}