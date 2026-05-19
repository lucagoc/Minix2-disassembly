// ==========================================
// INCLUDES ET VARIABLES GLOBALES
// ==========================================
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned char *text;    // Contient les données binaires de la section text
unsigned int text_size; // Taille de la section text
unsigned int pc = 0;    // Compteur de programme (Program Counter)

// Noms des registres Intel 8086 (8 bits et 16 bits)
const char *regs8[] = {"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"};
const char *regs16[] = {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di"};

// Formats d'adressage de base (Mod = 00, 01, 10)
const char *rm_str[] = {"bx+si", "bx+di", "bp+si", "bp+di",
                        "si",    "di",    "bp",    "bx"};

// ==========================================
// FONCTION : DÉCODAGE MODR/M
// ==========================================
// Cette fonction lit l'octet ModR/M et génère la chaîne de caractères
// représentant l'opérande (registre ou mémoire avec déplacement).
void decode_rm(int mod, int rm, int w, char *buf, unsigned int *bytes_read) {
  if (mod == 3) {
    strcpy(buf, w ? regs16[rm] : regs8[rm]);
    return;
  }
  char inner[64] = "";
  if (mod == 0 && rm == 6) {
    if (pc + 1 >= text_size)
      return;
    unsigned int disp = text[pc] | (text[pc + 1] << 8);
    pc += 2;
    *bytes_read += 2;
    sprintf(inner, "%04x", disp);
  } else {
    strcpy(inner, rm_str[rm]);
    if (mod == 1) {
      if (pc >= text_size)
        return;
      char disp = text[pc++];
      *bytes_read += 1;
      if (disp < 0)
        sprintf(inner + strlen(inner), "-%x", -disp);
      else if (disp > 0)
        sprintf(inner + strlen(inner), "+%x", disp);
    } else if (mod == 2) {
      if (pc + 1 >= text_size)
        return;
      short disp = text[pc] | (text[pc + 1] << 8);
      pc += 2;
      *bytes_read += 2;
      if (disp < 0)
        sprintf(inner + strlen(inner), "-%x", -disp);
      else if (disp > 0)
        sprintf(inner + strlen(inner), "+%x", disp);
    }
  }
  sprintf(buf, "[%s]", inner);
}

// ==========================================
// FONCTION PRINCIPALE
// ==========================================
int main(int argc, char *argv[]) {
  if (argc != 2)
    return 1;

  // Ouverture du fichier binaire compilé
  FILE *fp = fopen(argv[1], "rb");
  if (!fp)
    return 1;

  // Lecture de l'en-tête (32 octets pour a.out minix)
  unsigned char header[32];
  if (fread(header, 1, 32, fp) < 32)
    return 1;

  unsigned int header_size = header[4];
  text_size =
      header[8] | (header[9] << 8) | (header[10] << 16) | (header[11] << 24);

  // Allocation et lecture de la section text
  text = malloc(text_size);
  fseek(fp, header_size, SEEK_SET);
  fread(text, 1, text_size, fp);

  // ==========================================
  // BOUCLE DE DÉSASSEMBLAGE
  // ==========================================
  while (pc < text_size) {
    unsigned int start_pc = pc;
    unsigned char opcode = text[pc++];
    unsigned int bytes_read = 1;
    char mnemonic[128] = "???";
    char rm_buf[64] = "";
    char prefix[16] = "";
    int mod, reg, rm, w, d;

    // ------------------------------------------
    // 1. Gestion des préfixes (REP, REPE, etc.)
    // ------------------------------------------
    int skip_decode = 0;
    if (opcode == 0xF2 || opcode == 0xF3) {
      strcpy(prefix, (opcode == 0xF2) ? "rep " : "repe ");
      if (pc >= text_size) {
        sprintf(mnemonic, "%s", prefix);
        skip_decode = 1;
      } else {
        opcode = text[pc++];
        bytes_read++;
      }
    }

    // ------------------------------------------
    // 2. Décodage par famille d'instructions
    // ------------------------------------------

    if (skip_decode) {
      // Le décodage est ignoré car nous avons atteint la fin
      // du fichier après avoir lu un préfixe.
    }
    // --- ALU Ev, Gv / Gb, Eb et MOV/LEA/XCHG/TEST ---
    else if ((opcode <= 0x3F && (opcode & 0x04) == 0) ||
             (opcode >= 0x88 && opcode <= 0x8B) || opcode == 0x8D ||
             opcode == 0x87 || opcode == 0x86 || opcode == 0x84 ||
             opcode == 0x85) {
      if (pc >= text_size) {
        sprintf(mnemonic, "(undefined)");
      } else {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        w = opcode & 1;
        if (opcode == 0x8D)
          w = 1; // LEA is always 16-bit
        decode_rm(mod, rm, w, rm_buf, &bytes_read);
        const char *op_name = "???";
        int op_type = (opcode >> 3) & 7;
        if (opcode >= 0x88 && opcode <= 0x8B)
          op_name = "mov";
        else if (opcode == 0x8D)
          op_name = "lea";
        else if (opcode == 0x86 || opcode == 0x87)
          op_name = "xchg";
        else if (opcode == 0x84 || opcode == 0x85)
          op_name = "test";
        else {
          const char *alu[] = {"add", "or",  "adc", "sbb",
                               "and", "sub", "xor", "cmp"};
          op_name = alu[op_type];
        }

        d = (opcode & 2) >> 1;
        if (opcode == 0x8D)
          d = 1;
        if (opcode == 0x86 || opcode == 0x87 || opcode == 0x84 ||
            opcode == 0x85)
          d = 0;

        if (d == 1) { // to reg
          sprintf(mnemonic, "%s%s %s, %s", prefix, op_name,
                  w ? regs16[reg] : regs8[reg], rm_buf);
        } else { // to mem
          sprintf(mnemonic, "%s%s %s, %s", prefix, op_name, rm_buf,
                  w ? regs16[reg] : regs8[reg]);
        }
      }
    }
    // --- Opérations rapides sur registres ---
    else if ((opcode & 0xF0) == 0x40) { // INC/DEC r16
      sprintf(mnemonic, "%s%s %s", prefix, (opcode & 8) ? "dec" : "inc",
              regs16[opcode & 7]);
    } else if ((opcode & 0xF0) == 0x50) { // PUSH/POP r16
      sprintf(mnemonic, "%s%s %s", prefix, (opcode & 8) ? "pop" : "push",
              regs16[opcode & 7]);
    }
    // --- Sauts conditionnels (Jcc) ---
    else if ((opcode & 0xF0) == 0x70) { // Jcc
      if (pc < text_size) {
        char disp = text[pc++];
        bytes_read++;
        const char *jcc[] = {"jo",  "jno",  "jb",  "jnb", "je", "jne",
                             "jbe", "jnbe", "js",  "jns", "jp", "jnp",
                             "jl",  "jnl",  "jle", "jnle"};
        sprintf(mnemonic, "%s%s %04x", prefix, jcc[opcode & 15],
                (start_pc + 2 + disp) & 0xFFFF);
      }
    }
    // --- Groupe 1 (ADD, OR, ADC, SBB, AND, SUB, XOR, CMP avec valeur
    // immédiate) ---
    else if (opcode >= 0x80 && opcode <= 0x83) { // Grp1
      if (pc < text_size) {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        w = (opcode == 0x81 || opcode == 0x83);
        decode_rm(mod, rm, w, rm_buf, &bytes_read);
        const char *alu[] = {"add", "or",  "adc", "sbb",
                             "and", "sub", "xor", "cmp"};
        if (opcode == 0x81 && pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%s%s %s, %04x", prefix, alu[reg], rm_buf, imm);
        } else if (opcode == 0x83 && pc < text_size) {
          char imm = text[pc++];
          bytes_read++;
          if (imm < 0)
            sprintf(mnemonic, "%s%s %s, -%x", prefix, alu[reg], rm_buf, -imm);
          else
            sprintf(mnemonic, "%s%s %s, %x", prefix, alu[reg], rm_buf, imm);
        } else if (opcode == 0x80 && pc < text_size) {
          unsigned char imm = text[pc++];
          bytes_read++;
          if (mod != 3) {
            sprintf(mnemonic, "%s%s byte %s, %x", prefix, alu[reg], rm_buf,
                    imm);
          } else {
            sprintf(mnemonic, "%s%s %s, %x", prefix, alu[reg], rm_buf, imm);
          }
        }
      }
    }
    // --- Divers et autres groupes (MOV imm, XCHG, Grp2, Grp3, Grp5) ---
    else if ((opcode & 0xF8) == 0x90) { // XCHG r16, ax
      if (opcode == 0x90)
        sprintf(mnemonic, "%snop", prefix);
      else
        sprintf(mnemonic, "%sxchg %s, ax", prefix, regs16[opcode & 7]);
    } else if (opcode >= 0xB0 && opcode <= 0xBF) { // MOV reg, imm
      w = (opcode & 8) >> 3;
      if (w == 0 && pc < text_size) {
        unsigned char imm = text[pc++];
        bytes_read++;
        sprintf(mnemonic, "%smov %s, %02x", prefix, regs8[opcode & 7], imm);
      } else if (w == 1 && pc + 1 < text_size) {
        unsigned short imm = text[pc] | (text[pc + 1] << 8);
        pc += 2;
        bytes_read += 2;
        sprintf(mnemonic, "%smov %s, %04x", prefix, regs16[opcode & 7], imm);
      }
    } else if (opcode == 0xC6 || opcode == 0xC7) { // MOV rm, imm
      if (pc < text_size) {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        w = opcode & 1;
        decode_rm(mod, rm, w, rm_buf, &bytes_read);
        if (w == 0 && pc < text_size) {
          unsigned char imm = text[pc++];
          bytes_read++;
          if (mod != 3)
            sprintf(mnemonic, "%smov byte %s, %x", prefix, rm_buf, imm);
          else
            sprintf(mnemonic, "%smov %s, %x", prefix, rm_buf, imm);
        } else if (w == 1 && pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%smov %s, %04x", prefix, rm_buf, imm);
        }
      }
    } else if (opcode == 0xD1 || opcode == 0xD3) { // Grp2
      if (pc < text_size) {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        decode_rm(mod, rm, 1, rm_buf, &bytes_read);
        const char *shift[] = {"rol", "ror", "rcl", "rcr",
                               "shl", "shr", "sal", "sar"};
        sprintf(mnemonic, "%s%s %s, %s", prefix, shift[reg], rm_buf,
                (opcode == 0xD1) ? "1" : "cl");
      }
    } else if (opcode == 0xF6 || opcode == 0xF7) { // Grp3
      if (pc < text_size) {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        w = opcode & 1;
        decode_rm(mod, rm, w, rm_buf, &bytes_read);
        const char *grp3[] = {"test", "test", "not", "neg",
                              "mul",  "imul", "div", "idiv"};
        if ((reg == 0 || reg == 1)) {
          if (w == 0 && pc < text_size) {
            unsigned char imm = text[pc++];
            bytes_read++;
            if (mod != 3)
              sprintf(mnemonic, "%s%s byte %s, %x", prefix, grp3[reg], rm_buf,
                      imm);
            else
              sprintf(mnemonic, "%s%s %s, %x", prefix, grp3[reg], rm_buf, imm);
          } else if (w == 1 && pc + 1 < text_size) {
            unsigned short imm = text[pc] | (text[pc + 1] << 8);
            pc += 2;
            bytes_read += 2;
            sprintf(mnemonic, "%s%s %s, %04x", prefix, grp3[reg], rm_buf, imm);
          }
        } else {
          sprintf(mnemonic, "%s%s %s", prefix, grp3[reg], rm_buf);
        }
      }
    } else if (opcode == 0xFF) { // Grp5
      if (pc < text_size) {
        unsigned char modrm = text[pc++];
        bytes_read++;
        mod = modrm >> 6;
        reg = (modrm >> 3) & 7;
        rm = modrm & 7;
        decode_rm(mod, rm, 1, rm_buf, &bytes_read);
        const char *grp5[] = {"inc", "dec", "call", "call",
                              "jmp", "jmp", "push", "???"};
        sprintf(mnemonic, "%s%s %s", prefix, grp5[reg], rm_buf);
      }
    }
    // ------------------------------------------
    // 3. Instructions Sans ModR/M (Switch Cas par Cas)
    // ------------------------------------------
    else {
      switch (opcode) {
      case 0x05:
        if (pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%sadd ax, %04x", prefix, imm);
        }
        break;
      case 0x3D:
        if (pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%scmp ax, %04x", prefix, imm);
        }
        break;
      case 0x2D:
        if (pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%ssub ax, %04x", prefix, imm);
        }
        break;
      case 0x98:
        sprintf(mnemonic, "%scbw", prefix);
        break;
      case 0x99:
        sprintf(mnemonic, "%scwd", prefix);
        break;
      case 0xA3:
        if (pc + 1 < text_size) {
          unsigned short addr = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%smov [%04x], ax", prefix, addr);
        }
        break;
      case 0xA4:
        sprintf(mnemonic, "%smovsb", prefix);
        break;
      case 0xA5:
        sprintf(mnemonic, "%smovsw", prefix);
        break;
      case 0xA8:
        if (pc < text_size) {
          unsigned char imm = text[pc++];
          bytes_read++;
          sprintf(mnemonic, "%stest al, %x", prefix, imm);
        }
        break;
      case 0xAA:
        sprintf(mnemonic, "%sstosb", prefix);
        break;
      case 0xAE:
        sprintf(mnemonic, "%sscasb", prefix);
        break;
      case 0xC2:
        if (pc + 1 < text_size) {
          unsigned short imm = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%sret %04x", prefix, imm);
        }
        break;
      case 0xC3:
        sprintf(mnemonic, "%sret", prefix);
        break;
      case 0xCD:
        if (pc < text_size) {
          unsigned char imm = text[pc++];
          bytes_read++;
          sprintf(mnemonic, "%sint %02x", prefix, imm);
        }
        break;
      case 0xE2:
        if (pc < text_size) {
          char disp = text[pc++];
          bytes_read++;
          sprintf(mnemonic, "%sloop %04x", prefix,
                  (start_pc + 2 + disp) & 0xFFFF);
        }
        break;
      case 0xE5:
        if (pc < text_size) {
          unsigned char imm = text[pc++];
          bytes_read++;
          sprintf(mnemonic, "%sin ax, %x", prefix, imm);
        }
        break;
      case 0xE8:
        if (pc + 1 < text_size) {
          short disp = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%scall %04x", prefix,
                  (start_pc + 3 + disp) & 0xFFFF);
        }
        break;
      case 0xE9:
        if (pc + 1 < text_size) {
          short disp = text[pc] | (text[pc + 1] << 8);
          pc += 2;
          bytes_read += 2;
          sprintf(mnemonic, "%sjmp %04x", prefix,
                  (start_pc + 3 + disp) & 0xFFFF);
        }
        break;
      case 0xEB:
        if (pc < text_size) {
          char disp = text[pc++];
          bytes_read++;
          sprintf(mnemonic, "%sjmp short %04x", prefix,
                  (start_pc + 2 + disp) & 0xFFFF);
        }
        break;
      case 0xEC:
        sprintf(mnemonic, "%sin al, dx", prefix);
        break;
      case 0xFC:
        sprintf(mnemonic, "%scld", prefix);
        break;
      case 0xFD:
        sprintf(mnemonic, "%sstd", prefix);
        break;
      }
    }

    // ------------------------------------------
    // 4. Formatage et Affichage
    // ------------------------------------------
    {
      char bytes_str[32] = "";
      for (unsigned int i = 0; i < bytes_read; i++) {
        sprintf(bytes_str + strlen(bytes_str), "%02x", text[start_pc + i]);
      }

      printf("%04x: %-14s%s\n", start_pc, bytes_str, mnemonic);
    }
  }

  free(text);
  fclose(fp);
  return 0;
}
