(* ==========================================
 * INCLUDES AND GLOBAL VARIABLES
 * ========================================== *)
open Printf

(* Intel 8086 register names (8-bit and 16-bit) *)
let regs8 = [|"al"; "cl"; "dl"; "bl"; "ah"; "ch"; "dh"; "bh"|]
let regs16 = [|"ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"|]

(* Base addressing formats for Mod = 00, 01, 10 *)
let rm_str = [|"bx+si"; "bx+di"; "bp+si"; "bp+di"; "si"; "di"; "bp"; "bx"|]

(* Opcode family names for easier decoding *)
let alu_ops = [|"add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp"|]
let jcc_ops = [|"jo"; "jno"; "jb"; "jnb"; "je"; "jne"; "jbe"; "jnbe";
                "js"; "jns"; "jp"; "jnp"; "jl"; "jnl"; "jle"; "jnle"|]
let shift_ops = [|"rol"; "ror"; "rcl"; "rcr"; "shl"; "shr"; "sal"; "sar"|]
let grp3_ops = [|"test"; "test"; "not"; "neg"; "mul"; "imul"; "div"; "idiv"|]
let grp5_ops = [|"inc"; "dec"; "call"; "call"; "jmp"; "jmp"; "push"; "???"|]

(* Utility function to sign-extend an 8-bit value to a standard OCaml integer *)
let sign_extend_byte b =
  if b land 0x80 <> 0 then b - 0x100 else b

(* ==========================================
 * FUNCTION: MODR/M DECODING
 * ==========================================
 * This function reads the ModR/M byte parameters and generates the string
 * representing the operand (register or memory with displacement).
 * It returns a tuple: (number of displacement bytes read, string representation)
 *)
let decode_rm text pc text_size mod_val rm w =
  (* Mod = 3: Operand is a register *)
  if mod_val = 3 then
    (0, if w then regs16.(rm) else regs8.(rm))
  
  (* Mod = 0, R/M = 6: Special case for 16-bit absolute addressing *)
  else if mod_val = 0 && rm = 6 then
    if pc + 1 >= text_size then (0, "")
    else
      let disp = Bytes.get_uint16_le text pc in
      (2, sprintf "[%04x]" disp)
  
  (* Other ModR/M memory addressing modes *)
  else
    let inner = rm_str.(rm) in
    
    (* Mod = 1: 8-bit signed displacement *)
    if mod_val = 1 then
      if pc >= text_size then (0, "")
      else
        let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
        let s =
          if disp < 0 then sprintf "[%s-%x]" inner (-disp)
          else if disp > 0 then sprintf "[%s+%x]" inner disp
          else sprintf "[%s]" inner
        in
        (1, s)
    
    (* Mod = 2: 16-bit signed displacement *)
    else if mod_val = 2 then
      if pc + 1 >= text_size then (0, "")
      else
        let disp = Bytes.get_int16_le text pc in
        let s =
          if disp < 0 then sprintf "[%s-%x]" inner (-disp)
          else if disp > 0 then sprintf "[%s+%x]" inner disp
          else sprintf "[%s]" inner
        in
        (2, s)
    
    (* Mod = 0: No displacement *)
    else (0, sprintf "[%s]" inner)

(* ==========================================
 * MAIN FUNCTION
 * ========================================== *)
let () =
  if Array.length Sys.argv <> 2 then exit 1;
  let filename = Sys.argv.(1) in
  
  (* Open the compiled binary file *)
  let ic = open_in_bin filename in
  
  (* Read the header (32 bytes for minix a.out format) *)
  let header = Bytes.create 32 in
  let read_len = input ic header 0 32 in
  if read_len < 32 then (close_in ic; exit 1);

  (* Extract header size and text section size *)
  let header_size = Bytes.get_uint8 header 4 in
  let text_size =
    let b8 = Bytes.get_uint8 header 8 in
    let b9 = Bytes.get_uint8 header 9 in
    let b10 = Bytes.get_uint8 header 10 in
    let b11 = Bytes.get_uint8 header 11 in
    b8 lor (b9 lsl 8) lor (b10 lsl 16) lor (b11 lsl 24)
  in

  (* Allocate memory and read the text section containing the code *)
  let text = Bytes.create text_size in
  seek_in ic header_size;
  let read_text = input ic text 0 text_size in
  if read_text < text_size then (close_in ic; exit 1);
  close_in ic;

  (* ==========================================
   * DISASSEMBLY LOOP
   * ==========================================
   * We use a recursive function 'loop' to act as the instruction pointer (pc).
   *)
  let rec loop pc =
    if pc >= text_size then ()
    else
      let start_pc = pc in
      let opcode = Bytes.get_uint8 text pc in
      let pc = pc + 1 in
      let bytes_read = 1 in
      
      (* ------------------------------------------
       * 1. Prefix Handling (REP, REPE, etc.)
       * ------------------------------------------ *)
      let prefix =
        if opcode = 0xF2 then "rep "
        else if opcode = 0xF3 then "repe "
        else ""
      in
      
      (* If a prefix is found, fetch the actual instruction opcode *)
      let (opcode, pc, bytes_read, skip) =
        if prefix <> "" then
          if pc >= text_size then (opcode, pc, bytes_read, true)
          else (Bytes.get_uint8 text pc, pc + 1, bytes_read + 1, false)
        else (opcode, pc, bytes_read, false)
      in

      (* ------------------------------------------
       * 2. Instruction Decoding via Pattern Matching
       * ------------------------------------------ *)
      let (mnemonic, pc, bytes_read) =
        if skip then (prefix, pc, bytes_read)
        else
          match opcode with
          
          (* --- ALU Ev, Gv / Gb, Eb and MOV/LEA/XCHG/TEST --- *)
          | op when (op <= 0x3F && (op land 0x04) = 0) || (op >= 0x88 && op <= 0x8B) || op = 0x8D || op = 0x87 || op = 0x86 || op = 0x84 || op = 0x85 ->
              if pc >= text_size then ("(undefined)", pc, bytes_read)
              else
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let reg = (modrm lsr 3) land 7 in
                let rm = modrm land 7 in
                let w = if op = 0x8D then true else (op land 1) = 1 in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                
                let op_name =
                  if op >= 0x88 && op <= 0x8B then "mov"
                  else if op = 0x8D then "lea"
                  else if op = 0x86 || op = 0x87 then "xchg"
                  else if op = 0x84 || op = 0x85 then "test"
                  else alu_ops.((op lsr 3) land 7)
                in
                let d = if op = 0x8D then 1 else if op = 0x86 || op = 0x87 || op = 0x84 || op = 0x85 then 0 else (op land 2) lsr 1 in
                let reg_str = if w then regs16.(reg) else regs8.(reg) in
                let m =
                  if d = 1 then sprintf "%s%s %s, %s" prefix op_name reg_str rm_buf (* to reg *)
                  else sprintf "%s%s %s, %s" prefix op_name rm_buf reg_str          (* to mem *)
                in
                (m, pc, bytes_read)
          
          (* --- Quick Register Operations (INC/DEC) --- *)
          | op when (op land 0xF0) = 0x40 ->
              let m = sprintf "%s%s %s" prefix (if (op land 8) <> 0 then "dec" else "inc") regs16.(op land 7) in
              (m, pc, bytes_read)
          
          (* --- Quick Register Operations (PUSH/POP) --- *)
          | op when (op land 0xF0) = 0x50 ->
              let m = sprintf "%s%s %s" prefix (if (op land 8) <> 0 then "pop" else "push") regs16.(op land 7) in
              (m, pc, bytes_read)
          
          (* --- Conditional Jumps (Jcc) --- *)
          | op when (op land 0xF0) = 0x70 ->
              if pc < text_size then
                let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let addr = (start_pc + 2 + disp) land 0xFFFF in
                let m = sprintf "%s%s %04x" prefix jcc_ops.(op land 15) addr in
                (m, pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- Group 1 (ADD, OR, ADC, SBB, AND, SUB, XOR, CMP with immediate value) --- *)
          | op when op >= 0x80 && op <= 0x83 ->
              if pc < text_size then
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let reg = (modrm lsr 3) land 7 in
                let rm = modrm land 7 in
                let w = (op = 0x81 || op = 0x83) in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                
                (* 16-bit immediate *)
                if op = 0x81 && pc + 1 < text_size then
                  let imm = Bytes.get_uint16_le text pc in
                  let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                  (sprintf "%s%s %s, %04x" prefix alu_ops.(reg) rm_buf imm, pc, bytes_read)
                (* 8-bit sign-extended immediate to 16-bit *)
                else if op = 0x83 && pc < text_size then
                  let imm = Bytes.get_uint8 text pc |> sign_extend_byte in
                  let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                  let m = if imm < 0 then sprintf "%s%s %s, -%x" prefix alu_ops.(reg) rm_buf (-imm)
                          else sprintf "%s%s %s, %x" prefix alu_ops.(reg) rm_buf imm in
                  (m, pc, bytes_read)
                (* 8-bit immediate *)
                else if op = 0x80 && pc < text_size then
                  let imm = Bytes.get_uint8 text pc in
                  let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                  let m = if mod_val <> 3 then sprintf "%s%s byte %s, %x" prefix alu_ops.(reg) rm_buf imm
                          else sprintf "%s%s %s, %x" prefix alu_ops.(reg) rm_buf imm in
                  (m, pc, bytes_read)
                else ("???", pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- XCHG r16, ax or NOP --- *)
          | op when (op land 0xF8) = 0x90 ->
              if op = 0x90 then (sprintf "%snop" prefix, pc, bytes_read)
              else (sprintf "%sxchg %s, ax" prefix regs16.(op land 7), pc, bytes_read)
          
          (* --- MOV reg, imm --- *)
          | op when op >= 0xB0 && op <= 0xBF ->
              let w = (op land 8) lsr 3 in
              if w = 0 && pc < text_size then
                let imm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                (sprintf "%smov %s, %02x" prefix regs8.(op land 7) imm, pc, bytes_read)
              else if w = 1 && pc + 1 < text_size then
                let imm = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%smov %s, %04x" prefix regs16.(op land 7) imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- MOV rm, imm --- *)
          | 0xC6 | 0xC7 as op ->
              if pc < text_size then
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let rm = modrm land 7 in
                let w = (op land 1) = 1 in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                if not w && pc < text_size then
                  let imm = Bytes.get_uint8 text pc in
                  let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                  let m = if mod_val <> 3 then sprintf "%smov byte %s, %x" prefix rm_buf imm
                          else sprintf "%smov %s, %x" prefix rm_buf imm in
                  (m, pc, bytes_read)
                else if w && pc + 1 < text_size then
                  let imm = Bytes.get_uint16_le text pc in
                  let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                  (sprintf "%smov %s, %04x" prefix rm_buf imm, pc, bytes_read)
                else ("???", pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- Group 2 (Shifts and Rotates) --- *)
          | 0xD1 | 0xD3 as op ->
              if pc < text_size then
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let reg = (modrm lsr 3) land 7 in
                let rm = modrm land 7 in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm true in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                let m = sprintf "%s%s %s, %s" prefix shift_ops.(reg) rm_buf (if op = 0xD1 then "1" else "cl") in
                (m, pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- Group 3 (TEST, NOT, NEG, MUL, DIV) --- *)
          | 0xF6 | 0xF7 as op ->
              if pc < text_size then
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let reg = (modrm lsr 3) land 7 in
                let rm = modrm land 7 in
                let w = (op land 1) = 1 in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                
                (* Special handling for TEST imm *)
                if reg = 0 || reg = 1 then
                  if not w && pc < text_size then
                    let imm = Bytes.get_uint8 text pc in
                    let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                    let m = if mod_val <> 3 then sprintf "%s%s byte %s, %x" prefix grp3_ops.(reg) rm_buf imm
                            else sprintf "%s%s %s, %x" prefix grp3_ops.(reg) rm_buf imm in
                    (m, pc, bytes_read)
                  else if w && pc + 1 < text_size then
                    let imm = Bytes.get_uint16_le text pc in
                    let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                    (sprintf "%s%s %s, %04x" prefix grp3_ops.(reg) rm_buf imm, pc, bytes_read)
                  else ("???", pc, bytes_read)
                else
                  (sprintf "%s%s %s" prefix grp3_ops.(reg) rm_buf, pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* --- Group 5 (INC/DEC rm, CALL, JMP, PUSH) --- *)
          | 0xFF ->
              if pc < text_size then
                let modrm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let mod_val = modrm lsr 6 in
                let reg = (modrm lsr 3) land 7 in
                let rm = modrm land 7 in
                let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm true in
                let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
                (sprintf "%s%s %s" prefix grp5_ops.(reg) rm_buf, pc, bytes_read)
              else ("???", pc, bytes_read)
          
          (* ------------------------------------------
           * 3. Instructions without ModR/M (Single byte opcodes)
           * ------------------------------------------ *)
          | 0x05 ->
              if pc + 1 < text_size then
                let imm = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%sadd ax, %04x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0x3D ->
              if pc + 1 < text_size then
                let imm = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%scmp ax, %04x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0x2D ->
              if pc + 1 < text_size then
                let imm = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%ssub ax, %04x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0x98 -> (sprintf "%scbw" prefix, pc, bytes_read)
          | 0x99 -> (sprintf "%scwd" prefix, pc, bytes_read)
          | 0xA3 ->
              if pc + 1 < text_size then
                let addr = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%smov [%04x], ax" prefix addr, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xA4 -> (sprintf "%smovsb" prefix, pc, bytes_read)
          | 0xA5 -> (sprintf "%smovsw" prefix, pc, bytes_read)
          | 0xA8 ->
              if pc < text_size then
                let imm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                (sprintf "%stest al, %x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xAA -> (sprintf "%sstosb" prefix, pc, bytes_read)
          | 0xAE -> (sprintf "%sscasb" prefix, pc, bytes_read)
          | 0xC2 ->
              if pc + 1 < text_size then
                let imm = Bytes.get_uint16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                (sprintf "%sret %04x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xC3 -> (sprintf "%sret" prefix, pc, bytes_read)
          | 0xCD ->
              if pc < text_size then
                let imm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                (sprintf "%sint %02x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xE2 ->
              if pc < text_size then
                let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let addr = (start_pc + 2 + disp) land 0xFFFF in
                (sprintf "%sloop %04x" prefix addr, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xE5 ->
              if pc < text_size then
                let imm = Bytes.get_uint8 text pc in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                (sprintf "%sin ax, %x" prefix imm, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xE8 ->
              if pc + 1 < text_size then
                let disp = Bytes.get_int16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                let addr = (start_pc + 3 + disp) land 0xFFFF in
                (sprintf "%scall %04x" prefix addr, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xE9 ->
              if pc + 1 < text_size then
                let disp = Bytes.get_int16_le text pc in
                let pc = pc + 2 in let bytes_read = bytes_read + 2 in
                let addr = (start_pc + 3 + disp) land 0xFFFF in
                (sprintf "%sjmp %04x" prefix addr, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xEB ->
              if pc < text_size then
                let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
                let pc = pc + 1 in let bytes_read = bytes_read + 1 in
                let addr = (start_pc + 2 + disp) land 0xFFFF in
                (sprintf "%sjmp short %04x" prefix addr, pc, bytes_read)
              else ("???", pc, bytes_read)
          | 0xEC -> (sprintf "%sin al, dx" prefix, pc, bytes_read)
          | 0xFC -> (sprintf "%scld" prefix, pc, bytes_read)
          | 0xFD -> (sprintf "%sstd" prefix, pc, bytes_read)
          | _ -> ("???", pc, bytes_read)
      in

      (* ------------------------------------------
       * 4. Formatting and Output
       * ------------------------------------------ *)
      let bytes_str = ref "" in
      for i = 0 to bytes_read - 1 do
        bytes_str := !bytes_str ^ (sprintf "%02x" (Bytes.get_uint8 text (start_pc + i)))
      done;
      
      (* Print the PC, the raw hex bytes, and the decoded assembly mnemonic *)
      printf "%04x: %-14s%s\n" start_pc !bytes_str mnemonic;
      
      (* Continue loop with updated PC *)
      loop pc
  in
  
  (* Start decoding from PC = 0 *)
  loop 0
