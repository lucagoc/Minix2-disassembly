open Printf

(* Magic numbers and binary masks *)
let minix_header_size = 32
let prefix_rep_val = 0xF2
let prefix_repe_val = 0xF3
let mask_f0 = 0xF0
let mask_f8 = 0xF8
let op_inc_dec = 0x40
let op_push_pop = 0x50
let op_jcc = 0x70
let op_xchg_ax = 0x90

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
 * ========================================== *)
let decode_rm text pc text_size mod_val rm w =
  if mod_val = 3 then
    (0, if w then regs16.(rm) else regs8.(rm))
  else if mod_val = 0 && rm = 6 then
    if pc + 1 >= text_size then (0, "")
    else
      let disp = Bytes.get_uint16_le text pc in
      (2, sprintf "[%04x]" disp)
  else
    let inner = rm_str.(rm) in
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
    else (0, sprintf "[%s]" inner)

(* ==========================================
 * SUB-FUNCTIONS FOR OPCODE DECODING
 * ========================================== *)

let decode_alu_mov_test op text pc text_size bytes_read prefix =
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
      if d = 1 then sprintf "%s%s %s, %s" prefix op_name reg_str rm_buf
      else sprintf "%s%s %s, %s" prefix op_name rm_buf reg_str
    in
    (m, pc, bytes_read)

let decode_jcc op text pc text_size bytes_read prefix start_pc =
  if pc < text_size then
    let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
    let addr = (start_pc + 2 + disp) land 0xFFFF in
    let m = sprintf "%s%s %04x" prefix jcc_ops.(op land 15) addr in
    (m, pc + 1, bytes_read + 1)
  else ("???", pc, bytes_read)

let decode_grp1 op text pc text_size bytes_read prefix =
  if pc < text_size then
    let modrm = Bytes.get_uint8 text pc in
    let pc = pc + 1 in let bytes_read = bytes_read + 1 in
    let mod_val = modrm lsr 6 in
    let reg = (modrm lsr 3) land 7 in
    let rm = modrm land 7 in
    let w = (op = 0x81 || op = 0x83) in
    let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
    let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
    
    if op = 0x81 && pc + 1 < text_size then
      let imm = Bytes.get_uint16_le text pc in
      (sprintf "%s%s %s, %04x" prefix alu_ops.(reg) rm_buf imm, pc + 2, bytes_read + 2)
    else if op = 0x83 && pc < text_size then
      let imm = Bytes.get_uint8 text pc |> sign_extend_byte in
      let m = if imm < 0 then sprintf "%s%s %s, -%x" prefix alu_ops.(reg) rm_buf (-imm)
              else sprintf "%s%s %s, %x" prefix alu_ops.(reg) rm_buf imm in
      (m, pc + 1, bytes_read + 1)
    else if op = 0x80 && pc < text_size then
      let imm = Bytes.get_uint8 text pc in
      let m = if mod_val <> 3 then sprintf "%s%s byte %s, %x" prefix alu_ops.(reg) rm_buf imm
              else sprintf "%s%s %s, %x" prefix alu_ops.(reg) rm_buf imm in
      (m, pc + 1, bytes_read + 1)
    else ("???", pc, bytes_read)
  else ("???", pc, bytes_read)

let decode_mov_reg_imm op text pc text_size bytes_read prefix =
  let w = (op land 8) lsr 3 in
  if w = 0 && pc < text_size then
    let imm = Bytes.get_uint8 text pc in
    (sprintf "%smov %s, %02x" prefix regs8.(op land 7) imm, pc + 1, bytes_read + 1)
  else if w = 1 && pc + 1 < text_size then
    let imm = Bytes.get_uint16_le text pc in
    (sprintf "%smov %s, %04x" prefix regs16.(op land 7) imm, pc + 2, bytes_read + 2)
  else ("???", pc, bytes_read)

let decode_mov_rm_imm op text pc text_size bytes_read prefix =
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
      let m = if mod_val <> 3 then sprintf "%smov byte %s, %x" prefix rm_buf imm
              else sprintf "%smov %s, %x" prefix rm_buf imm in
      (m, pc + 1, bytes_read + 1)
    else if w && pc + 1 < text_size then
      let imm = Bytes.get_uint16_le text pc in
      (sprintf "%smov %s, %04x" prefix rm_buf imm, pc + 2, bytes_read + 2)
    else ("???", pc, bytes_read)
  else ("???", pc, bytes_read)

let decode_grp2 op text pc text_size bytes_read prefix =
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

let decode_grp3 op text pc text_size bytes_read prefix =
  if pc < text_size then
    let modrm = Bytes.get_uint8 text pc in
    let pc = pc + 1 in let bytes_read = bytes_read + 1 in
    let mod_val = modrm lsr 6 in
    let reg = (modrm lsr 3) land 7 in
    let rm = modrm land 7 in
    let w = (op land 1) = 1 in
    let (rm_bytes, rm_buf) = decode_rm text pc text_size mod_val rm w in
    let pc = pc + rm_bytes in let bytes_read = bytes_read + rm_bytes in
    
    if reg = 0 || reg = 1 then
      if not w && pc < text_size then
        let imm = Bytes.get_uint8 text pc in
        let m = if mod_val <> 3 then sprintf "%s%s byte %s, %x" prefix grp3_ops.(reg) rm_buf imm
                else sprintf "%s%s %s, %x" prefix grp3_ops.(reg) rm_buf imm in
        (m, pc + 1, bytes_read + 1)
      else if w && pc + 1 < text_size then
        let imm = Bytes.get_uint16_le text pc in
        (sprintf "%s%s %s, %04x" prefix grp3_ops.(reg) rm_buf imm, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
    else
      (sprintf "%s%s %s" prefix grp3_ops.(reg) rm_buf, pc, bytes_read)
  else ("???", pc, bytes_read)

let decode_grp5 text pc text_size bytes_read prefix =
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

let decode_single_byte op text pc text_size bytes_read prefix start_pc =
  match op with
  | 0x05 ->
      if pc + 1 < text_size then
        let imm = Bytes.get_uint16_le text pc in
        (sprintf "%sadd ax, %04x" prefix imm, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0x3D ->
      if pc + 1 < text_size then
        let imm = Bytes.get_uint16_le text pc in
        (sprintf "%scmp ax, %04x" prefix imm, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0x2D ->
      if pc + 1 < text_size then
        let imm = Bytes.get_uint16_le text pc in
        (sprintf "%ssub ax, %04x" prefix imm, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0x98 -> (sprintf "%scbw" prefix, pc, bytes_read)
  | 0x99 -> (sprintf "%scwd" prefix, pc, bytes_read)
  | 0xA3 ->
      if pc + 1 < text_size then
        let addr = Bytes.get_uint16_le text pc in
        (sprintf "%smov [%04x], ax" prefix addr, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0xA4 -> (sprintf "%smovsb" prefix, pc, bytes_read)
  | 0xA5 -> (sprintf "%smovsw" prefix, pc, bytes_read)
  | 0xA8 ->
      if pc < text_size then
        let imm = Bytes.get_uint8 text pc in
        (sprintf "%stest al, %x" prefix imm, pc + 1, bytes_read + 1)
      else ("???", pc, bytes_read)
  | 0xAA -> (sprintf "%sstosb" prefix, pc, bytes_read)
  | 0xAE -> (sprintf "%sscasb" prefix, pc, bytes_read)
  | 0xC2 ->
      if pc + 1 < text_size then
        let imm = Bytes.get_uint16_le text pc in
        (sprintf "%sret %04x" prefix imm, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0xC3 -> (sprintf "%sret" prefix, pc, bytes_read)
  | 0xCD ->
      if pc < text_size then
        let imm = Bytes.get_uint8 text pc in
        (sprintf "%sint %02x" prefix imm, pc + 1, bytes_read + 1)
      else ("???", pc, bytes_read)
  | 0xE2 ->
      if pc < text_size then
        let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
        let addr = (start_pc + 2 + disp) land 0xFFFF in
        (sprintf "%sloop %04x" prefix addr, pc + 1, bytes_read + 1)
      else ("???", pc, bytes_read)
  | 0xE5 ->
      if pc < text_size then
        let imm = Bytes.get_uint8 text pc in
        (sprintf "%sin ax, %x" prefix imm, pc + 1, bytes_read + 1)
      else ("???", pc, bytes_read)
  | 0xE8 ->
      if pc + 1 < text_size then
        let disp = Bytes.get_int16_le text pc in
        let addr = (start_pc + 3 + disp) land 0xFFFF in
        (sprintf "%scall %04x" prefix addr, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0xE9 ->
      if pc + 1 < text_size then
        let disp = Bytes.get_int16_le text pc in
        let addr = (start_pc + 3 + disp) land 0xFFFF in
        (sprintf "%sjmp %04x" prefix addr, pc + 2, bytes_read + 2)
      else ("???", pc, bytes_read)
  | 0xEB ->
      if pc < text_size then
        let disp = Bytes.get_uint8 text pc |> sign_extend_byte in
        let addr = (start_pc + 2 + disp) land 0xFFFF in
        (sprintf "%sjmp short %04x" prefix addr, pc + 1, bytes_read + 1)
      else ("???", pc, bytes_read)
  | 0xEC -> (sprintf "%sin al, dx" prefix, pc, bytes_read)
  | 0xFC -> (sprintf "%scld" prefix, pc, bytes_read)
  | 0xFD -> (sprintf "%sstd" prefix, pc, bytes_read)
  | _ -> ("???", pc, bytes_read)

let decode_instruction text pc text_size bytes_read start_pc opcode skip prefix =
  if skip then (prefix, pc, bytes_read)
  else
    match opcode with
    | op when (op <= 0x3F && (op land 0x04) = 0) || (op >= 0x88 && op <= 0x8B) || op = 0x8D || op = 0x87 || op = 0x86 || op = 0x84 || op = 0x85 ->
        decode_alu_mov_test op text pc text_size bytes_read prefix
    | op when (op land mask_f0) = op_inc_dec ->
        let m = sprintf "%s%s %s" prefix (if (op land 8) <> 0 then "dec" else "inc") regs16.(op land 7) in
        (m, pc, bytes_read)
    | op when (op land mask_f0) = op_push_pop ->
        let m = sprintf "%s%s %s" prefix (if (op land 8) <> 0 then "pop" else "push") regs16.(op land 7) in
        (m, pc, bytes_read)
    | op when (op land mask_f0) = op_jcc ->
        decode_jcc op text pc text_size bytes_read prefix start_pc
    | op when op >= 0x80 && op <= 0x83 ->
        decode_grp1 op text pc text_size bytes_read prefix
    | op when (op land mask_f8) = op_xchg_ax ->
        if op = 0x90 then (sprintf "%snop" prefix, pc, bytes_read)
        else (sprintf "%sxchg %s, ax" prefix regs16.(op land 7), pc, bytes_read)
    | op when op >= 0xB0 && op <= 0xBF ->
        decode_mov_reg_imm op text pc text_size bytes_read prefix
    | 0xC6 | 0xC7 as op ->
        decode_mov_rm_imm op text pc text_size bytes_read prefix
    | 0xD1 | 0xD3 as op ->
        decode_grp2 op text pc text_size bytes_read prefix
    | 0xF6 | 0xF7 as op ->
        decode_grp3 op text pc text_size bytes_read prefix
    | 0xFF ->
        decode_grp5 text pc text_size bytes_read prefix
    | op ->
        decode_single_byte op text pc text_size bytes_read prefix start_pc
