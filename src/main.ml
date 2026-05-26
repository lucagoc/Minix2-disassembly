open Printf

(* ==========================================
 * MAIN FUNCTION
 * ========================================== *)
let () =
  if Array.length Sys.argv <> 2 then exit 1;
  let filename = Sys.argv.(1) in
  
  (* Open the compiled binary file *)
  let ic = open_in_bin filename in
  
  (* Read the header *)
  let header = Bytes.create Decoder.minix_header_size in
  let read_len = input ic header 0 Decoder.minix_header_size in
  if read_len < Decoder.minix_header_size then (close_in ic; exit 1);

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
   * ========================================== *)
  let rec loop pc =
    if pc >= text_size then ()
    else
      let start_pc = pc in
      let opcode = Bytes.get_uint8 text pc in
      let pc = pc + 1 in
      let bytes_read = 1 in
      
      (* 1. Prefix Handling *)
      let prefix =
        if opcode = Decoder.prefix_rep_val then "rep "
        else if opcode = Decoder.prefix_repe_val then "repe "
        else ""
      in
      
      let (opcode, pc, bytes_read, skip) =
        if prefix <> "" then
          if pc >= text_size then (opcode, pc, bytes_read, true)
          else (Bytes.get_uint8 text pc, pc + 1, bytes_read + 1, false)
        else (opcode, pc, bytes_read, false)
      in

      (* 2. Instruction Decoding via Sub-functions *)
      let (mnemonic, pc, bytes_read) =
        Decoder.decode_instruction text pc text_size bytes_read start_pc opcode skip prefix
      in

      (* 3. Formatting and Output *)
      let bytes_str = ref "" in
      for i = 0 to bytes_read - 1 do
        bytes_str := !bytes_str ^ (sprintf "%02x" (Bytes.get_uint8 text (start_pc + i)))
      done;
      
      printf "%04x: %-14s%s\n" start_pc !bytes_str mnemonic;
      loop pc
  in
  
  loop 0
