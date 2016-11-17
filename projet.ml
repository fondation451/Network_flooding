let convert_to_byte n len =
  let out = Bytes.create len in
  let tmp = ref n in
  for i = 0 to len-1 do
    Bytes.set out (tmp mod 8);
    tmp := tmp / 8;
  done;
  out
;;

let make_pack id body =
  let out = Buffer.create 10 in
  Buffer.add_char out 57;
  Buffer.add_char out 0;
  Buffer.add_bytes out (convert_to_byte (Buffer.length body) 2);
  Buffer.add_bytes out (convert_to_byte id 4);
  Buffer.add_buffer out body;
  out
;;
