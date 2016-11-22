(*
  Projet Network_flooding
  Nicolas ASSOUAD
*)

open Printf;;
open Unix;;

type id_pair = string;;
type seqno = int;;

type tlv_type =
  |Pad0
  |PadN of int (* Nombre de zero du MBZ *)
  |IHU of id_pair
  |Neighbour_Request
  |Neighbour of (id_pair * string * int) list
  |Data of seqno * id_pair * tlv_type list
  |IHave of seqno * id_pair
  |TLV_Data of int * string
;;

(*Random.self_init ();;
let host_id =
  let value = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set value i (char_of_int (65 + Random.int 40))
  done;
  value
;;*)

let host_id = Bytes.of_string "AAAAAAAA";;

let buffer_add_octet buf n nb_octet =
  let tmp = ref n in
  for i = 1 to nb_octet do
    Buffer.add_char buf (char_of_int (!tmp mod 8));
    tmp := !tmp / 8
  done
;;

let buffer_add_id_pair buf id_pair =
  Buffer.add_string buf id_pair
;;

let buffer_add_seqno buf seqno =
  buffer_add_octet buf seqno 4
;;

let rec make_TLV tlv =
  let out = Buffer.create 2 in
  (match tlv with
  |Pad0 -> Buffer.add_char out (char_of_int 0)
  |PadN(nb_MBZ) ->
    Buffer.add_char out (char_of_int 1);
    Buffer.add_char out (char_of_int nb_MBZ);
    for i = 1 to nb_MBZ do
      Buffer.add_char out (char_of_int 0)
    done
  |IHU(id_pair) ->
    Buffer.add_char out (char_of_int 2);
    buffer_add_id_pair out id_pair
  |Neighbour_Request ->
    Buffer.add_char out (char_of_int 3);
    Buffer.add_char out (char_of_int 0)
  |Neighbour(neighbour_l) ->
    Buffer.add_char out (char_of_int 4);
    Buffer.add_char out (char_of_int ((List.length neighbour_l) * 26)); (* id+ip+port = 26 octets  *)
    List.iter
      (fun (id_pair, ip_pair, port) ->
        buffer_add_id_pair out id_pair;
        Buffer.add_string out ip_pair;
        buffer_add_octet out port 2)
      neighbour_l
  |Data(seqno, id_pair, data_block_l) ->
    let tlv_data_block = Buffer.create 2 in
    List.iter (fun tlv -> Buffer.add_buffer tlv_data_block (make_TLV tlv)) data_block_l;
    Buffer.add_char out (char_of_int 5);
    Buffer.add_char out (char_of_int (12 + (Buffer.length tlv_data_block)));
    buffer_add_seqno out seqno;
    buffer_add_id_pair out id_pair;
    Buffer.add_buffer out tlv_data_block
  |IHave(seqno, id_pair) ->
    Buffer.add_char out (char_of_int 6);
    Buffer.add_char out (char_of_int 12);
    buffer_add_seqno out seqno;
    buffer_add_id_pair out id_pair
  |TLV_Data(nb_tlv, data_block) ->
    Buffer.add_char out (char_of_int nb_tlv);
    Buffer.add_string out data_block);
  out
;;

let make_pack id tlv_l =
  let body = Buffer.create 2 in
  List.iter (fun tlv -> Buffer.add_buffer body (make_TLV tlv)) tlv_l;
  let out = Buffer.create 14 in
  Buffer.add_char out (char_of_int 57);
  Buffer.add_char out (char_of_int 0);
  buffer_add_octet out (Buffer.length body) 2;
  buffer_add_id_pair out id;
  Buffer.add_buffer out body;
  out
;;

let send_pack sock sin id tlv_l =
  let pack = make_pack id tlv_l in
  let data = Buffer.to_bytes pack in
  let data_len = Bytes.length data in
  let nb_bytes_written = sendto sock data 0 data_len [MSG_PEEK] sin in
  if nb_bytes_written <> data_len then
    assert false
;;

(* MAIN *)

let _ =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let addr = inet_addr_of_string "81.194.27.155" in
  let port = 1212 in
  let sin = ADDR_INET(addr, port) in
  printf "%s\n" host_id;
  let tvl_l = [Pad0] in
  send_pack sock sin host_id tvl_l
;;
