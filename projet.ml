(*
  Projet Network_flooding
  Nicolas ASSOUAD
*)

open Printf;;
open Unix;;

exception Paquet_Non_Conforme of string;;

type id_pair = string;;
type seqno = int;;
type neighbour = string * float option * float option;;

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

let port_protocol = 1212;;
let udp_max_len = 5000;;
let timeout_uni_neigh = 100.0;;
let timeout_sym_neigh = 150.0;;
let timeout_sym_neigh_IHU = 300.0;;

let char_of = char_of_int;;

(*Random.self_init ();;
let host_id =
  let value = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set value i (char_of_int (65 + Random.int 40))
  done;
  value
;;*)

let host_id = Bytes.of_string "AAAAAAAA";;

let host_seqno = ref 0;;

let host_data = ref ("ASSOUAD");;

let host_data_tbl = ref [(host_id, !host_seqno, !host_data, Sys.time ())];;

let host_pot_neigh : neighbour list ref = ref [("81.194.27.155", None, None)];;
let host_uni_neigh : neighbour list ref = ref [];;
let host_sym_neigh : neighbour list ref = ref [];;

let is_in_neigh ip neigh_l = List.exists (fun (ip_neigh, _, _) -> ip_neigh = ip) neigh_l;;
let remove_neigh ip neigh_l = List.find_all (fun (ip_neigh, _, _) -> ip_neigh <> ip) neigh_l;;
let update_neigh ip t1 t2 neigh_l = (ip, t1, t2)::(remove_neigh ip neigh_l);;

let maintain_uni_neigh () =
  let curr_t = Sys.time () in
  host_uni_neigh :=
    List.find_all
      (fun (ip_neigh, t1, t2) ->
        match t1 with
        |Some(neigh_t) -> curr_t <= neigh_t +. timeout_uni_neigh
        |_ -> false)
      !host_uni_neigh
;;

let maintain_sym_neigh () =
  let curr_t = Sys.time () in
  host_sym_neigh :=
    List.find_all
      (fun (ip_neigh, t1, t2) ->
        match t1, t2 with
        |Some(neigh_t), Some(neigh_t_IHU) ->
          curr_t <= neigh_t +. timeout_sym_neigh &&
          curr_t <= neigh_t_IHU +. timeout_sym_neigh_IHU
        |_ -> false)
      !host_sym_neigh
;;

let maintain_neigh () =
  maintain_uni_neigh ();
  maintain_sym_neigh ()
;;

let pick_random l =
  let len = List.length l in
  let ind = Random.int len in
  List.nth l ind
;;

let buffer_add_octet buf n nb_octet =
  let tmp = ref n in
  for i = 1 to nb_octet do
    Buffer.add_char buf (char_of (!tmp mod 8));
    tmp := !tmp / 8
  done
;;

let buffer_add_id_pair buf id_pair =
  Buffer.add_string buf id_pair
;;

let buffer_add_seqno buf seqno =
  buffer_add_octet buf seqno 4
;;

let make_int_from_bytes b i l =
  let out = ref 0 in
  let acc = ref 1 in
  for k = i to i+l-1 do
    out := !out + (int_of_char (Bytes.get b k)) * !acc;
    acc := !acc * 256
  done;
  !out
;;

let rec make_TLV tlv =
  let out = Buffer.create 2 in
  (match tlv with
  |Pad0 -> Buffer.add_char out (char_of 0)
  |PadN(nb_MBZ) ->
    Buffer.add_char out (char_of 1);
    Buffer.add_char out (char_of nb_MBZ);
    for i = 1 to nb_MBZ do
      Buffer.add_char out (char_of 0)
    done
  |IHU(id_pair) ->
    Buffer.add_char out (char_of 2);
    buffer_add_id_pair out id_pair
  |Neighbour_Request ->
    Buffer.add_char out (char_of 3);
    Buffer.add_char out (char_of 0)
  |Neighbour(neighbour_l) ->
    Buffer.add_char out (char_of 4);
    Buffer.add_char out (char_of ((List.length neighbour_l) * 26)); (* id+ip+port = 26 octets  *)
    List.iter
      (fun (id_pair, ip_pair, port) ->
        buffer_add_id_pair out id_pair;
        Buffer.add_string out ip_pair;
        buffer_add_octet out port 2)
      neighbour_l
  |Data(seqno, id_pair, data_block_l) ->
    let tlv_data_block = Buffer.create 2 in
    List.iter (fun tlv -> Buffer.add_buffer tlv_data_block (make_TLV tlv)) data_block_l;
    Buffer.add_char out (char_of 5);
    Buffer.add_char out (char_of (12 + (Buffer.length tlv_data_block)));
    buffer_add_seqno out seqno;
    buffer_add_id_pair out id_pair;
    Buffer.add_buffer out tlv_data_block
  |IHave(seqno, id_pair) ->
    Buffer.add_char out (char_of 6);
    Buffer.add_char out (char_of 12);
    buffer_add_seqno out seqno;
    buffer_add_id_pair out id_pair
  |TLV_Data(nb_tlv, data_block) ->
    Buffer.add_char out (char_of nb_tlv);
    Buffer.add_string out data_block);
  out
;;

let make_pack id tlv_l =
  let body = Buffer.create 2 in
  List.iter (fun tlv -> Buffer.add_buffer body (make_TLV tlv)) tlv_l;
  let out = Buffer.create 14 in
  Buffer.add_char out (char_of 57);
  Buffer.add_char out (char_of 0);
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

let rec send_pack_l sock neigh tlv_l =
  match neigh with
  |[] -> ()
  |h::t ->
    let (ip, _, _) = h in
    let addr = inet_addr_of_string ip in
    let sin = ADDR_INET(addr, port_protocol) in
    send_pack sock sin host_id tlv_l;
    send_pack_l sock t tlv_l
;;


let rec interpret_body pack pos pack_len =
  let end_of_pack = pos + pack_len in
  let rec aux pos out =
    if pos >= end_of_pack then
      out
    else begin
      match int_of_char (Bytes.get pack pos) with
      |0 -> aux (pos+1) (Pad0::out)
      |n -> begin
        let tlv_len = make_int_from_bytes pack (pos+1) 1 in
        match n with
        |1 ->
          aux (pos+2+tlv_len) ((PadN(tlv_len))::out)
        |2 ->
          let id_pair = Bytes.sub_string pack (pos+2) 8 in
          aux (pos+10) ((IHU(id_pair))::out)
        |3 -> aux (pos+2+tlv_len) (Neighbour_Request::out)
        |4 ->
          if tlv_len mod 28 <> 0 then
            raise (Paquet_Non_Conforme(sprintf "Taille TLV Neighbours non valide : %d mod <> 0" tlv_len));
          let neigh_l = ref [] in
          let compt = ref 0 in
          while !compt < tlv_len do
            let id_pair = Bytes.sub_string pack (pos+2+ !compt) 8 in
            let ip_pair = Bytes.sub_string pack (pos+2+ !compt+8) 16 in
            let port_pair = make_int_from_bytes pack (pos+2+ !compt+24) 2 in
            compt := !compt + 28;
            neigh_l := ((id_pair, ip_pair, port_pair)::(!neigh_l))
          done;
          aux (pos+2+tlv_len) ((Neighbour(!neigh_l))::out)
        |5 ->
          let seqno_data = make_int_from_bytes pack (pos+2) 4 in
          let id_pair = Bytes.sub_string pack (pos+6) 8 in
          let data_TLV = interpret_body pack (pos+14) (tlv_len - 12) in
          aux (pos+2+tlv_len) ((Data(seqno_data, id_pair, data_TLV))::out)
        |6 ->
          let seqno_data = make_int_from_bytes pack (pos+2) 4 in
          let id_pair = Bytes.sub_string pack (pos+6) 8 in
          aux (pos+2+tlv_len) ((IHave(seqno_data, id_pair))::out)
        |n ->
          let data = Bytes.sub_string pack (pos+2) tlv_len in
          aux (pos+2+tlv_len) ((TLV_Data(n, data))::out)
      end
    end
  in
  aux pos []
;;

let interpret_pack pack =
  if Bytes.get pack 0 <> (char_of 57) then
    raise (Paquet_Non_Conforme("Erreur champs Magic"))
  else if Bytes.get pack 1 <> (char_of 0) then
    raise (Paquet_Non_Conforme("Erreur champs Version"))
  else
    let pack_len = make_int_from_bytes pack 2 2 in
    let src_id = Bytes.sub_string pack 4 8 in
    (src_id, interpret_body pack 12 pack_len)
;;


(* MAIN *)


let send_empty_pack_service sock =
  send_pack_l sock !host_sym_neigh [];
  send_pack_l sock !host_uni_neigh [];
  if List.length !host_sym_neigh < 5 then
    send_pack_l sock [pick_random !host_pot_neigh] []
;;

let send_IHU_service sock =
  send_pack_l sock !host_sym_neigh [IHU(host_id)];
  send_pack_l sock !host_uni_neigh [IHU(host_id)]
;;

let send_service sock compt =
  if compt mod 30 = 0 then
    send_empty_pack_service sock
  else if compt mod 90 = 0 then
    send_IHU_service sock
;;

let update_uni_neigh src_ip t =
  if not (is_in_neigh src_ip !host_uni_neigh && is_in_neigh src_ip !host_sym_neigh) then
    host_pot_neigh := remove_neigh src_ip !host_pot_neigh;
  host_uni_neigh := update_neigh src_ip (Some(t)) None !host_uni_neigh
;;

let update_sym_neigh src_ip body t =
  if List.exists (fun tlv -> tlv = IHU(host_id)) body &&
     not (is_in_neigh src_ip !host_sym_neigh) then begin
    host_pot_neigh := remove_neigh src_ip !host_pot_neigh;
    host_uni_neigh := remove_neigh src_ip !host_uni_neigh
  end;
  host_sym_neigh := update_neigh src_ip (Some(t)) (Some(t)) !host_sym_neigh
;;

let handle_pack src_ip src_id body t =
  update_uni_neigh src_ip t;
  update_sym_neigh src_ip body t
;;

let recv_service sock =
  let pack = Bytes.create udp_max_len in
  match recvfrom sock pack 0 udp_max_len [MSG_PEEK] with
  |len, ADDR_INET(addr, port) ->
    let t = Sys.time () in
    let src_ip = string_of_inet_addr addr in
    let (src_id, body) = interpret_pack pack in
    handle_pack src_ip src_id body t
  |_ -> assert false
;;

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
