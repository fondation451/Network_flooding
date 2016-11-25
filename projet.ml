(*
  Projet Network_flooding
  Nicolas ASSOUAD
*)

open Printf;;
open Unix;;

exception Paquet_Non_Conforme of string;;

(* Type Definition *)

type id_pair = string;;
type seqno = int;;
type neigbour_info = id_pair * string * int;; (* ID * IP * Port *)
type neighbour = neigbour_info * float option * float option;;

type tlv_type =
  |Pad0
  |PadN of int (* Nombre de zero du MBZ *)
  |IHU of id_pair
  |Neighbour_Request
  |Neighbour of neigbour_info list
  |Data of seqno * id_pair * tlv_type list
  |IHave of seqno * id_pair
  |TLV_Data of int * string
;;

(* DEBUGGING Functions *)

let rec tlv_to_string tlv =
  let out = Buffer.create 2 in
  (match tlv with
  |Pad0 -> Buffer.add_string out "Pad0"
  |PadN(n) -> Buffer.add_string out (sprintf "PadN(%d)" n)
  |IHU(id_pair) -> Buffer.add_string out (sprintf "IHU(%s)" id_pair)
  |Neighbour_Request -> Buffer.add_string out "Neighbour_Request"
  |Neighbour(neigh_info) ->
    Buffer.add_string out "Neighbour(";
    List.iter (fun (id, ip, port) -> Buffer.add_string out (sprintf "(%s, %s, %d), " id ip port)) neigh_info;
    Buffer.add_string out ")"
  |Data(seqno, id, tlv_l) ->
    Buffer.add_string out (sprintf "Data(%s, %d)\n" id seqno);
    List.iter (fun tlv -> Buffer.add_string out (sprintf "\t%s\n" (tlv_to_string tlv))) tlv_l
  |IHave(seqno, id) -> Buffer.add_string out (sprintf "IHave(%s, %d)" id seqno)
  |TLV_Data(nb, data) -> Buffer.add_string out (sprintf "TLV_Data(%d, %s)" nb data));
  Buffer.contents out
;;

let print_tlv tlv =
  print_string (tlv_to_string tlv)
;;

let print_tlv_l tlv_l =
  List.iter print_tlv tlv_l
;;

(***********)

(* Constant Defintion *)

let port_protocol = 1212;;
let udp_max_len = 5000;;
let timeout_empty = 30.0;;
let timeout_IHU = 90.0;;
let timeout_neighbour_request = 180.0;;
let timeout_uni_neigh = 100.0;;
let timeout_sym_neigh = 150.0;;
let timeout_sym_neigh_IHU = 300.0;;
let timeout_data_maintain = 600.0;;
let timeout_data = 2100.0;;
let timeout_inondation_send = 3.0;;
let timeout_inondation = 11.0;;

let char_of = char_of_int;;

(*Random.self_init ();;
let host_id =
  let value = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set value i (char_of_int (65 + Random.int 40))
  done;
  value
;;*)

let ip_prof = "0000:0000:0000:0000:0000:ffff:51c2:1b9b";;

let host_id = Bytes.of_string "AAAAAAAA";;

let host_seqno = ref 20;;

let host_data = ref (TLV_Data(32, "ca degage !"));;

let host_data_tbl = ref [(host_id, !host_seqno, [!host_data], Sys.time ())];;

let host_pot_neigh : neighbour list ref = ref [((host_id, ip_prof, port_protocol), None, None)];;
let host_uni_neigh : neighbour list ref = ref [];;
let host_sym_neigh : neighbour list ref = ref [];;

let inondation_in_process = ref [];;

(***********)

let print_neigh ((id, ip, port), st1, st2) =
  printf "(%s, %s, %d" id ip port;
  if st1 <> None then
    let Some(t1) = st1 in
    printf ", t1 = %f" t1;
  if st2 <> None then
    let Some(t2) = st2 in
    printf ", t2 = %f" t2;
  printf ")"
;;

let print_neigh_l neigh_l =
  printf "[";
  List.iter (fun neigh -> print_neigh neigh; printf " ; ") neigh_l;
  printf "]"
;;

let print_state () =
  printf "NEIGH POT : ";
  print_neigh_l !host_pot_neigh;
  printf "\nNEIGH UNI : ";
  print_neigh_l !host_uni_neigh;
  printf "\nNEIGH SYM : ";
  print_neigh_l !host_sym_neigh;
  printf "\nDATA TABLE : ";
  List.iter (fun (id, seqno, tlv_l, t) -> printf "(%s, %d, %f, " id seqno t; print_tlv_l tlv_l; printf ") ; ") !host_data_tbl;
  printf "\nINONDATION : \n";
  List.iter
    (fun (seqno, id, data_l, neigh_l, t_init, t_send) ->
      printf "\tID : %s seqno : %d t_i = %f t_s = %f data : " id seqno t_init t_send;
      print_tlv_l data_l; printf " neigh : ";
      print_neigh_l neigh_l;
      printf "\n")
    !inondation_in_process;
  printf "\n"
;;

let timeout t t_timeout =
  let curr_t = Sys.time () in
  abs_float (curr_t -. t) >= t_timeout
;;

let int_of_hex c =
  match c with
  |'a' |'A' -> 10
  |'b' |'B' -> 11
  |'c' |'C' -> 12
  |'d' |'D' -> 13
  |'e' |'E' -> 14
  |'f' |'F' -> 15
  |'0' -> 0
  |'1' -> 1
  |'2' -> 2
  |'3' -> 3
  |'4' -> 4
  |'5' -> 5
  |'6' -> 6
  |'7' -> 7
  |'8' -> 8
  |'9' -> 9
  |_ -> assert false
;;

let hex_of_int n =
  match n with
  |10 -> 'a'
  |11 -> 'b'
  |12 -> 'c'
  |13 -> 'd'
  |14 -> 'e'
  |15 -> 'f'
  |0 -> '0'
  |1 -> '1'
  |2 -> '2'
  |3 -> '3'
  |4 -> '4'
  |5 -> '5'
  |6 -> '6'
  |7 -> '7'
  |8 -> '8'
  |9 -> '9'
  |_ -> assert false
;;

let string_to_ip6 ip =
  let out = Buffer.create 2 in
  let i = ref 0 in
  while !i <= 38 do
    let tmp1 = (int_of_hex ip.[!i]) * 16 + (int_of_hex ip.[!i+1]) in
    let tmp2 = (int_of_hex ip.[!i+2]) * 16 + (int_of_hex ip.[!i+3]) in
    i := !i + 5;
    Buffer.add_char out (char_of tmp1);
    Buffer.add_char out (char_of tmp2)
  done;
  Buffer.to_bytes out
;;

let ip6_to_string ip =
  let out = Buffer.create 2 in
  let i = ref 0 in
  while !i < 16 do
    let tmp1 = int_of_char (ip.[!i]) in
    let tmp2 = int_of_char (ip.[!i + 1]) in
    Buffer.add_char out (hex_of_int (tmp1 / 16));
    Buffer.add_char out (hex_of_int (tmp1 mod 16));
    Buffer.add_char out (hex_of_int (tmp2 / 16));
    Buffer.add_char out (hex_of_int (tmp2 mod 16));
    i := !i + 2;
    if !i <> 16 then Buffer.add_char out ':';
  done;
  Buffer.to_bytes out
;;

let string_to_hex s =
  let out = Buffer.create 2 in
  let len = String.length s - 1 in
  for i = 0 to len do
    let o = int_of_char (s.[i]) in
    let h1 = hex_of_int (o / 16) in
    let h2 = hex_of_int (o mod 16) in
    Buffer.add_char out h1;
    Buffer.add_char out h2;
  done;
  Buffer.contents out
;;

let is_in_neigh ip neigh_l = List.exists (fun ((_, ip_neigh, _), _, _) -> ip_neigh = ip) neigh_l;;
let remove_neigh ip id port neigh_l =
  List.find_all
    (fun ((id_neigh, ip_neigh, port_neigh), _, _) -> ip_neigh <> ip)
    neigh_l;;
let update_neigh ip id port t1 t2 neigh_l = ((id, ip, port), t1, t2)::(remove_neigh ip id port neigh_l);;

let maintain_uni_neigh () =
  host_uni_neigh :=
    List.find_all
      (fun (ip_neigh, t1, t2) ->
        match t1 with
        |Some(neigh_t) -> not (timeout neigh_t timeout_uni_neigh)
        |_ -> false)
      !host_uni_neigh
;;

let maintain_sym_neigh () =
  host_sym_neigh :=
    List.find_all
      (fun (ip_neigh, t1, t2) ->
        match t1, t2 with
        |Some(neigh_t), Some(neigh_t_IHU) ->
          not (timeout neigh_t timeout_sym_neigh) &&
          not (timeout neigh_t_IHU timeout_sym_neigh_IHU)
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

let pick_random_n l n =
  if l = [] then
    []
  else
    [List.hd l]
;;

let make_neighbourg ip id = ((id, ip, port_protocol), None, None);;

let buffer_add_octet buf n nb_octet =
  let tmp = ref n in
  let acc = ref [] in
  for i = 1 to nb_octet do
    acc := (char_of (!tmp mod 256))::(!acc);
    tmp := !tmp / 256
  done;
  List.iter (fun o -> Buffer.add_char buf o) !acc
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
    (*print_endline (sprintf "%d, " (int_of_char (Bytes.get b k)));*)
    out := !out * !acc + (int_of_char (Bytes.get b k));
    acc := !acc * 256
  done;
  (*print_endline (sprintf "out = %d" !out);*)
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
    Buffer.add_char out (char_of 8);
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
        Buffer.add_string out (string_to_ip6 ip_pair);
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
    Buffer.add_char out (char_of (String.length data_block));
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
    let ((id, ip, port), _, _) = h in
    let addr = inet_addr_of_string ip in
    let sin = ADDR_INET(addr, port) in
    print_endline (sprintf "Envoie à Id : %s  IP : %s  port : %d" id ip port);
    print_tlv_l tlv_l;
    send_pack sock sin host_id tlv_l;
    send_pack_l sock t tlv_l
;;


let rec interpret_body pack pos pack_len =
  let end_of_pack = pos + pack_len in
  let rec aux pos out =
    if pos >= end_of_pack then
      out
    else begin
      print_endline (sprintf "interpretation : pos = %d  octet = %d" pos (int_of_char (Bytes.get pack pos)));
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
          if tlv_len mod 26 <> 0 then
            raise (Paquet_Non_Conforme(sprintf "Taille TLV Neighbours non valide : %d mod <> 0" tlv_len));
          let neigh_l = ref [] in
          let compt = ref 0 in
          while !compt < tlv_len do
            let id_pair = Bytes.sub_string pack (pos+2+ !compt) 8 in
            let ip_pair = ip6_to_string (Bytes.sub_string pack (pos+2+ !compt+8) 16) in
            let port_pair = make_int_from_bytes pack (pos+2+ !compt+24) 2 in
            compt := !compt + 26;
            neigh_l := ((id_pair, ip_pair, port_pair)::(!neigh_l))
          done;
          aux (pos+2+tlv_len) ((Neighbour(!neigh_l))::out)
        |5 ->
          let seqno_data = make_int_from_bytes pack (pos+2) 4 in
          let id_pair = Bytes.sub_string pack (pos+6) 8 in
          let data_TLV = interpret_body pack (pos+14) (tlv_len - 12) in
          aux (pos+2+tlv_len) ((Data(seqno_data, id_pair, data_TLV))::out)
        |6 ->
          print_endline "IHAVE interprete !!";
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
    (*print_endline (sprintf "id : %s et len : %d" src_id pack_len);
    print_endline (string_to_hex pack);*)
    (src_id, interpret_body pack 12 pack_len)
;;

(***********)

(* MAIN *)

let init_inondation seqno id data_l =
  let curr_t = Sys.time () in
  inondation_in_process := (seqno, id, data_l, !host_sym_neigh, curr_t, curr_t)::(!inondation_in_process)
;;

let remove_from_inondation seqno id data_l neigh_l t_init t_send src_id =
  let neigh_l = List.filter (fun ((id', _, _), _, _) -> id' <> src_id) neigh_l in
  inondation_in_process := List.filter (fun (_, id', _, _, _, _) -> id' <> id) !inondation_in_process;
  inondation_in_process := (seqno, id, data_l, neigh_l, t_init, t_send)::(!inondation_in_process)
;;

let inondation_step sock =
  inondation_in_process :=
    List.rev_map
      (fun (seqno, id, data_l, neigh_l, t_init, t_send) ->
        if timeout t_send timeout_inondation_send then begin
          send_pack_l sock neigh_l [Data(seqno, id, data_l)];
          (seqno, id, data_l, neigh_l, t_init, Sys.time ())
        end else
          (seqno, id, data_l, neigh_l, t_init, t_send))
      !inondation_in_process
;;

let maintain_inondation () =
  inondation_in_process :=
    List.filter
      (fun (seqno, id, _, neigh_l, t_init, _) ->
        if neigh_l = [] then begin
          printf "Inondation termine ==> Id : %s Seqno : %d\n" id seqno;
          false
        end else if timeout t_init timeout_inondation then begin
          printf "Inondation arretee par Timeout ==> Id : %s    Seqno : %d\n" id seqno;
          false
        end else
          true)
      !inondation_in_process
;;

let send_empty_pack_service sock =
  send_pack_l sock !host_sym_neigh [];
  send_pack_l sock !host_uni_neigh [];
  if List.length !host_sym_neigh < 5 then
    send_pack_l sock [pick_random !host_pot_neigh] []
;;

let send_IHU_service sock =
  printf "IHU Service !\n";
  send_pack_l sock !host_sym_neigh [IHU(host_id)];
  send_pack_l sock !host_uni_neigh [IHU(host_id)]
;;

let send_neighbour_request_service sock =
  let neigh = pick_random !host_sym_neigh in
  send_pack_l sock [neigh] [Neighbour_Request]
;;

let send_service sock (t_empty, t_IHU, t_neigh_request) =
  let curr_t = Sys.time () in
  let new_t_empty =
    if timeout t_empty timeout_empty then begin
      send_empty_pack_service sock;
      curr_t
    end else
      t_empty
  in
  let new_t_IHU =
    if timeout t_IHU timeout_IHU then begin
      send_IHU_service sock;
      curr_t
    end else
      t_IHU
  in
  let new_t_neigh_request =
    if timeout t_neigh_request timeout_neighbour_request && List.length !host_pot_neigh < 5 then begin
      send_neighbour_request_service sock;
      curr_t
    end else
      t_neigh_request
  in
  (new_t_empty, new_t_IHU, new_t_neigh_request)
;;

let update_uni_neigh src_ip src_id src_port t =
  if not (is_in_neigh src_ip !host_uni_neigh || is_in_neigh src_ip !host_sym_neigh) then begin
    host_pot_neigh := remove_neigh src_ip src_id src_port !host_pot_neigh;
    host_uni_neigh := update_neigh src_ip src_id src_port (Some(t)) None !host_uni_neigh
  end
;;


let handle_tlv sock src_ip src_id src_port tlv t =
  match tlv with
  |Pad0 -> ()
  |PadN(_) -> ()
  |IHU(id) ->
    if not (is_in_neigh src_ip !host_sym_neigh) then begin
      send_pack_l sock [make_neighbourg src_ip src_id] [IHU(src_id)];
      host_pot_neigh := remove_neigh src_ip src_id src_port !host_pot_neigh;
      host_uni_neigh := remove_neigh src_ip src_id src_port !host_uni_neigh
    end;
    host_sym_neigh := update_neigh src_ip src_id src_port (Some(t)) (Some(t)) !host_sym_neigh
  |Neighbour_Request ->
    let neigh_l = pick_random_n !host_sym_neigh 5 in
    let neigh_l = List.rev_map (fun (neigh_info, _, _) -> neigh_info) neigh_l in
    let tlv_l = [Neighbour(neigh_l)] in
    send_pack_l sock [make_neighbourg src_ip src_id] tlv_l
  |Neighbour(neigh_l) ->
    List.iter
      (fun neigh_info -> host_pot_neigh := (neigh_info, None, None)::(!host_pot_neigh))
      neigh_l
  |Data(seqno, id, tlv_data_l) -> begin
    try
      let (id, old_seqno, old_data, old_time) = List.find (fun (id', _, _, _) -> id' = id) !host_data_tbl in
      if seqno > old_seqno then begin
        host_data_tbl := List.find_all (fun (id', _, _, _) -> id' <> id) !host_data_tbl;
        host_data_tbl := (id, seqno, tlv_data_l, Sys.time ())::(!host_data_tbl);
        init_inondation seqno id tlv_data_l;
        send_pack_l sock [((src_id, src_ip, src_port), None, None)] [IHave(seqno, id)]
      end
    with
    |Not_found ->
      host_data_tbl := (id, seqno, tlv_data_l, Sys.time ())::(!host_data_tbl);
      init_inondation seqno id tlv_data_l;
      send_pack_l sock [((src_id, src_ip, src_port), None, None)] [IHave(seqno, id)]
  end
  |IHave(seqno, id) -> begin
    print_endline "IHAVE C EST BON !!!!!!!!\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
    try
      let (_, _, data_l, neigh_l, t_init, t_send) =
        List.find
          (fun (seqno', id', data_l, neigh_l, t_init, t_send) -> id' = id && seqno' <= seqno)
          !inondation_in_process
      in
      remove_from_inondation seqno id data_l neigh_l t_init t_send src_id
    with
    |Not_found -> ()
  end
  |TLV_Data(data_type, data) -> ()
;;

let handle_pack sock src_ip src_id port body t =
  update_uni_neigh src_ip src_id port t;
  List.iter (fun tlv -> handle_tlv sock src_ip src_id port tlv t) body
;;

let maintain_data_tbl () =
  host_data_tbl :=
    List.find_all
      (fun (id, seqno, data, t_data) -> not (timeout t_data timeout_data))
      !host_data_tbl
;;


let recv_service sock =
  let pack = Bytes.create udp_max_len in
  try
    match recvfrom sock pack 0 udp_max_len [MSG_PEEK] with
    |len, ADDR_INET(addr, port) ->
      let t = Sys.time () in
      let src_ip = string_of_inet_addr addr in
      let (src_id, body) = interpret_pack pack in
      (*print_endline (sprintf "Reception Id : %s" src_id);
      print_tlv_l body;
      print_endline "";*)
      handle_pack sock src_ip src_id port body t
    |_ -> assert false
  with
  |Unix_error(EAGAIN, _, _) -> ()
;;

let run_protocol () =
  let curr_t = Sys.time () -. 100.0 in
  let sock = socket PF_INET6 SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  setsockopt_float sock SO_RCVTIMEO 0.1;
  bind sock (ADDR_INET(inet6_addr_any, port_protocol));
  let t_inon = ref curr_t in
  let rec protocol_loop sock t_send =
    (*print_state (); print_endline "";*)
    maintain_neigh ();
    maintain_data_tbl ();
    maintain_inondation ();
    inondation_step sock;
    let new_t_send = send_service sock t_send in
    recv_service sock;
    if timeout !t_inon 15.0 then begin
      t_inon := Sys.time ();
      host_seqno := !host_seqno + 1;
      init_inondation !host_seqno host_id [!host_data];
    end;
    flush Pervasives.stdout;
    protocol_loop sock new_t_send
  in
  protocol_loop sock (curr_t, curr_t, curr_t)
;;

let _ =
  run_protocol ()
;;

(*
let _ =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let addr = inet_addr_of_string "81.194.27.155" in
  let port = 1212 in
  let sin = ADDR_INET(addr, port) in
  printf "%s\n" host_id;
  let tvl_l = [Pad0] in
  print_tlv_l tvl_l;
  send_pack sock sin host_id tvl_l
;;*)
(*
let _ =
  let ip1 = string_to_ip6 ip_prof in
  let ip2 = ip6_to_string ip1 in
  print_endline ip_prof;
  print_endline ip1;
  print_endline ip2;
  let tlv = [Neighbour(["NASSOUAD", ip_prof, 123 ; "JULEPERS", ip_prof, 1000]) ; IHave(3333, "AZERTYUI") ; PadN(20) ; Pad0] in
  let pack = Buffer.to_bytes (make_pack host_id tlv) in
  printf "%s et %d\n" pack (Bytes.length pack);
  print_endline "_______________________";
  let (id, tlv_l) = interpret_pack pack in
  printf "ID = %s\n" host_id;
  print_tlv_l tlv;
  printf "ID = %s\n" id;
  print_tlv_l tlv_l
;;*)


