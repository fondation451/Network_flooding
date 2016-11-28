(*
  Projet Network_flooding
  Nicolas ASSOUAD
*)

open Printf;;
open Unix;;

let init_time = Unix.time ();;
let time () = Unix.time () -. init_time;;

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

let read_file f =
  let out = Buffer.create 2 in
  let fd = open_in f in
  let rec aux () =
    try
      let new_line = input_line fd in
      Buffer.add_string out new_line;
      aux ()
    with
    |End_of_file -> close_in fd
  in
  aux ();
  Buffer.contents out
;;

(***********)

(* Constant Definition *)

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
let timeout_inondation_host_data = 1800.0;;

let char_of = char_of_int;;

let data_file = "data.txt";;

let ip_prof = "81.194.27.155";;

let host_id = Bytes.of_string "NASSOUAD";;

let host_seqno = ref 1;;

let host_data = ref (TLV_Data(32, read_file data_file));;

let host_data_last_mod = ref ((stat data_file).st_mtime);;

let host_data_tbl = ref [];;

let bootstrap_neigh = (("", ip_prof, port_protocol), None, None);;

let host_pot_neigh : neighbour list ref = ref [];;
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
  print_endline "\n\n\n\n";
  printf "Time : %f\n\n" (time ());
  printf "NEIGH POT : ";
  print_neigh_l !host_pot_neigh;
  printf "\nNEIGH UNI : ";
  print_neigh_l !host_uni_neigh;
  printf "\nNEIGH SYM : ";
  print_neigh_l !host_sym_neigh;
  printf "\nDATA TABLE : \n";
  printf "(%s, %d, " host_id !host_seqno; print_tlv_l [!host_data]; printf ")\n";
  List.iter (fun (id, seqno, tlv_l, t) -> printf "(%s, %d, %f, " id seqno t; print_tlv_l tlv_l; printf ")\n") !host_data_tbl;
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
  let curr_t = time () in
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

let ip4_to_string ip4 =
  let out = Buffer.create 18 in
  Scanf.sscanf ip4 "%d.%d.%d.%d"
    (fun a b c d ->
      Buffer.add_char out (char_of a);
      Buffer.add_char out (char_of b);
      Buffer.add_char out (char_of c);
      Buffer.add_char out (char_of d));
  Buffer.contents out
;;

let string_to_ip4 s =
  let out = Buffer.create 18 in
  for i = 12 to 15 do
    Buffer.add_string out (string_of_int (int_of_char (Bytes.get s i)));
    if i <> 15 then
      Buffer.add_char out '.';
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
let is_in_neigh_id id neigh_l = List.exists (fun ((id', _, _), _, _) -> id' = id) neigh_l;;
let remove_neigh ip id port neigh_l = List.find_all (fun ((_, ip', _), _, _) -> ip' <> ip)  neigh_l;;
let remove_neigh_id ip id port neigh_l =
  List.find_all (fun ((id', ip', _), _, _) -> id' <> id || ip' <> ip) neigh_l
;;
let update_neigh ip id port t1 t2 neigh_l = ((id, ip, port), t1, t2)::(remove_neigh ip id port neigh_l);;
let update_neigh_id ip id port t1 t2 neigh_l = ((id, ip, port), t1, t2)::(remove_neigh_id ip id port neigh_l);;

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
  maintain_sym_neigh ();
  if !host_pot_neigh = [] &&
     !host_uni_neigh = [] &&
     !host_sym_neigh = [] then
    host_pot_neigh := [bootstrap_neigh]
;;

let pick_random l =
  let len = List.length l in
  let ind = Random.int len in
  List.nth l ind
;;

let pick_random_n l n =
  let rec extract_n l n out =
    if n = 0 then
      (List.hd l, List.rev_append (List.tl l) out)
    else
      extract_n (List.tl l) (n-1) ((List.hd l)::out)
  in
  let rec aux l n out =
    if l = [] || n <= 0 then
      out
    else
      let len = List.length l in
      let ind = Random.int len in
      let (new_elem, new_l) = extract_n l ind [] in
      aux new_l (n-1) (new_elem::out)
  in
  aux l n []
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
    out := !out * !acc + (int_of_char (Bytes.get b k));
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
        Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);
        Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);
        Buffer.add_char out (char_of 0);Buffer.add_char out (char_of 0);
        Buffer.add_char out (char_of 255);Buffer.add_char out (char_of 255);
        Buffer.add_string out (ip4_to_string ip_pair);
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
  let nb_bytes_written = sendto sock data 0 data_len [] sin in
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
          if tlv_len mod 26 <> 0 then
            raise (Paquet_Non_Conforme(sprintf "Taille TLV Neighbours non valide : %d mod <> 0" tlv_len));
          let neigh_l = ref [] in
          let compt = ref 0 in
          while !compt < tlv_len do
            let id_pair = Bytes.sub_string pack (pos+2+ !compt) 8 in
            let ip_pair = string_to_ip4 (Bytes.sub_string pack (pos+2+ !compt+8) 16) in
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
    (src_id, interpret_body pack 12 pack_len)
;;

(***********)

(* MAIN *)

let init_inondation seqno id data_l =
  let curr_t = time () in
  inondation_in_process := (seqno, id, data_l, !host_sym_neigh, curr_t, curr_t)::(!inondation_in_process)
;;

let remove_from_inondation seqno id data_l neigh_l t_init t_send src_id src_ip =
  let neigh_l = List.filter (fun ((id', ip', _), _, _) -> id' <> src_id || ip' <> src_ip) neigh_l in
  inondation_in_process := List.filter (fun (_, id', _, _, _, _) -> id' <> id) !inondation_in_process;
  inondation_in_process := (seqno, id, data_l, neigh_l, t_init, t_send)::(!inondation_in_process)
;;

let inondation_step sock =
  inondation_in_process :=
    List.rev_map
      (fun (seqno, id, data_l, neigh_l, t_init, t_send) ->
        if timeout t_send timeout_inondation_send then begin
          send_pack_l sock neigh_l [Data(seqno, id, data_l)];
          (seqno, id, data_l, neigh_l, t_init, time ())
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
  if List.length !host_sym_neigh < 5 && !host_pot_neigh <> [] then
    send_pack_l sock [pick_random !host_pot_neigh] []
;;

let send_IHU_service sock =
  let rec aux l =
    match l with
    |[] -> ()
    |((id, ip, port), t1, t2)::t ->
      send_pack_l sock [((id, ip, port), t1, t2)] [IHU(id)];
      aux t
  in
  aux !host_sym_neigh;
  aux !host_uni_neigh
;;

let send_neighbour_request_service sock =
  let neigh = pick_random_n !host_sym_neigh 1 in
  send_pack_l sock neigh [Neighbour_Request]
;;

let send_service sock (t_empty, t_IHU, t_neigh_request) =
  let curr_t = time () in
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

let update_uni_neigh sock src_ip src_id src_port t =
  if not (is_in_neigh src_ip !host_uni_neigh || is_in_neigh src_ip !host_sym_neigh) then begin
    send_pack_l sock [make_neighbourg src_ip src_id] [IHU(src_id)];
    host_pot_neigh := remove_neigh src_ip src_id src_port !host_pot_neigh;
    host_uni_neigh := update_neigh_id src_ip src_id src_port (Some(t)) None !host_uni_neigh
  end
;;


let handle_tlv sock src_ip src_id src_port tlv t =
  match tlv with
  |Pad0 -> ()
  |PadN(_) -> ()
  |IHU(id) ->
    if id = host_id then begin
      if not (is_in_neigh src_ip !host_sym_neigh) then begin
        send_pack_l sock [make_neighbourg src_ip src_id] [IHU(src_id)];
        host_pot_neigh := remove_neigh src_ip src_id src_port !host_pot_neigh;
        host_uni_neigh := remove_neigh_id src_ip src_id src_port !host_uni_neigh
      end;
      if not (List.exists (fun ((id, _, _), _, _) -> id = src_id) !host_sym_neigh) then begin
        host_sym_neigh := update_neigh_id src_ip src_id src_port (Some(t)) (Some(t)) !host_sym_neigh;
        init_inondation !host_seqno host_id [!host_data]
      end else
        host_sym_neigh := update_neigh_id src_ip src_id src_port (Some(t)) (Some(t)) !host_sym_neigh
    end
  |Neighbour_Request ->
    let neigh_l = pick_random_n !host_sym_neigh 5 in
    let neigh_l = List.rev_map (fun (neigh_info, _, _) -> neigh_info) neigh_l in
    let tlv_l = [Neighbour(neigh_l)] in
    send_pack_l sock [make_neighbourg src_ip src_id] tlv_l
  |Neighbour(neigh_l) ->
    List.iter
      (fun (id, ip, port) ->
        if id <> host_id &&
           not (is_in_neigh_id id !host_uni_neigh) &&
           not (is_in_neigh_id id !host_sym_neigh) then
          host_pot_neigh := update_neigh ip id port None None !host_pot_neigh)
      neigh_l
  |Data(seqno, id, tlv_data_l) -> begin
    if id <> host_id then begin
      try
        let (id, old_seqno, old_data, old_time) = List.find (fun (id', _, _, _) -> id' = id) !host_data_tbl in
        if seqno > old_seqno then begin
          host_data_tbl := List.find_all (fun (id', _, _, _) -> id' <> id) !host_data_tbl;
          host_data_tbl := (id, seqno, tlv_data_l, time ())::(!host_data_tbl);
          init_inondation seqno id tlv_data_l;
          send_pack_l sock [((src_id, src_ip, src_port), None, None)] [IHave(seqno, id)]
        end
      with
      |Not_found ->
        host_data_tbl := (id, seqno, tlv_data_l, time ())::(!host_data_tbl);
        init_inondation seqno id tlv_data_l;
        send_pack_l sock [((src_id, src_ip, src_port), None, None)] [IHave(seqno, id)]
    end
  end
  |IHave(seqno, id) -> begin
    try
      let (_, _, data_l, neigh_l, t_init, t_send) =
        List.find
          (fun (seqno', id', data_l, neigh_l, t_init, t_send) -> id' = id && seqno' <= seqno)
          !inondation_in_process
      in
      remove_from_inondation seqno id data_l neigh_l t_init t_send src_id src_ip
    with
    |Not_found -> ()
  end
  |TLV_Data(data_type, data) -> ()
;;

let handle_pack sock src_ip src_id port body t =
  update_uni_neigh sock src_ip src_id port t;
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
    match recvfrom sock pack 0 udp_max_len [] with
    |len, ADDR_INET(addr, port) ->
      let t = time () in
      let src_ip = string_of_inet_addr addr in
      let (src_id, body) = interpret_pack pack in
      handle_pack sock src_ip src_id port body t
    |_ -> assert false
  with
  |Unix_error(EAGAIN, _, _) -> ()
;;

let update_host_data () =
  let file_stats = stat data_file in
  if file_stats.st_mtime > !host_data_last_mod then begin
    host_data_last_mod := file_stats.st_mtime;
    host_data := TLV_Data(32, read_file data_file);
    host_seqno := !host_seqno + 1;
    init_inondation !host_seqno host_id [!host_data]
  end
;;

let run_protocol () =
  let curr_t = time () -. 100000.0 in (* Pour que les timeouts soient vrais au premier passage *)
  let sock = socket PF_INET SOCK_DGRAM 0 in
  setsockopt sock SO_REUSEADDR true;
  setsockopt_float sock SO_RCVTIMEO 0.01;
  bind sock (ADDR_INET(inet_addr_any, port_protocol));
  let t_inon = ref curr_t in
  let rec protocol_loop sock t_send =
    print_state (); print_endline "";
    update_host_data ();
    maintain_neigh ();
    maintain_data_tbl ();
    maintain_inondation ();
    inondation_step sock;
    let new_t_send = send_service sock t_send in
    recv_service sock;
    if timeout !t_inon timeout_inondation_host_data then begin
      t_inon := time ();
      host_seqno := !host_seqno + 1;
      init_inondation !host_seqno host_id [!host_data];
    end;
    protocol_loop sock new_t_send
  in
  protocol_loop sock (curr_t, curr_t, curr_t)
;;

let _ =
  run_protocol ()
;;

