(* Module pour les opérations sur les identifiants *)
module Id = struct
  let m = 16 (* Nombre de bits pour les identifiants *)
  type t = int

  let create key = key mod (1 lsl m)
  let distance a b = (b - a + (1 lsl m)) mod (1 lsl m)
  let between x (a, b) =
    if a = b then x <> a
    else if a < b then x > a && x <= b
    else x > a || x <= b
end

(* Structure de données pour un nœud *)
type node = {
  id: Id.t;
  mutable predecessor: node option;
  mutable successor: node;
  mutable finger_table: node array;
}

(* Création d'un nœud *)
let create_node id =
  let rec node = {
    id;
    predecessor = None;
    successor = node;  (* Le nœud est initialement son propre successeur *)
    finger_table = [||];
  } in
  node.finger_table <- Array.make Id.m node;
  node

(* Recherche du successeur *)
let rec find_successor node id =
  if Id.between id (node.id, node.successor.id) then
    node.successor
  else
    let n' = closest_preceding_finger node id in
    if n'.id = node.id then
      node.successor
    else
      find_successor n' id

(* Trouver le doigt le plus proche précédant l'id *)
and closest_preceding_finger node id =
  let rec aux i =
    if i < 0 then node
    else
      let finger = node.finger_table.(i) in
      if Id.between finger.id (node.id, id) then finger
      else aux (i - 1)
  in
  aux (Id.m - 1)

let notify node n' =
  match node.predecessor with
  | None -> node.predecessor <- Some n'
  | Some p ->
      if Id.between n'.id (p.id, node.id) then
        node.predecessor <- Some n'

let stabilize node =
  let x = node.successor in
  (match x.predecessor with
   | Some p when Id.between p.id (node.id, x.id) ->
       node.successor <- p;
   | _ -> ());
  notify node.successor node

let rec nth_successor node n =
  if n = 0 then node
  else nth_successor node.successor (n - 1)

let fix_fingers node =
  for i = 0 to Id.m - 1 do
    let next = (node.id + (1 lsl i)) mod (1 lsl Id.m) in
    node.finger_table.(i) <- find_successor node next
  done

let join node known_node =
  if node.id <> known_node.id then begin
    node.predecessor <- None;
    node.successor <- find_successor known_node node.id
  end

let run_maintenance node =
  stabilize node;
  fix_fingers node

let print_node_info node =
  Printf.printf "Node %d:\n" node.id;
  Printf.printf "  Predecessor: %s\n" 
    (match node.predecessor with 
     | Some p -> string_of_int p.id 
     | None -> "None");
  Printf.printf "  Successor: %d\n" node.successor.id

let print_finger_table node =
  Printf.printf "Finger Table for Node %d:\n" node.id;
  Array.iteri (fun i finger ->
    let calculation = (node.id + (1 lsl i)) mod (1 lsl Id.m) in
    Printf.printf "  Finger[%2d] = Node %d (Calculation: (%d + 2^%d) mod 2^%d = %d)\n" 
      i finger.id node.id i Id.m calculation
  ) node.finger_table

let print_chord_ring nodes =
  Printf.printf "\nChord Ring:\n";
  let sorted_nodes = List.sort (fun a b -> compare a.id b.id) nodes in
  List.iter (fun node ->
    Printf.printf "%2d -> %2d\n" node.id node.successor.id
  ) sorted_nodes

let print_network_info nodes =
  List.iter (fun node ->
    print_node_info node;
    print_finger_table node;
    Printf.printf "\n"
  ) nodes;
  print_chord_ring nodes

(* Exemple d'utilisation *)
let () =
  let nodes = List.map create_node [1; 8; 15; 22; 30] in
    
  (* Le premier nœud crée le réseau *)
  let first_node = List.hd nodes in

  (* Les autres nœuds rejoignent le réseau *)
  List.tl nodes |> List.iter (fun n -> join n first_node);

  (* Maintenance du réseau *)
  for _ = 1 to 20 do  (* Répéter plusieurs fois pour assurer la stabilité *)
    List.iter run_maintenance nodes
  done;

  (* Affichage des informations du réseau *)
  print_network_info nodes