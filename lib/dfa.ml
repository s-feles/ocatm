module type OrderedType = Map.OrderedType

module type DFA = sig
  type sym
  type state
  type atm

  val empty : atm
  val states : atm -> state list
  val start : atm -> state
  val accept_states : atm -> state list
  val delta : atm -> state -> sym -> state option

  val add_transition : state -> sym -> state -> atm -> atm
  val add_accept_state : state -> atm -> atm
  val add_accept_states : state list -> atm -> atm
  val compute : sym list -> atm -> bool
end

module MakeDFA (Ord : OrderedType) = struct
  module TransitionMap = Map.Make(struct
    type t = int * Ord.t
    let compare (x0, y0) (x1, y1) = 
        match Int.compare x0 x1 with
        | 0 -> Ord.compare y0 y1
        | n -> n
  end)

  type sym = Ord.t
  type state = int
  type atm = state list * state TransitionMap.t * state * state list

  let empty = [ 0 ], TransitionMap.empty, 0, []
  
  let states (q, _, _, _) = q

  let start (_, _, q0, _) = q0

  let accept_states (_, _, _, f) = f

  let delta (_, d, _, _) = fun q a -> TransitionMap.find_opt (q, a) d

  let add_transition q1 a q2 (states, d, start, accept) =
    if not (List.mem q1 states) then raise Not_found else
    let d' = TransitionMap.add (q1, a) q2 d in
    let states' = if List.mem q2 states then states else q2 :: states in
    states', d', start, accept

  let add_accept_state q (states, d, start, accept) =
    if not (List.mem q states) then raise Not_found 
    else states, d, start, if List.mem q accept then accept else q :: accept

  let add_accept_states qs (states, d, start, accept) =
    if List.exists (fun q -> not (List.mem q states)) qs then raise Not_found
    else states, d, start, List.fold_left (fun sts q -> if List.mem q sts then sts else q :: sts) accept qs

  let compute s m =
    let del = delta m in
    let res = List.fold_left (fun curr a ->
        match curr with
        | None -> None
        | Some q -> del q a) (Some (start m)) s in
    match res with
    | Some q -> List.mem q (accept_states m)
    | None -> false
end