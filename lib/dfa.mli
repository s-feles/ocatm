(** Deterministic finite automata over strings (lists) of characters of an ordered type. 

   Standard definition of a DFA (deterministic finite automaton) requires the transition
   function to be total over the Cartesian product of the set of states and the alphabet;
   both of those are required to be finite as well. One can circumvent the totality
   requirement by creating a non-accepting "dead state" and having all pairs outside 
   the domain output this state, with all transitions from it going back to itself.
   Thus the function becomes total, but any arguments from outside the original domain
   effectively lead to immediate rejection. In this implementation, the dead state
   is implemented via the option type, with the transition function outputting None
   if a transition does not exist; this discontinues further computations with a
   rejecting result.
   
   This also allows usage of infinite alphabets and creation of automata over such symbol
   types as Int.
   *)

module type OrderedType = sig
  type t
    (** The type of symbols of the automaton's alphabet. *)

  val compare : t -> t -> int
    (** A total ordering function, analogous to Stdlib.compare. *)
end
(** The OrderedType module type is the same as the one used by, for example, Map. *)

module type DFA = sig
  type sym
    (** The type of symbols of the automaton's alphabet. *)

  type state
    (** The type of states of the automaton. *)

  type atm
    (** The type of an automaton. *)

  val empty : atm
    (** Automaton that accepts no string (recognizes the empty language). *)

  val states : atm -> state list
    (** Returns the states of an automaton. *)

  val start : atm -> state
    (** Returns the start state of an automaton. *)
  
  val accept_states : atm -> state list
    (** Returns the accept states of an automaton. *)

  val delta : atm -> state -> sym -> state option
    (** Returns the transition function of an automaton. *)


  val add_transition : state -> sym -> state -> atm -> atm
    (** Adds a transition between two states to an automaton. 
        The latter state may not exist, in which case it is created
        and added to the automaton's set of states.
        Raises Not_found if the former state does not exist.
        Overwrites existing transitions.
        *)

  val add_accept_state : state -> atm -> atm
    (** Adds the given state to the set of accepting states
        of the automaton. Raises Not_found if the state doesn't
        exist. Does nothing if the state is already accepting.
        *)

  val add_accept_states : state list -> atm -> atm
    (** Similar to the above, but adds multiple states.
        Raises Not_found if any of them do not exist.
        *)

  val compute : sym list -> atm -> bool
    (** Performs a computation on the automaton given a string
        of symbols. Returns true if the computation succeeds
        (ends in an accepting state), false if it fails 
        (ends in a non-accepting state or the transition function
        fails to produce a new state).
        *)
end

module MakeDFA (Ord : OrderedType) : DFA with type sym = Ord.t