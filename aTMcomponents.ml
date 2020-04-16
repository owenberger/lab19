(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

let database = ref [];;

let rec initialize (lst:  account_spec list) : unit = 
    match lst with
    |[] -> ()
    | hd::tl -> database := !database @ [hd]; initialize tl;;


(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let rec acquire_id () : id =
    Printf.printf "Enter ID: ";
    match read_int_opt () with 
    | None -> Printf.printf "Invalid id"; acquire_id ()
    | Some i -> i ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let rec acquire_amount () : int =
    Printf.printf "Enter amount: ";
    match read_int_opt () with 
    | None -> Printf.printf "Invalid amount"; acquire_amount ()
    | Some i -> if i > 0 then i else (Printf.printf "Invalid amount"; acquire_amount ()) ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let rec acquire_act () : action =
    Printf.printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
    match read_line () with 
    | "B" -> Balance
    | "-" -> Withdraw (acquire_amount ()) 
    | "+" -> Deposit (acquire_amount ())
    | "=" -> Next
    | "X" -> Finished 
    | _ -> Printf.printf "Invalid action"; acquire_act () ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (id_input : id) : int = 
    let lst = List.filter (fun {id;_} ->  id = id_input) !database
    in match lst with
        | [{balance;_}] -> balance
        | _ -> raise Exit ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (id_input : id) : string =
    let lst = List.filter (fun {id;_} ->  id = id_input) !database
    in match lst with
        | [{name;_}] -> name
        | _ -> raise Exit ;; ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance : id -> int -> unit ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
val present_message : string -> unit ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
val deliver_cash : int -> unit ;;
