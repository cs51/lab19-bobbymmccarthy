
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

type account_spec = {name : string; id : id; balance : int} ;;

let accts = ref [];;

let initialize (lst: account_spec list) : unit =
	accts := (List.fold_left (fun x y -> ref y :: x) [] lst) @ !accts;;

let acquire_id () : id =
	print_string ("print your id: \n");
	read_int ();;

let acquire_amount () : int =
	print_string ("amount: \n");
	read_int ();;

let acquire_act () : action =
	print_string ("enter action: \n");
	match (read_line ()) with
	| "Balance" -> Balance
	| "Withdraw" -> Withdraw (acquire_amount ())
	| "Deposit" -> Deposit (acquire_amount ())
	| "Next" -> Next
	| "Finished" -> Finished
	| _ -> raise (Failure "not valid input");;

let get_balance (i : id) : int =
	match List.filter (fun x -> !x.id = i) !accts with
	| [] -> raise Not_found
	| h :: _ -> !h.balance;;

let get_name (i : id) : string =
	match List.filter (fun x -> !x.id = i) !accts with
	| [] -> raise Not_found
	| h :: _ -> !h.name;;

let update_balance (i : id) (money : int): unit =
	if (List.fold_left (fun x y-> if (!y).id = i then (y :=  {!y with balance = money}; true) else x) false !accts) then
	() else raise Not_found;;

let present_message (s: string) : unit =
	print_string (s ^ "\n");;

let rec deliver_cash (money: int) : unit =
	if money >= 20 then 
		(print_string ("[20 @ 20]");
		deliver_cash (money - 20))
	else
		(Printf.printf "and %i more" money; ()) ;;





