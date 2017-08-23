open Unix

let _ = Printf.printf "\n"
let base_pid = getpid ()

type ('a, 'b) t = {
  mutable pid : int;
  mutable parent_pid : int option;
  name : string option;
  target : ('a -> unit) option;
  args : 'a option;
  kwargs : (string * 'b) list;
  daemon : bool;
  mutable is_alive : bool;
}

let children : int list ref = ref []

let add_child p =
  children := p.pid :: !children

let create ?(pid=0) ?(parent_pid=None) ?name ?target ?args ?(kwargs=[]) ?(daemon=false) () =
  { pid;
    parent_pid;
    name;
    target;
    args;
    kwargs;
    daemon;
    is_alive = false }

let run p =
  Printf.printf "run %d in %d\n" p.pid (getpid ());
  add_child p;
  begin match (p.target, p.args) with
    | Some f, Some args -> f args
    | _ -> ()
  end;
  ()

let start p =
  p.parent_pid <- Some (getpid ());
  match fork () with
  | 0 ->
    p.pid <- getpid ();
    p.is_alive <- true;
    Printf.printf "fork process %d\n" (getpid ());
    run p
  | _ -> Printf.printf "parent process\n"

let join p =
  Printf.printf "join %d %d %d\n" base_pid p.pid (getpid ());
  if base_pid = getpid () then begin
    Printf.printf "join at parent\n";
    ignore @@ wait ()
  end else begin
      (*
      while p.is_alive do
        ()
      done;
       *)
    Printf.printf "exit pid %d\n" p.pid;
    exit 0
  end
