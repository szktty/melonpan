open Unix

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
  mutable exn : exn option;
}

let children : int list ref = ref []

let add_child p =
  children := p.pid :: !children

let remove_child p =
  children := List.filter (fun pid -> p.pid <> pid) !children

let create ?(pid=0) ?(parent_pid=None) ?name ?target ?args ?(kwargs=[]) ?(daemon=false) () =
  { pid;
    parent_pid;
    name;
    target;
    args;
    kwargs;
    daemon;
    is_alive = false;
    exn = None }

let terminate p =
  Printf.printf "terminate\n%!";
  p.is_alive <- false;
  remove_child p;
  exit 0

let run p =
  add_child p;
  begin match (p.target, p.args) with
    | Some f, Some args ->
      begin try f args with
        | exn -> Printf.printf "raised exn\n%!";p.exn <- Some exn
      end;
      terminate p
    | _ -> ()
  end;
  ()

let start p =
  p.parent_pid <- Some (getpid ());
  match fork () with
  | 0 ->
    p.pid <- getpid ();
    p.is_alive <- true;
    run p
  | _ ->
    Printf.printf "parent process\n%!"

let join p =
  Printf.printf "join %d %d %d\n%!" base_pid p.pid (getpid ());
  if base_pid = getpid () then begin
    Printf.printf "join at parent\n%!";
    ignore @@ wait ()
  end else begin
    Printf.printf "wait\n%!";
    while p.is_alive do
      ()
    done;
    Printf.printf "exit pid %d\n%!" p.pid
  end

let core_count () =
  let chan = open_process_in "sysctl -n hw.ncpu" in
  let count = input_line chan in
  close_in chan;
  count

