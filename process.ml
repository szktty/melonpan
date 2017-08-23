open Unix

type ('a, 'b) t = {
  mutable pid : Pid.t option;
  mutable parent_pid : Pid.t option;
  name : string option;
  target : ('a -> unit) option;
  args : 'a option;
  kwargs : (string * 'b) list;
  daemon : bool;
  mutable is_alive : bool;
  mutable exn : exn option;
}

let pid_exn p =
  Printf.printf "pid_exn\n%!";
  Option.value_exn p.pid

let children : Pid.t list ref = ref []

let add_child p =
  children := pid_exn p :: !children

let remove_child p =
  children := List.filter (Pid.eq (pid_exn p)) !children

let create ?name ?target ?args ?(kwargs=[]) ?(daemon=false) () =
  { pid = None;
    parent_pid = None;
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
  (*remove_child p;*)
  exit 0

let run p =
  (*add_child p;*)
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
  match Os.fork () with
  | In_child ->
    p.parent_pid <- Some (Os.current_parent_pid ());
    p.is_alive <- true;
    run p
  | In_parent _ ->
    Printf.printf "parent process\n%!"

let join p =
  if Os.is_parent_pid () then begin
    Printf.printf "join at parent\n%!";
    ignore @@ Os.wait ()
  end else begin
    Printf.printf "wait\n%!";
    while p.is_alive do
      ()
    done
  end
