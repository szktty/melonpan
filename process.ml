open Unix

type signal =
  | SIGTERM

type exit = 
  | Exit_code of int
  | Exit_signal of signal
  | Exit_exn of exn

type ('a, 'b) t = {
  mutable pid : Pid.t option;
  parent_pid : Pid.t;
  name : string;
  target : ('a -> unit) option;
  args : 'a option;
  kwargs : (string * 'b) list;
  daemon : bool;
  mutable is_alive : bool;
  mutable exit: exit option;
}

let used_id = ref 0

let next_id () =
  let id = !used_id in
  used_id := id + 1;
  id

let pid_exn p =
  Printf.printf "pid_exn\n%!";
  Option.value_exn p.pid

let create ?name ?target ?args ?(kwargs=[]) ?(daemon=false) () =
  { pid = None;
    parent_pid = Os.pid ();
    name = Option.value name
        ~default:(Printf.sprintf "process-%d" (next_id ()));
    target;
    args;
    kwargs;
    daemon;
    is_alive = false;
    exit = None }

let is_current_parent p =
  p.parent_pid = Os.pid ()

let terminate p =
  Printf.printf "terminate\n%!";
  p.is_alive <- false;
  p.exit <- Some (Exit_signal SIGTERM);
  (*remove_child p;*)
  exit 0

let run p =
  (*add_child p;*)
  begin match (p.target, p.args) with
    | Some f, Some args ->
      begin try f args with
        | exn ->
          Printf.printf "raised exn\n%!";
          p.exit <- Some (Exit_exn exn);
          p.is_alive <- false;
          exit 0
      end;
      (*terminate p*)
    | _ -> ()
  end;
  ()

let start p =
  match Os.fork () with
  | In_parent _ ->
    Printf.printf "fork parent\n%!"
  | In_child ->
    Printf.printf "fork child\n%!";
    p.is_alive <- true;
    run p

let join p =
  Printf.printf "join ppid %d in pid %d\n%!" (Pid.to_int p.parent_pid)
    (Pid.to_int @@ Os.pid ());
  if is_current_parent p then begin
    Printf.printf "wait\n%!";
    while p.is_alive do
      ()
    done
  end else
    failwith "only in child process\n%!"
