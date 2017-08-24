open Unix

type signal =
  | SIGTERM

type exit_status = 
  | Exit_code of int
  | Exit_signal of signal
  | Exit_exn of exn

type t = {
  mutable pid : Pid.t option;
  parent_pid : Pid.t;
  name : string;
  is_daemon : bool;
  mutable is_alive : bool;
  mutable exit_status : exit_status option;
}

let used_id = ref 0

let next_id () =
  let id = !used_id in
  used_id := id + 1;
  id

let pid_exn p =
  Option.value_exn p.pid

let create ?name ?(is_daemon=false) () =
  { pid = None;
    parent_pid = Os.pid ();
    name = Option.value name
        ~default:(Printf.sprintf "process-%d" (next_id ()));
    is_daemon;
    is_alive = false;
    exit_status = None }

let parent_pid p =
  p.parent_pid

let name p =
  p.name

let is_alive p =
  p.is_alive

let exit_status p =
  p.exit_status

let is_current_parent p =
  p.parent_pid = Os.pid ()

let terminate p =
  p.is_alive <- false;
  p.exit_status <- Some (Exit_signal SIGTERM);
  exit 0
(*remove_child p;*)

let run p ~f =
  (*add_child p;*)
  begin try f () with
    | exn ->
      p.exit_status <- Some (Exit_exn exn);
      p.is_alive <- false
  end

let start p ~f =
  (* TODO: daemon *)
  if p.is_alive then begin
    failwith "already started"
  end;
  match Os.fork () with
  | In_parent _ -> ()
  | In_child ->
    p.pid <- Some (Os.pid ());
    p.is_alive <- true;
    run p ~f;
    terminate p

let join p =
  if is_current_parent p then begin
    ignore @@ Unix.wait ()
  end else
    failwith "only in child process\n%!"
