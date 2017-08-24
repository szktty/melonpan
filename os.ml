include Unix

type fork =
  | In_child
  | In_parent of Pid.t

let pid () =
  Pid.of_int @@ Unix.getpid ()

let pid_value () =
  Pid.to_int @@ pid ()

let parent_pid () =
  match Unix.getpid (), Unix.getppid () with
  | pid, ppid when pid <> ppid -> Some (Pid.of_int ppid)
  | _ -> None

let parent_pid_value () =
  Option.value_map (parent_pid ())
    ~default:None
    ~f:(fun pid -> Some (Pid.to_int pid))

let parent_pid_value_exn () =
  Option.value_exn @@ parent_pid_value ()

let is_parent_pid () =
  Option.is_some @@ parent_pid ()

let fork () =
  match Unix.fork () with
  | 0 -> In_child
  | n -> In_parent (Pid.of_int n)

let wait () =
  Unix.wait ()

let wait_pid_exn pid =
  ignore @@ Unix.waitpid [] (Pid.to_int pid)

let core_count () =
  (* Mac OS X *)
  let chan = Unix.open_process_in "sysctl -n hw.ncpu" in
  let count = input_line chan in
  close_in chan;
  count
