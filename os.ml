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
  Pid.to_int @@ parent_pid ()

let is_parent_pid () =
  Option.is_some @@ parent_pid ()

let fork () =
  match Unix.fork () with
  | 0 -> In_child
  | n -> In_parent (Pid.of_int n)

let wait () =
  Unix.wait ()

let core_count () =
  (* Mac OS X *)
  let chan = Unix.open_process_in "sysctl -n hw.ncpu" in
  let count = input_line chan in
  close_in chan;
  count
