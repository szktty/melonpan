include Unix

type process =
  | In_child
  | In_parent of Pid.t

let current_pid () =
  match Unix.getpid () with
  | 0 -> In_child
  | n -> In_parent (Pid.create_parent n)

let current_parent_pid () =
  match current_pid () with
  | In_parent pid -> pid
  | In_child -> failwith "not parent"

let main_pid =
  match Unix.getpid () with
  | 0 -> failwith "not parent"
  | n -> Pid.create_parent n

let is_parent_pid () =
  match current_pid () with
  | In_child -> false
  | In_parent _ -> true

let fork () =
  match Unix.fork () with
  | 0 -> In_child
  | n -> In_parent (Pid.create_parent n)

let wait () =
  Unix.wait ()

let core_count () =
  (* Mac OS X *)
  let chan = Unix.open_process_in "sysctl -n hw.ncpu" in
  let count = input_line chan in
  close_in chan;
  count
