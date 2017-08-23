let used_id = ref 0

let next_id () =
  let id = !used_id in
  used_id := id + 1;
  id

type t = {
  id : int;
  pid : int option;
  name : string option;
}

let create_child ?name () =
  { id = next_id (); pid = None; name }

let create_parent ?name pid =
  { id = next_id (); pid = Some pid; name }

let eq p1 p2 =
  p1.id = p2.id

let pid_exn p =
  Option.value_exn p.pid
