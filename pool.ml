type ('a, 'b) t = {
  processes : int;
  workers : ('a, 'b) Process.t list;
}

let create ?(processes=4) () =
  { processes; workers = [] }

let close _pool = ()

let terminate _pool = ()

let start _pool = ()

let join _pool = ()

let map _pool ~f ~arg = ()
