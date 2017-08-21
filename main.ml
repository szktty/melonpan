module Process = struct

  type ('a, 'b) t = {
    mutable pid : int;
    name : string option;
    target : ('a -> unit) option;
    args : 'a option;
    kwargs : (string * 'b) list;
    daemon : bool;
    mutable is_alive : bool;
  }

  let create ?name ?target ?args ?(kwargs=[]) ?(daemon=false) () =
    { pid = 0; name; target; args; kwargs; daemon; is_alive = false }

  let run p =
    p.pid <- Unix.fork ();
    begin match (p.target, p.args) with
      | Some f, Some args -> f args
      | _ -> ()
    end;
    ()

  let start p =
    run p

  let join p =
    ()

end

let () =
  Printf.printf "current pid = %d\n" (Unix.getpid ());
  let f name =
    Printf.printf "current pid = %d\n" (Unix.getpid ());
    Printf.printf "hello %s\n" name
  in
  let p = Process.create ~target:f ~args:"bob" () in
  Process.run p;
  Process.join p;
  ()
