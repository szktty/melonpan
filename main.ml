open Unix

let () =
  Printf.printf "base pid = %d\n" Process.base_pid;
  Printf.printf "parent pid = %d\n" (getpid ());
  let f name =
    Printf.printf "current pid = %d\n" (getpid ());
    Printf.printf "hello %s\n" name;
  in
  let p = Process.create
      ~parent_pid:(Some (getpid ()))
      ~target:f ~args:"bob" () in
  Process.start p;
  Process.join p;
  Printf.printf "ok\n";
  ()
