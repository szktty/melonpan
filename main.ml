open Unix

let () =
  Printf.printf "base pid = %d\n%!" Process.base_pid;
  Printf.printf "parent pid = %d\n%!" (getpid ());
  let f name =
    Printf.printf "current pid = %d\n%!" (getpid ());
    Printf.printf "hello %s\n%!" name;
    failwith "fail"
  in
  let p = Process.create
      ~parent_pid:(Some (getpid ()))
      ~target:f ~args:"bob" () in
  Printf.printf "child %d\n%!" p.pid;
  Process.start p;
  Process.join p;
  begin match p.exn with
    | None -> Printf.printf "no exception\n%!"
    | Some exn -> Printf.printf "catch exn\n%!";raise exn
  end;
  Printf.printf "ok\n%!";
  ()
