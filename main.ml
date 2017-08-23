open Os

let () =
  Printf.printf "main pid = %d\n%!" (Pid.pid_exn Os.main_pid);
  Printf.printf "parent pid = %d\n%!" (getpid ());
  let f name =
    Printf.printf "current pid = %d\n%!" (getpid ());
    Printf.printf "hello %s\n%!" name;
    failwith "fail"
  in
  Printf.printf "create process\n%!";
  let p = Process.create ~target:f ~args:"bob" () in
  Process.start p;
  Process.join p;
  begin match p.exn with
    | None -> Printf.printf "no exception\n%!"
    | Some exn -> Printf.printf "catch exn\n%!";raise exn
  end;
  Printf.printf "ok\n%!";
  ()
