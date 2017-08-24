open Printf

let () =
  printf "parent pid = %d\n%!" (Os.pid_value ());
  let f name () =
    printf "\ncurrent pid = %d\n%!" (Os.pid_value ());
    printf "hello %s\n\n%!" name;
    failwith "fail"
  in
  printf "create process\n%!";
  let p = Process.create () in
  Process.start p ~f:(f "bob");
  printf "started in %d\n%!" (Os.pid_value ());
  Process.join p;
  begin match p.exit_status with
    | Some (Exit_exn exn) -> printf "catch exn\n%!";raise exn
    | _ -> printf "no exception\n%!"
  end;
  printf "ok\n%!";
  ()
