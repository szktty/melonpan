open Printf

let test1 () =
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

let test2 () =
  let info title =
    printf "%s\n%!" title;
    printf "(parent process: %d)\n%!" (Os.parent_pid_value_exn ());
    printf "(process id: %d)\n%!" (Os.pid_value ())
  in
  let f name () =
    info "function f";
    printf "(hello, %s)\n%!" name
  in
  info "main line";
  let p = Process.create () in
  info "start";
  Process.start p ~f:(f "bob");
  info "join";
  Process.join p;
  info "end"

let () =
  test2 ()
