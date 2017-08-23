let value_exn = function
  | None -> failwith "none"
  | Some value -> value
