let is_none = function
  | None -> true
  | Some _ -> false

let is_some opt =
  not @@ is_none opt

let value_exn = function
  | None -> failwith "none"
  | Some value -> value

let value opt ~default =
  match opt with
  | None -> default
  | Some value -> value

let value_map opt ~default ~f =
  match opt with
  | None -> default
  | Some value -> f value

let iter opt ~f =
  match opt with
  | None -> ()
  | Some value -> f value
