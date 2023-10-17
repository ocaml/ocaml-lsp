module Option = struct
  type 'a t = 'a option = None | Some of 'a

  let is_some = function None -> false | Some _ -> true
  let value ~default = function None -> default | Some x -> x

  let join_list l =
    let rec loop acc = function
      | [] -> Some (List.rev acc)
      | Some a :: q -> loop (a :: acc) q
      | None :: _ -> None
    in
    loop [] l
end

module Char = struct
  include Char

  let equal (x : char) y = x = y
end

module String = struct
  include String

  let for_all f str =
    let rec aux i =
      if i >= String.length str then true
      else if f (String.get str i) then aux (i + 1)
      else false
    in
    aux 0
end
