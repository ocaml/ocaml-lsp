type 'a dlist =
  | Empty
  | Non_empty of
      { mutable head : 'a node
      ; mutable end_ : 'a node
      ; mutable length : int
      }

and 'a node =
  { mutable prev : 'a node option
  ; mutable next : 'a node option
  ; data : 'a
  }

type 'a t = 'a dlist ref

let create () = ref Empty

let length (t : 'a t) =
  match !t with
  | Empty -> 0
  | Non_empty { length; _ } -> length

let prepend t data =
  let new_node = { prev = None; next = None; data } in
  match !t with
  | Empty -> t := Non_empty { head = new_node; end_ = new_node; length = 1 }
  | Non_empty ({ head; length; _ } as lst) ->
    new_node.next <- Some head;
    head.prev <- Some new_node;
    lst.head <- new_node;
    lst.length <- length + 1

let append t data =
  let new_node = { prev = None; next = None; data } in
  match !t with
  | Empty -> t := Non_empty { head = new_node; end_ = new_node; length = 1 }
  | Non_empty ({ end_; length; _ } as lst) ->
    new_node.prev <- Some end_;
    lst.end_ <- new_node;
    lst.length <- length + 1

let pop_right_exn t =
  match !t with
  | Empty -> raise @@ Invalid_argument "pop_left_exn on empty dlist"
  | Non_empty ({ head; end_; length } as lst) ->
    lst.length <- length - 1;
    if head = end_ then (
      t := Empty;
      head.data
    ) else
      let data = end_.data in
      lst.end_ <- Option.get end_.prev;
      lst.end_.next <- None;
      data

let drop_one_right_exn t = pop_right_exn t |> ignore

let to_list t =
  match !t with
  | Empty -> []
  | Non_empty { end_; _ } ->
    let rec loop acc = function
      | None -> acc
      | Some node -> loop (node.data :: acc) node.prev
    in
    loop [] @@ Some end_
