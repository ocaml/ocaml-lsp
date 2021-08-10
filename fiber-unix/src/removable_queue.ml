type 'a node = 'a Doubly_linked.node

type 'a t = 'a Doubly_linked.t

let create = Doubly_linked.create

let is_empty = Doubly_linked.is_empty

let push = Doubly_linked.append

let pop = Doubly_linked.detach_head

let remove node = Doubly_linked.detach node |> ignore
