val find
  :  range:Range.t option
  -> position:Position.t
  -> direction:[< `Next | `Prev ]
  -> Range.t list
  -> Range.t option

val all : ?pipeline_name:string -> Document.Merlin.t -> Range.t list Fiber.t
