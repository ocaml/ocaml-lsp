open Core
open Async

(** Interoperation between fibers and Async. This library assumes that the outer program
    is running with Async and that the fibers will be interpreted by [deferred_of_fiber].

    The most important difference between fibers and deferreds is that fibers are just
    continuations and do not store the value they compute, so if you bind twice on a given
    fiber the computation will be run twice. A [Fiber.Ivar.t] can be used to save the
    result of a computation. *)

module Fiber : sig
  include module type of struct
    include Fiber
  end

  include Monad.S with type 'a t := 'a t
end

(** Convert a fiber to a computation that returns a deferred when run. *)
val deferred_of_fiber : 'a Fiber.t -> (unit -> 'a Deferred.t)

(** Convert a deferred to a fiber that stores the result in a [Fiber.Ivar.t]. This fiber
    can only be interpreted by [deferred_of_fiber] - using a different scheduler to run it
    will fail. *)
val fiber_of_deferred : 'a Deferred.t -> 'a Fiber.t
