open! Core

module Lru = Lru

module Vec : sig
  type t = int array

  (** Include the `Stringable` trait basically that now has to be implemented in problems.ml **)
  include Stringable.S with type t := t
end

val command : Command.t
