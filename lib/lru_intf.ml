open! Core

module type K = sig
  type t

  include Hashtbl.Key_plain with type t := t
end

module type S = sig
  type key
  type 'a t [@@deriving sexp_of]

  val create : size:int -> unit -> 'a t
  val get : 'a t -> key -> 'a option
  val add : 'a t -> key:key -> data:'a -> unit
  val length : 'a t -> int
end

module type Lru = sig
  module type K = K
  module type S = S

  module Make (K : K) : S with type key = K.t
end
