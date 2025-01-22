open! Core

type ('k, 'v) t

val create : size:int -> 'k Base.Hashtbl.Key.t -> 'v Base.Hashtbl.Key.t -> ('k, 'v) t
val get : ('k, 'v) t -> 'k -> 'v option
val put : ('k, 'v) t -> 'k -> 'v -> unit
val length : ('k, 'v) t -> int
