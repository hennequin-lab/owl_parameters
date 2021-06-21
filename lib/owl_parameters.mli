open Base

type pos =
  | Global
  | Local
[@@deriving yojson]

type 'a tag =
  | Pinned of 'a
  | Learned of 'a
  | Learned_bounded of 'a * float option * float option
[@@deriving yojson]

type t = pos * AD.t tag [@@deriving to_yojson]
type h = AD.t -> t
type setter = ?above:float -> ?below:float -> AD.t -> AD.t tag

val pinned : setter
val learned : setter
val numel : t -> int
val pin : t -> t
val extract : pos * 'a tag -> 'a
val map : ('a -> 'b) -> pos * 'a tag -> pos * 'b tag

module type Packer = sig
  val pack : ?f:(AD.t -> AD.t) * (AD.t -> AD.t) -> t -> h
  val finalize : Unit.t -> AD.t * Owl.Mat.mat option * Owl.Mat.mat option
end

module Packer () : Packer

module type Basic = sig
  type 'a prm

  val map : f:('a -> 'b) -> 'a prm -> 'b prm
  val fold : ?prefix:String.t -> init:'a -> f:('a -> 'b * String.t -> 'a) -> 'b prm -> 'a
end

module type T = sig
  include Basic

  type p = t prm
  type ph = h prm

  val pack : (module Packer) -> p -> ph
  val unpack : ph -> AD.t -> p
  val save_to_files : ?prefix:String.t -> prms:p -> unit
end

val with_prefix : ?prefix:String.t -> String.t -> String.t

module Make (B : Basic) : T with type 'a prm = 'a B.prm
module Empty : T with type 'a prm = unit

module Single (X : sig
  val label : string
end) : T with type 'a prm = 'a
