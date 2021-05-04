open Base
open Owl

type 'a tag =
  | Pinned of 'a
  | Learned of 'a
  | Learned_bounded of 'a * float option * float option
[@@deriving yojson]

type t = AD.t tag [@@deriving to_yojson]
type h = AD.t -> t
type setter = ?above:float -> ?below:float -> AD.t -> t

let pinned ?above:_ ?below:_ x = Pinned x

let learned ?above ?below x =
  match above, below with
  | None, None -> Learned x
  | _ -> Learned_bounded (x, above, below)


let numel = function
  | Pinned x -> AD.numel x
  | Learned x -> AD.numel x
  | Learned_bounded (x, _, _) -> AD.numel x


let pin = function
  | Pinned x -> Pinned x
  | Learned x -> Pinned x
  | Learned_bounded (x, _, _) -> Pinned x


let extract = function
  | Pinned x -> x
  | Learned x -> x
  | Learned_bounded (x, _, _) -> x


let map f = function
  | Pinned x -> Pinned (f x)
  | Learned x -> Learned (f x)
  | Learned_bounded (x, lb, ub) -> Learned_bounded (f x, lb, ub)


module type Packer = sig
  val pack : ?f:(AD.t -> AD.t) * (AD.t -> AD.t) -> t -> h
  val finalize : Unit.t -> AD.t * Owl.Mat.mat option * Owl.Mat.mat option
end

module Packer () = struct
  let i = ref (-1)
  let v = ref []
  let lb = ref []
  let ub = ref []

  let finalize () =
    match !v with
    | [] -> AD.Mat.empty 0 0, None, None
    | z ->
      let theta = z |> List.rev |> Array.of_list |> Mat.concatenate ~axis:1 in
      let lb =
        match !lb with
        | [] -> None
        | z ->
          let lb = Mat.create 1 (Mat.numel theta) Float.neg_infinity in
          List.iter z ~f:(fun (i, len, b) ->
              let b = Mat.create 1 len b in
              Mat.set_slice [ []; [ i; i + len - 1 ] ] lb b);
          Some lb
      in
      let ub =
        match !ub with
        | [] -> None
        | z ->
          let ub = Mat.create 1 (Mat.numel theta) Float.infinity in
          List.iter z ~f:(fun (i, len, b) ->
              let b = Mat.create 1 len b in
              Mat.set_slice [ []; [ i; i + len - 1 ] ] ub b);
          Some ub
      in
      AD.pack_arr theta, lb, ub


  let rec pack ?f prm =
    let open AD in
    match prm with
    | Pinned x -> fun _ -> Pinned x
    | Learned (F x) ->
      Int.incr i;
      let x =
        match f with
        | Some (f, _) -> unpack_elt (f (F x))
        | None -> x
      in
      v := Owl.Mat.create 1 1 x :: !v;
      let ii = !i in
      fun theta ->
        let y = Maths.get_item theta 0 ii in
        let y =
          match f with
          | Some (_, f) -> f y
          | None -> y
        in
        Learned y
    | Learned_bounded (F x, l, u) ->
      Option.iter l ~f:(fun l -> lb := (!i + 1, 1, l) :: !lb);
      Option.iter u ~f:(fun u -> ub := (!i + 1, 1, u) :: !ub);
      let ft = pack ?f (Learned (F x)) in
      fun theta -> Learned_bounded (extract (ft theta), l, u)
    | Learned (Arr x) ->
      Int.incr i;
      let s = Owl.Arr.shape x in
      let x =
        match f with
        | Some (f, _) -> unpack_arr (f (Arr x))
        | None -> x
      in
      let x = Owl.Mat.reshape x [| 1; -1 |] in
      v := x :: !v;
      let ii = !i in
      let f theta =
        let y = Maths.get_slice [ [ 0 ]; [ ii; ii + Owl.Mat.numel x - 1 ] ] theta in
        let y = Maths.reshape y s in
        let y =
          match f with
          | Some (_, f) -> f y
          | None -> y
        in
        Learned y
      in
      i := !i + Owl.Mat.numel x - 1;
      f
    | Learned_bounded (Arr x, l, u) ->
      Option.iter l ~f:(fun l -> lb := (!i + 1, Owl.Arr.numel x, l) :: !lb);
      Option.iter u ~f:(fun u -> ub := (!i + 1, Owl.Arr.numel x, u) :: !ub);
      let ft = pack ?f (Learned (Arr x)) in
      fun theta -> Learned_bounded (extract (ft theta), l, u)
    | _ -> assert false
end

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

module Make (B : Basic) = struct
  include B

  type p = t prm
  type ph = h prm

  let pack (module P : Packer) prms = map prms ~f:P.pack
  let unpack (h : ph) v = map h ~f:(fun h -> h v)

  let save_to_files ?prefix ~(prms : p) =
    fold ?prefix prms ~init:() ~f:(fun () (prm, descr) ->
        let prm = extract prm |> AD.primal' in
        let prm =
          match prm with
          | F x -> Mat.create 1 1 x
          | Arr x -> x
          | _ -> assert false
        in
        let prm = if Mat.row_num prm = 1 then Mat.transpose prm else prm in
        Mat.save_txt ~out:descr prm;
        ())
end

let with_prefix ?prefix s =
  match prefix with
  | None -> s
  | Some p -> p ^ "." ^ s

