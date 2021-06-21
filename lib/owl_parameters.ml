open Base
open Owl

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

let pinned ?above:_ ?below:_ x = Pinned x

let learned ?above ?below x =
  match above, below with
  | None, None -> Learned x
  | _ -> Learned_bounded (x, above, below)


let numel (_, x) =
  match x with
  | Pinned x -> AD.numel x
  | Learned x -> AD.numel x
  | Learned_bounded (x, _, _) -> AD.numel x


let pin (p, x) =
  ( p
  , match x with
    | Pinned x -> Pinned x
    | Learned x -> Pinned x
    | Learned_bounded (x, _, _) -> Pinned x )


let extract (_, x) =
  match x with
  | Pinned x -> x
  | Learned x -> x
  | Learned_bounded (x, _, _) -> x


let map f (p, x) =
  ( p
  , match x with
    | Pinned x -> Pinned (f x)
    | Learned x -> Learned (f x)
    | Learned_bounded (x, lb, ub) -> Learned_bounded (f x, lb, ub) )


module type Packer = sig
  val pack : ?f:(AD.t -> AD.t) * (AD.t -> AD.t) -> t -> h

  val finalize
    :  Unit.t
    -> AD.t
       * Owl.Mat.mat option
       * Owl.Mat.mat option
       * (Owl.Mat.mat -> unit)
       * (Owl.Mat.mat -> unit)
end

module Packer () = struct
  let i = ref (-1)
  let v = ref []
  let lb = ref []
  let ub = ref []
  let shared = ref []

  let share shared =
    let shared = Array.of_list shared in
    fun theta ->
      shared
      |> Array.map ~f:(fun slice -> Owl.Mat.get_slice slice theta)
      |> C.broadcast
      |> Array.iteri ~f:(fun i xi -> Owl.Mat.set_slice shared.(i) theta xi)


  let collect_gradients shared =
    let shared = Array.of_list shared in
    fun theta ->
      shared
      |> Array.map ~f:(fun slice -> Owl.Mat.get_slice slice theta)
      |> C.allgather (* n_nodes x n_slices *)
      |> Array.transpose
      |> (fun v -> Option.value_exn v)
      |> Array.iteri ~f:(fun i xi ->
             let xi = xi |> Owl.Mat.concatenate ~axis:0 |> Owl.Mat.mean ~axis:0 in
             Owl.Mat.set_slice shared.(i) theta xi)


  let finalize () =
    match !v with
    | [] -> AD.Mat.empty 0 0, None, None, (fun _ -> ()), fun _ -> ()
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
      AD.pack_arr theta, lb, ub, share !shared, collect_gradients !shared


  let add_shared_slice p s =
    match p with
    | Global -> shared := s :: !shared
    | Local -> ()


  let rec pack ?f (p, prm) =
    let open AD in
    match prm with
    | Pinned x -> fun _ -> p, Pinned x
    | Learned (F x) ->
      Int.incr i;
      let x =
        match f with
        | Some (f, _) -> unpack_elt (f (F x))
        | None -> x
      in
      v := Owl.Mat.create 1 1 x :: !v;
      let ii = !i in
      add_shared_slice p [ [ 0 ]; [ ii ] ];
      fun theta ->
        let y = Maths.get_item theta 0 ii in
        let y =
          match f with
          | Some (_, f) -> f y
          | None -> y
        in
        p, Learned y
    | Learned_bounded (F x, l, u) ->
      Option.iter l ~f:(fun l -> lb := (!i + 1, 1, l) :: !lb);
      Option.iter u ~f:(fun u -> ub := (!i + 1, 1, u) :: !ub);
      let ft = pack ?f (p, Learned (F x)) in
      fun theta -> p, Learned_bounded (extract (ft theta), l, u)
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
      let n = Owl.Mat.numel x in
      add_shared_slice p [ [ 0 ]; [ ii; ii + n - 1 ] ];
      let f theta =
        let y = Maths.get_slice [ [ 0 ]; [ ii; ii + n - 1 ] ] theta in
        let y = Maths.reshape y s in
        let y =
          match f with
          | Some (_, f) -> f y
          | None -> y
        in
        p, Learned y
      in
      i := !i + Owl.Mat.numel x - 1;
      f
    | Learned_bounded (Arr x, l, u) ->
      Option.iter l ~f:(fun l -> lb := (!i + 1, Owl.Arr.numel x, l) :: !lb);
      Option.iter u ~f:(fun u -> ub := (!i + 1, Owl.Arr.numel x, u) :: !ub);
      let ft = pack ?f (p, Learned (Arr x)) in
      fun theta -> p, Learned_bounded (extract (ft theta), l, u)
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
    fold ?prefix prms ~init:() ~f:(fun () ((pos, prm), descr) ->
        let prm = extract (pos, prm) |> AD.primal' in
        let prm =
          match prm with
          | F x -> Mat.create 1 1 x
          | Arr x -> x
          | _ -> assert false
        in
        let prm = if Mat.row_num prm = 1 then Mat.transpose prm else prm in
        match pos with
        | Global -> C.root_perform (fun () -> Mat.save_txt ~out:descr prm)
        | Local -> Mat.save_txt ~out:(descr ^ ".node" ^ Int.to_string C.rank) prm)
end

let with_prefix ?prefix s =
  match prefix with
  | None -> s
  | Some p -> p ^ "." ^ s


module Empty = Make (struct
  type 'a prm = unit

  let map ~f:_ () = ()
  let fold ?prefix:_ ~init ~f:_ () = init
end)

module Single (X : sig
  val label : string
end) =
Make (struct
  type 'a prm = 'a

  let map ~f x = f x
  let fold ?prefix ~init ~f x = f init (x, with_prefix ?prefix X.label)
end)
