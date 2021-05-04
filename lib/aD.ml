open Base
include Owl.Algodiff.D

let print_shape ~label x =
  x
  |> shape
  |> Array.to_list
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:" x "
  |> Stdio.printf "[%s] shape = %s\n%!" label


type my_float = float [@@deriving to_yojson]
type my_arr = float array array [@@deriving to_yojson]

let rec to_yojson x =
  if is_float x
  then my_float_to_yojson (unpack_elt x)
  else (
    let x = unpack_arr x in
    if Owl.Arr.numel x > 5
    then
      to_yojson
        (let x = Owl.Arr.reshape x [| 1; -1 |] in
         let x = Owl.Arr.get_slice [ []; [ 0; 4 ] ] x in
         pack_arr x)
    else my_arr_to_yojson (Owl.Mat.to_arrays x))


let numel x = if is_float x then 1 else numel x
let unpack_fun f x = unpack_arr (f (pack_arr x))
let pack_fun f x = pack_arr (f (unpack_arr x))

