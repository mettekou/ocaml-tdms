open Angstrom

type t = {
  name : string;
  file_path : string;
  properties : Property.t list;
  raw_data_blocks : Raw_data_blocks.t option;
  big_endian : bool;
}

let init name file_path properties raw_data_blocks big_endian =
  { name; file_path; properties; raw_data_blocks; big_endian }

let find_property_opt name' { properties; _ } =
  List.find_opt (fun { Property.name; _ } -> name = name') properties

let parse_raw_data_float_bigarray_opt { file_path; raw_data_blocks; _ } =
  Option.map
    (fun raw_data_blocks' ->
      let size = Raw_data_blocks.size raw_data_blocks' in
      let _raw_data =
        Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout
          (Stdint.Uint64.to_int size)
      in
      ())
    raw_data_blocks
