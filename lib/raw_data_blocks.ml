open Angstrom

type format_changing_scaler = {
  daq_mx_raw_data_type : Stdint.uint32;
  raw_buffer_index : Stdint.uint32;
  raw_byte_offset_within_stride : Stdint.uint32;
  sample_format_bitmap : Stdint.uint32;
  scale_id : Stdint.uint32;
}

type interleaved_primitive_raw_data_block = {
  start : Stdint.uint64;
  count : Stdint.uint64;
  skip : Stdint.uint64;
}

type primitive_raw_data_block =
  | DecimatedPrimitiveRawDataBlock of (Stdint.uint64 * Stdint.uint64)
  | InterleavedPrimitiveRawDataBlock of interleaved_primitive_raw_data_block

type t =
  | PrimitiveRawDataBlocks of Type.t * primitive_raw_data_block list
  | StringRawDataBlocks of (Stdint.uint64 * Stdint.uint64 * Stdint.uint64) list

let add_primitive_raw_data_block ty primitive_raw_data_block = function
  | PrimitiveRawDataBlocks (ty', primitive_raw_data_blocks) when ty = ty' ->
      PrimitiveRawDataBlocks
        (ty', primitive_raw_data_block :: primitive_raw_data_blocks)
  | PrimitiveRawDataBlocks _ ->
      failwith
        "Object already has type, cannot add a primitive data block of another \
         type to it; check whether the TDMS file is valid"
  | StringRawDataBlocks _ ->
      failwith
        "Object already has type string, cannot add a primitive data block to \
         it; check whether the TDMS file is valid"

let add_string_raw_data_block string_raw_data_block = function
  | StringRawDataBlocks string_raw_data_blocks ->
      StringRawDataBlocks (string_raw_data_block :: string_raw_data_blocks)
  | PrimitiveRawDataBlocks _ ->
      failwith
        "Object already has primitive type, cannot add a string data block to \
         it; check whether the TDMS file is valid"

let primitive_raw_data_block_size = function
  | DecimatedPrimitiveRawDataBlock (_, size) -> size
  | InterleavedPrimitiveRawDataBlock { count; _ } -> count

let string_raw_data_block_size (_, _, _) = Stdint.Uint64.zero

let size = function
  | PrimitiveRawDataBlocks (_, primitive_raw_data_blocks) ->
      List.map primitive_raw_data_block_size primitive_raw_data_blocks
      |> List.fold_left Stdint.Uint64.( + ) Stdint.Uint64.zero
  | StringRawDataBlocks string_raw_data_blocks ->
      List.map string_raw_data_block_size string_raw_data_blocks
      |> List.fold_left Stdint.Uint64.( + ) Stdint.Uint64.zero

let count_bigarray n offset bigarray p =
  let rec loop index =
    if index = n then return ()
    else
      let* result = p in
      Bigarray.Array1.set bigarray (offset + index) result;
      loop (index + 1)
  in
  loop 0

let parse_raw_data_block_float_bigarray big_endian offset bigarray = function
  | DecimatedPrimitiveRawDataBlock (file_offset, size) ->
      advance (Stdint.Uint64.to_int file_offset)
      *> count_bigarray
           (Stdint.Uint64.to_int size)
           offset bigarray
           (if big_endian then BE.any_double else LE.any_double)
  | InterleavedPrimitiveRawDataBlock _ ->
      failwith "Interleaved raw data not implemented"

let parse_raw_data_float_bigarray big_endian offset bigarray = function
  | PrimitiveRawDataBlocks (_, primitive_raw_data_blocks) ->
      let rec loop = function
        | [] -> return ()
        | primitive_raw_data_block :: primitive_raw_data_blocks'' ->
            let* _ =
              parse_raw_data_block_float_bigarray big_endian offset bigarray
                primitive_raw_data_block
            in
            loop primitive_raw_data_blocks''
      in
      loop primitive_raw_data_blocks
  | StringRawDataBlocks _ -> return ()
