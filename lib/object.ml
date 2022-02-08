open Angstrom

type t =
  | File of Property.t list
  | Group of string * Property.t list
  | Channel of
      string * string * Property.t list * Raw_data_blocks.t option * bool

let equal obj = function
  | File _ -> ( match obj with File _ -> true | Channel _ | Group _ -> false)
  | Group (name, _) -> (
      match obj with
      | Group (name', _) -> String.equal name name'
      | File _ | Channel _ -> false)
  | Channel (group_name, name, _, _, _) -> (
      match obj with
      | Channel (group_name', name', _, _, _) ->
          String.equal group_name group_name' && String.equal name name'
      | File _ | Group _ -> false)

let add_primitive_raw_data_block ty primitive_raw_data_block raw_data_blocks =
  match raw_data_blocks with
  | None ->
      Some
        (Raw_data_blocks.PrimitiveRawDataBlocks
           (ty, [ primitive_raw_data_block ]))
  | Some
      (Raw_data_blocks.PrimitiveRawDataBlocks (ty', primitive_raw_data_blocks))
    ->
      Some
        (Raw_data_blocks.PrimitiveRawDataBlocks
           (ty', primitive_raw_data_block :: primitive_raw_data_blocks))
  | Some (Raw_data_blocks.StringRawDataBlocks _) ->
      failwith
        "Object already has type string, cannot add a primitive data block to \
         it; check whether the TDMS file is valid"

let add_string_raw_data_block string_raw_data_block raw_data_blocks =
  match raw_data_blocks with
  | None -> Some (Raw_data_blocks.StringRawDataBlocks [ string_raw_data_block ])
  | Some (Raw_data_blocks.StringRawDataBlocks string_raw_data_blocks) ->
      Some
        (Raw_data_blocks.StringRawDataBlocks
           (string_raw_data_block :: string_raw_data_blocks))
  | Some (Raw_data_blocks.PrimitiveRawDataBlocks _) ->
      failwith
        "Object already has type, cannot add a string data block to it; check \
         whether the TDMS file is valid"

let parse_omitted_raw_data_index raw_data_blocks raw_data_position interleaved =
  match raw_data_blocks with
  | None ->
      failwith
        "Missing raw data index for object; check whether the TDMS file is \
         valid"
  | Some
      (Raw_data_blocks.PrimitiveRawDataBlocks
        (ty, primitive_raw_data_block_list)) -> (
      match primitive_raw_data_block_list with
      | [] ->
          failwith
            "Missing primitive raw data blocks for object; this is a bug in \
             ocaml-tdms"
      | Raw_data_blocks.DecimatedPrimitiveRawDataBlock (_, bytes) :: _ ->
          if interleaved then
            failwith
              "Object raw data changes from decimated to interleaved without a \
               new raw data index; check whether the TDMS file is valid"
          else
            return
              ( (add_primitive_raw_data_block ty
                   (Raw_data_blocks.DecimatedPrimitiveRawDataBlock
                      (raw_data_position, bytes)))
                  raw_data_blocks,
                bytes )
      | Raw_data_blocks.InterleavedPrimitiveRawDataBlock _ :: _ ->
          if not interleaved then
            failwith
              "Object raw data changes from interleaved to decimal without a \
               new raw data index; check whether the TDMS file is valid"
          else return (raw_data_blocks, Stdint.Uint64.of_int (Type.size ty)))
  | Some (Raw_data_blocks.StringRawDataBlocks _) -> fail "Not implemented"

let parse_omitted_raw_data_index' raw_data_blocks raw_data_position big_endian
    interleaved =
  let* _ = if big_endian then BE.int32 0l else LE.int32 0l in
  let+ raw_data_blocks, bytes =
    parse_omitted_raw_data_index raw_data_blocks raw_data_position interleaved
  in
  (raw_data_blocks, bytes)

let parse_primitive_raw_data_index raw_data_blocks raw_data_position big_endian
    interleaved =
  let* _ = if big_endian then BE.int32 20l else LE.int32 20l in
  let* ty = Type.parse big_endian in
  let* _ = if big_endian then BE.int32 1l else LE.int32 1l in
  let+ length = if big_endian then BE.any_int64 else LE.any_int64 in
  if not interleaved then
    ( (add_primitive_raw_data_block ty
         (Raw_data_blocks.DecimatedPrimitiveRawDataBlock
            (raw_data_position, Stdint.Uint64.of_int64 length)))
        raw_data_blocks,
      Stdint.Uint64.( * )
        (Stdint.Uint64.of_int64 length)
        (Stdint.Uint64.of_int (Type.size ty)) )
  else
    ( add_primitive_raw_data_block ty
        (Raw_data_blocks.InterleavedPrimitiveRawDataBlock
           {
             Raw_data_blocks.start = raw_data_position;
             count = Stdint.Uint64.of_int64 length;
             skip = Stdint.Uint64.of_int64 0L;
           })
        raw_data_blocks,
      Stdint.Uint64.of_int (Type.size ty) )

let parse_string_raw_data_index raw_data_blocks raw_data_position big_endian =
  let* _ = if big_endian then BE.int32 28l else LE.int32 28l in
  let* _ = Type.parse big_endian in
  let* _ = if big_endian then BE.any_int32 else LE.any_int32 in
  let* length = if big_endian then BE.any_int64 else LE.any_int64 in
  let+ bytes = if big_endian then BE.any_int64 else LE.any_int64 in
  ( add_string_raw_data_block
      ( raw_data_position,
        Stdint.Uint64.of_int64 length,
        Stdint.Uint64.of_int64 bytes )
      raw_data_blocks,
    Stdint.Uint64.of_int64 bytes )

let parse_daq_mx_raw_data_index big_endian =
  let* _ =
    if big_endian then BE.(int32 0x1269l <|> int32 0x1269l)
    else LE.(int32 0x126Al <|> int32 0x126Al)
  in
  fail "DAQmx raw data not implemented"

let parse_raw_data_index raw_data_blocks raw_data_position big_endian
    interleaved =
  choice
    ~failure_msg:
      "Invalid raw data index length; check whether the TDMS file is valid"
    [
      parse_omitted_raw_data_index' raw_data_blocks raw_data_position big_endian
        interleaved;
      parse_primitive_raw_data_index raw_data_blocks raw_data_position
        big_endian interleaved;
      parse_string_raw_data_index raw_data_blocks raw_data_position big_endian;
      parse_daq_mx_raw_data_index big_endian;
      (if big_endian then BE.int32 0xFFFFFFFFl else LE.int32 0xFFFFFFFFl)
      *> return (raw_data_blocks, raw_data_position);
    ]

let parse_properties big_endian =
  let* length = if big_endian then BE.any_int32 else LE.any_int32 in
  count (Int32.to_int length) (Property.parse big_endian)

let parse_file big_endian =
  let* _ = char '/' in
  let* _ = if big_endian then BE.int32 0xFFFFFFFFl else LE.int32 0xFFFFFFFFl in
  let+ properties = parse_properties big_endian in
  (File properties, Stdint.Uint64.zero)

let parse_component = string "/'" *> many (not_char '\'') <* char '\''

let parse_group big_endian =
  let* group_name = parse_component in
  let* _ = if big_endian then BE.int32 0xFFFFFFFFl else LE.int32 0xFFFFFFFFl in
  let+ properties = parse_properties big_endian in
  ( Group (String.of_seq (List.to_seq group_name), properties),
    Stdint.Uint64.zero )

let parse_channel objects raw_data_position big_endian interleaved =
  let* group_name =
    lift (fun chars -> String.of_seq (List.to_seq chars)) parse_component
  in
  let* channel_name =
    lift (fun chars -> String.of_seq (List.to_seq chars)) parse_component
  in
  let channel =
    List.find_map
      (function
        | Channel (group_name', channel_name', _, _, _) as channel
          when String.equal group_name' group_name
               && String.equal channel_name' channel_name ->
            Some channel
        | Channel _ | File _ | Group _ -> None)
      objects
  in
  let* raw_data_blocks, raw_data_skip =
    parse_raw_data_index
      (Option.bind channel (function
        | Channel (_, _, _, raw_data_blocks, _) -> raw_data_blocks
        | File _ | Group _ -> None))
      raw_data_position big_endian interleaved
  in
  let+ properties = parse_properties big_endian in
  ( Channel (group_name, channel_name, properties, raw_data_blocks, big_endian),
    raw_data_skip )

let parse objects raw_data_position big_endian interleaved =
  let* _ = if big_endian then BE.any_int32 else LE.any_int32 in
  choice ~failure_msg:"Failed to parse object, name length"
    [
      parse_channel objects raw_data_position big_endian interleaved;
      parse_group big_endian;
      parse_file big_endian;
    ]

let to_channel_opt file_path = function
  | Channel (_, name, properties, raw_data_blocks, big_endian) ->
      Some { Channel.name; file_path; properties; raw_data_blocks; big_endian }
  | Group _ | File _ -> None

let to_group_opt file_path channels = function
  | Group (name, properties) ->
      Some
        {
          Group.name;
          properties;
          channels = List.filter_map (to_channel_opt file_path) channels;
        }
  | Channel _ | File _ -> None
