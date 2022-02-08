open Angstrom

type tag = Tdsm | Tdsh

let contains_metadata_flag = 2l

let contains_raw_data_flag = 8l

let contains_daq_mx_raw_data_flag = 128l

let contains_interleaved_data_flag = 32l

let contains_big_endian_data_flag = 64l

let contains_new_object_list_flag = 4l

type version = OnePointOh | TwoPointOh

type table_of_contents = {
  contains_metadata : bool;
  contains_raw_data : bool;
  contains_daq_mx_raw_data : bool;
  contains_interleaved_data : bool;
  contains_big_endian_data : bool;
  contains_new_object_list : bool;
}

type lead_in = {
  tag : tag;
  table_of_contents : table_of_contents;
  version : version;
  next_segment_offset : Stdint.uint64;
  raw_data_offset : Stdint.uint64;
}

let tdsm_tag = 0x5444536Dl

let tdsh_tag = 0x54445368l

let parse_tag =
  BE.int32 tdsm_tag *> return Tdsm
  <|> BE.int32 tdsh_tag *> return Tdsh
  <?> "TDSm or TDSh tag"

let is_flag_set flag flags = Int32.equal flag (Int32.logand flag flags)

let parse_table_of_contents : table_of_contents t =
  map BE.any_int32 ~f:(fun flags ->
      {
        contains_metadata = is_flag_set contains_metadata_flag flags;
        contains_raw_data = is_flag_set contains_raw_data_flag flags;
        contains_daq_mx_raw_data =
          is_flag_set contains_daq_mx_raw_data_flag flags;
        contains_interleaved_data =
          is_flag_set contains_interleaved_data_flag flags;
        contains_big_endian_data =
          is_flag_set contains_big_endian_data_flag flags;
        contains_new_object_list =
          is_flag_set contains_new_object_list_flag flags;
      })
  <?> "Table of contents"

let parse_version big_endian =
  let parse_int32 = if big_endian then BE.int32 else LE.int32 in
  parse_int32 4712l *> return OnePointOh
  <|> parse_int32 4713l *> return TwoPointOh
  <?> "TDMS file format version"

let parse_lead_in =
  let* tag = parse_tag in
  let* ({ contains_big_endian_data; _ } as table_of_contents) =
    parse_table_of_contents
  in
  let* version = parse_version contains_big_endian_data in
  let* next_segment_offset =
    map ~f:Stdint.Uint64.of_int64
      (if contains_big_endian_data then BE.any_int64 else LE.any_int64)
  in
  let+ raw_data_offset =
    map ~f:Stdint.Uint64.of_int64
      (if contains_big_endian_data then BE.any_int64 else LE.any_int64)
  in
  { tag; table_of_contents; version; next_segment_offset; raw_data_offset }

let add_or_update_object obj objects =
  match List.find_opt (Object.equal obj) objects with
  | None -> obj :: objects
  | Some _ -> (
      match obj with
      | Object.File properties ->
          List.map
            (function
              | Object.File properties' ->
                  Object.File (List.append properties properties')
              | (Object.Channel _ | Object.Group _) as obj -> obj)
            objects
      | Object.Group (name, properties) ->
          List.map
            (function
              | Object.Group (name', properties') when String.equal name' name
                ->
                  Object.Group (name', List.append properties properties')
              | (Object.Channel _ | Object.File _ | Object.Group _) as obj ->
                  obj)
            objects
      | Object.Channel (group_name, name, properties, raw_data_blocks, _) ->
          List.map
            (function
              | Object.Channel (group_name', name', properties', _, big_endian')
                when String.equal group_name' group_name
                     && String.equal name' name ->
                  Object.Channel
                    ( group_name',
                      name',
                      List.append properties properties',
                      raw_data_blocks,
                      big_endian' )
              | (Object.Channel _ | Object.File _ | Object.Group _) as obj ->
                  obj)
            objects)

let parse_objects count objects _raw_data_offset _next_segment_offset big_endian
    interleaved =
  let rec loop todo objects' raw_data_skip =
    if todo = 0l then return objects'
    else
      let* obj, raw_data_skip' =
        Object.parse objects' raw_data_skip big_endian interleaved
      in
      loop (Int32.sub todo 1l)
        (add_or_update_object obj objects')
        raw_data_skip'
  in
  loop count objects Stdint.Uint64.zero

let parse_metadata objects raw_data_offset _next_segment_offset big_endian
    interleaved =
  let* object_count = if big_endian then BE.any_int32 else LE.any_int32 in
  parse_objects object_count objects raw_data_offset _next_segment_offset
    big_endian interleaved

let parse objects =
  let* {
         raw_data_offset;
         next_segment_offset;
         table_of_contents =
           { contains_big_endian_data; contains_interleaved_data; _ };
         _;
       } =
    parse_lead_in
  in
  let* objects' =
    parse_metadata objects raw_data_offset next_segment_offset
      contains_big_endian_data contains_interleaved_data
  in
  advance
    (Stdint.Uint64.to_int
       (Stdint.Uint64.( - ) next_segment_offset raw_data_offset))
  *> return objects'
