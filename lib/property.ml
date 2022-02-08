open Stdint
open Angstrom

type property_value =
  | VoidPropertyValue
  | BoolPropertyValue of bool
  | I8PropertyValue of int
  | I16PropertyValue of int
  | I32PropertyValue of int32
  | I64PropertyValue of int64
  | U8PropertyValue of int
  | U16PropertyValue of int
  | U32PropertyValue of Stdint.uint32
  | U64PropertyValue of Stdint.uint64
  | SingleFloatPropertyValue of float
  | DoubleFloatPropertyValue of float
  | ComplexSingleFloatPropertyValue of Complex.t
  | ComplexDoubleFloatPropertyValue of Complex.t
  | StringPropertyValue of string
  | TimestampPropertyValue of Timestamp.t

let parse_value big_endian =
  Type.parse big_endian >>= function
  | Type.Void -> return VoidPropertyValue
  | Type.Bool ->
      map ~f:(fun value -> BoolPropertyValue (Int.equal value 0)) any_int8
  | Type.I8 -> map ~f:(fun value -> I8PropertyValue value) any_int8
  | Type.I16 ->
      map
        ~f:(fun value -> I16PropertyValue value)
        (if big_endian then BE.any_int16 else LE.any_int16)
  | Type.I32 ->
      map
        ~f:(fun value -> I32PropertyValue value)
        (if big_endian then BE.any_int32 else LE.any_int32)
  | Type.I64 ->
      map
        ~f:(fun value -> I64PropertyValue value)
        (if big_endian then BE.any_int64 else LE.any_int64)
  | Type.U8 -> map ~f:(fun value -> U8PropertyValue value) any_uint8
  | Type.U16 ->
      map
        ~f:(fun value -> U16PropertyValue value)
        (if big_endian then BE.any_uint16 else LE.any_uint16)
  | Type.U32 ->
      map
        ~f:(fun value -> U32PropertyValue (Uint32.of_int32 value))
        (if big_endian then BE.any_int32 else LE.any_int32)
  | Type.U64 ->
      map
        ~f:(fun value -> U64PropertyValue (Uint64.of_int64 value))
        (if big_endian then BE.any_int64 else LE.any_int64)
  | Type.SingleFloat | Type.SingleFloatWithUnit ->
      map
        ~f:(fun value -> SingleFloatPropertyValue value)
        (if big_endian then BE.any_float else LE.any_float)
  | Type.DoubleFloat | Type.DoubleFloatWithUnit ->
      map
        ~f:(fun value -> DoubleFloatPropertyValue value)
        (if big_endian then BE.any_double else LE.any_double)
  | Type.ComplexSingleFloat ->
      let float = if big_endian then BE.any_float else LE.any_float in
      let* real = float in
      let+ imaginary = float in
      ComplexSingleFloatPropertyValue
        { Complex.re = real; Complex.im = imaginary }
  | Type.ComplexDoubleFloat ->
      let double = if big_endian then BE.any_double else LE.any_double in
      let* real = double in
      let+ imaginary = double in
      ComplexDoubleFloatPropertyValue
        { Complex.re = real; Complex.im = imaginary }
  | Type.String ->
      let* length = if big_endian then BE.any_int32 else LE.any_int32 in
      let+ value = take (Int32.to_int length) in
      StringPropertyValue value
  | Type.ExtendedFloat | Type.ExtendedFloatWithUnit ->
      failwith "Not implemented"
  | Type.Timestamp ->
      if big_endian then
        BE.(
          let* seconds_since_ni_epoch = any_int64 in
          let+ fractions_of_a_second = any_int64 in
          TimestampPropertyValue
            {
              seconds_since_ni_epoch;
              Timestamp.fractions_of_a_second =
                Int64.to_uint64 fractions_of_a_second;
            })
      else
        LE.(
          let* fractions_of_a_second = any_int64 in
          let+ seconds_since_ni_epoch = any_int64 in
          TimestampPropertyValue
            {
              seconds_since_ni_epoch;
              Timestamp.fractions_of_a_second =
                Int64.to_uint64 fractions_of_a_second;
            })

type t = { name : string; value : property_value }

let parse big_endian =
  let* length = if big_endian then BE.any_int32 else LE.any_int32 in
  let* name = take (Int32.to_int length) in
  let+ value = parse_value big_endian in
  { name; value }

let get_value_bool_opt { value; _ } =
  match value with
  | BoolPropertyValue value' -> Some value'
  | VoidPropertyValue | I8PropertyValue _ | I16PropertyValue _
  | I32PropertyValue _ | I64PropertyValue _ | U8PropertyValue _
  | U16PropertyValue _ | U32PropertyValue _ | U64PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_int_opt { value; _ } =
  match value with
  | I8PropertyValue value' -> Some value'
  | I16PropertyValue value' -> Some value'
  | U8PropertyValue value' -> Some value'
  | U16PropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I32PropertyValue _
  | I64PropertyValue _ | U32PropertyValue _ | U64PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_int32_opt { value; _ } =
  match value with
  | I32PropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I64PropertyValue _ | U8PropertyValue _
  | U16PropertyValue _ | U32PropertyValue _ | U64PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_int64_opt { value; _ } =
  match value with
  | I64PropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | U8PropertyValue _
  | U16PropertyValue _ | U32PropertyValue _ | U64PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_uint32_opt { value; _ } =
  match value with
  | U32PropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U64PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_uint64_opt { value; _ } =
  match value with
  | U64PropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U32PropertyValue _
  | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_float_opt { value; _ } =
  match value with
  | SingleFloatPropertyValue value' -> Some value'
  | DoubleFloatPropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U32PropertyValue _
  | U64PropertyValue _ | ComplexSingleFloatPropertyValue _
  | ComplexDoubleFloatPropertyValue _ | StringPropertyValue _
  | TimestampPropertyValue _ ->
      None

let get_value_complex_opt { value; _ } =
  match value with
  | ComplexSingleFloatPropertyValue value' -> Some value'
  | ComplexDoubleFloatPropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U32PropertyValue _
  | U64PropertyValue _ | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | StringPropertyValue _ | TimestampPropertyValue _ ->
      None

let get_value_string_opt { value; _ } =
  match value with
  | StringPropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U32PropertyValue _
  | U64PropertyValue _ | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | TimestampPropertyValue _ ->
      None

let get_value_timestamp_opt { value; _ } =
  match value with
  | TimestampPropertyValue value' -> Some value'
  | VoidPropertyValue | BoolPropertyValue _ | I8PropertyValue _
  | I16PropertyValue _ | I32PropertyValue _ | I64PropertyValue _
  | U8PropertyValue _ | U16PropertyValue _ | U32PropertyValue _
  | U64PropertyValue _ | SingleFloatPropertyValue _ | DoubleFloatPropertyValue _
  | ComplexSingleFloatPropertyValue _ | ComplexDoubleFloatPropertyValue _
  | StringPropertyValue _ ->
      None
