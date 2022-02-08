open Angstrom

type t =
  | Void
  | Bool
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | SingleFloat
  | SingleFloatWithUnit
  | DoubleFloat
  | DoubleFloatWithUnit
  | ComplexSingleFloat
  | ComplexDoubleFloat
  | ExtendedFloat
  | ExtendedFloatWithUnit
  | String
  | Timestamp

let parse big_endian =
  let int32 = if big_endian then BE.int32 else LE.int32 in
  choice
    [
      int32 0x0l *> return Void;
      int32 0x21l *> return Bool;
      int32 1l *> return I8;
      int32 2l *> return I16;
      int32 3l *> return I32;
      int32 4l *> return I64;
      int32 5l *> return U8;
      int32 6l *> return U16;
      int32 7l *> return U32;
      int32 8l *> return U64;
      int32 9l *> return SingleFloat;
      int32 0x19l *> return SingleFloatWithUnit;
      int32 10l *> return DoubleFloat;
      int32 0x1Al *> return DoubleFloatWithUnit;
      int32 0x08000Cl *> return ComplexSingleFloat;
      int32 0x10000Dl *> return ComplexDoubleFloat;
      int32 11l *> return ExtendedFloat;
      int32 0x1Bl *> return ExtendedFloatWithUnit;
      int32 0x20l *> return String;
      int32 0x44l *> return Timestamp;
    ]
  <?> "TDMS property type"

let size = function
  | Void | Bool | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 | SingleFloat | SingleFloatWithUnit -> 4
  | I64 | U64 | DoubleFloat | DoubleFloatWithUnit | ComplexSingleFloat -> 8
  | ComplexDoubleFloat | ExtendedFloat | ExtendedFloatWithUnit | Timestamp -> 16
  | String -> failwith "Strings are of variable length"

(*let to_bigarray_elt_opt = function | SingleFloat | SingleFloatWithUnit -> Some
  Bigarray.float32_elt | DoubleFloat | DoubleFloatWithUnit -> Some
  Bigarray.float64_elt | ComplexSingleFloat -> Some Bigarray.complex32_elt |
  ComplexDoubleFloat -> Some Bigarray.complex64_elt *)
