type t = {
  buf : bytes;
  mutable pos : int;
  len : int;
}

let of_bytes buf =
  { buf; pos = 0; len = Bytes.length buf }


exception Buffer_exhausted

let getbytes src n =
  let {pos; len; _} = src in
  if pos + n <= len then
    (src.pos <- src.pos + n; pos)
  else
    raise Buffer_exhausted

let read_char src =
  let off = getbytes src 1 in
  Bytes.get src.buf off

let read_byte src =
  Char.code (read_char src)

let read_bytes src n =
  let off = getbytes src n in
  Bytes.sub_string src.buf off n

let read_bool src =
  let n = read_byte src in
  n land 1 = 1

let read_int32 src =
  let off = getbytes src 4 in
  EndianBytes.LittleEndian.get_int32 src.buf off

let read_int64 src =
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_int64 src.buf off

let read_float src =
  let off = getbytes src 8 in
  EndianBytes.LittleEndian.get_double src.buf off

let write_char dst ch =
  let off = getbytes dst 1 in
  Bytes.set dst.buf off ch



type mark = bytes * int
let mark { buf; pos; _ } = (buf, pos)

let copy_since_mark dst (buf, pos) len =
  let off = getbytes dst len in
  Bytes.blit buf pos dst.buf off len

let extent_since src (buf, pos) =
  if src.buf <> buf || pos > src.pos then
    failwith "extent_since: invalid mark";
  src.pos - pos
