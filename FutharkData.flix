namespace FutharkData {
    ///
    /// The shape of a value. Scalars have an empty shape.
    ///
    pub type alias Shape = List[Int64]
    
    /// Thhe element type.
    pub enum ElemType with ToString {
        case I8
        case I16
        case I32
        case I64
        case U8
        case U16
        case U32
        case U64
        case F16
        case F32
        case F64
        case Bool
    }
    
    ///
    /// A shape along with an element type.
    ///
    pub type alias ValueType = (Shape,ElemType)

    ///
    /// An array of bytes.
    ///
    pub type alias Bytes = Array[Int8,Impure]

    ///
    /// A value of a pair of the type of the value and its representation as a byte array.
    ///
    opaque enum Value with ToString { case Value(ValueType, Bytes) }

    ///
    /// The type of a value.
    ///
    pub def valueType(v: Value): ValueType =
        let Value(t,_) = v;
        t

    ///
    /// The size of a scalar value of this element type.
    ///
    pub def elemTypeSize(t: ElemType): Int64 =
        match t {
            case Bool => 1
            case F16 => 2
            case F32 => 4
            case F64 => 8
            case I8 => 1
            case I16 => 2
            case I32 => 4
            case I64 => 8
            case U8 => 1
            case U16 => 2
            case U32 => 4
            case U64 => 8
        } |> Int32.toInt64

    def product(l: List[Int64]): Int64 =
        List.foldLeft((x, y) -> x*y, 1i64, l)

    ///
    /// The size of a value of this type (not counting header, only raw data).
    ///
    pub def valueTypeSize(t: ValueType): Int64 =
        product(fst(t)) * elemTypeSize(snd(t))

    ///
    /// The element type of a value.
    ///
    pub def valueElemType(v: Value): ElemType =
        snd(valueType(v))

    pub def valueShape(v: Value): Shape =
        fst(valueType(v))

    pub def valueBytes(v: Value): Bytes =
        let Value(_, bs) = v;
        bs

    ///
    /// Construct a value from a type and its representation as bytes.  Returns Some if this is a valid value.
    ///
    pub def mkValue(t: ValueType, bs: Bytes): Result[Value,String] =
        if (valueTypeSize(t) == Int32.toInt64(Array.length(bs))) Ok(Value(t, bs))
        else Err("Not enough bytes.")

    def byteString(c: Int8): String = Char.toString(Char.fromInt32(Int8.toInt32(c)))

    def elemTypeFromBytes(t0: Int8, t1: Int8, t2: Int8, t3: Int8): Option[ElemType] =
        match byteString(t0) + byteString(t1) + byteString(t2) + byteString(t3) {
            case "  i8" => Some(I8)
            case " i16" => Some(I16)
            case " i32" => Some(I32)
            case " i64" => Some(I64)
            case "  u8" => Some(U8)
            case " u16" => Some(U16)
            case " u32" => Some(U32)
            case " u64" => Some(U64)
            case " f16" => Some(F16)
            case " f32" => Some(F32)
            case " f64" => Some(F64)
            case "bool" => Some(Bool)
            case _      => None
        }

    def elemTypeToBytes(et: ElemType): Array[Int8,r] =
        match et {
            case I8 => [32,32,105,56]
            case I16 => [32,105,49,54]
            case I32 => [32,105,51,50]
            case I64 => [32,105,54,52]
            case U8 => [32,32,117,56]
            case U16 => [32,117,49,54]
            case U32 => [32,117,51,50]
            case U64 => [32,117,54,52]
            case F16 => [32.102,49,54]
            case F32 => [32,102,51,50]
            case F64 => [32,102,54,52]
            case Bool => [98,111,111,108]
        }

    pub def shapeFromBytes(bytes: Array[Int8,r]): Shape & r =
        if (Array.length(bytes) == 0) Nil
        else (bytes[0..8] |> Array.foldLeft((acc, d) -> acc*256i64 + Int8.toInt64(d), 0i64))
             :: shapeFromBytes(bytes[8..])

    ///
    /// Read a value from the provides byte array.  Note that we allow unused bytes to follow the value.
    ///
    pub def valueFromBytes(bytes: Array[Int8,Impure]): Result[Value,String] \ Impure =
        match Array.toList(bytes[0..7]) {
            case 98i8 :: 2i8 :: num_dims :: t0 :: t1 :: t2 :: t3 :: Nil =>
                match elemTypeFromBytes(t0,t1,t2,t3) {
                    case Some(et) =>
                        let num_dims32 = Int8.toInt32(num_dims);
                        let shape = shapeFromBytes(bytes[7..7+num_dims32*8]);
                        let vt = (shape,et);
                        mkValue(vt,bytes[7+num_dims32*8..7+num_dims32*8])
                    case None => Err("Invalid type.")
                }
            case _ => {
                Err("Invalid header length")
            }
        }

    ///
    /// Convert Futhark value to byte array.
    ///
    pub def valueToBytes(v: Value) : Array[Int8,Impure] \ Impure =
        let shape = valueShape(v);
        let num_dims = Int32.clampToInt8({min=Int8.minValue()}, {max=Int8.maxValue()}, List.length(shape));
        Array.append([98i8,2i8,num_dims], elemTypeToBytes(valueElemType(v)))

    ///
    /// Read single Futhark value from file.
    ///
    pub def valuesFromFile(f: String): Result[Value,String] & Impure =
        Result.flatMap(valueFromBytes, File.readBytes(f))

}