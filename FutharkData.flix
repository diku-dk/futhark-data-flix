namespace FutharkData {
    ///
    /// The shape of a value. Scalars have an empty shape.
    ///
    pub type alias Shape = List[Int64]
    
    /// The element type.
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
    /// An vector of bytes.
    ///
    pub type alias Bytes = Vector[Int8]

    ///
    /// A value of a pair of the type of the value and its representation as a byte array.
    ///
    opaque enum Value with ToString { case Value(ValueType, Bytes) }

    ///
    /// The type of a value.
    ///
    pub def valueType(v: Value): ValueType =
        let Value.Value(t,_) = v;
        t

    ///
    /// The size of a scalar value of this element type.
    ///
    pub def elemTypeSize(t: ElemType): Int64 =
        match t {
            case ElemType.Bool => 1
            case ElemType.F16 => 2
            case ElemType.F32 => 4
            case ElemType.F64 => 8
            case ElemType.I8 => 1
            case ElemType.I16 => 2
            case ElemType.I32 => 4
            case ElemType.I64 => 8
            case ElemType.U8 => 1
            case ElemType.U16 => 2
            case ElemType.U32 => 4
            case ElemType.U64 => 8
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
        let Value.Value(_, bs) = v;
        bs

    ///
    /// Construct a value from a type and its representation as bytes.  Returns Some if this is a valid value.
    ///
    pub def mkValue(t: ValueType, bs: Bytes): Result[String,Value] =
        if (valueTypeSize(t) == Int32.toInt64(Vector.length(bs))) Ok(Value.Value(t, bs))
        else Err("Not enough bytes.")

    def byteString(c: Int8): String = Char.toString(Char.fromInt32(Int8.toInt32(c)))

    def elemTypeFromBytes(t0: Int8, t1: Int8, t2: Int8, t3: Int8): Option[ElemType] =
        match byteString(t0) + byteString(t1) + byteString(t2) + byteString(t3) {
            case "  i8" => Some(ElemType.I8)
            case " i16" => Some(ElemType.I16)
            case " i32" => Some(ElemType.I32)
            case " i64" => Some(ElemType.I64)
            case "  u8" => Some(ElemType.U8)
            case " u16" => Some(ElemType.U16)
            case " u32" => Some(ElemType.U32)
            case " u64" => Some(ElemType.U64)
            case " f16" => Some(ElemType.F16)
            case " f32" => Some(ElemType.F32)
            case " f64" => Some(ElemType.F64)
            case "bool" => Some(ElemType.Bool)
            case _      => None
        }

    def elemTypeToBytes(et: ElemType): Vector[Int8] =
        match et {
            case ElemType.I8 => Vector#{32i8,32i8,105i8,56i8}
            case ElemType.I16 => Vector#{32i8,105i8,49i8,54i8}
            case ElemType.I32 => Vector#{32i8,105i8,51i8,50i8}
            case ElemType.I64 => Vector#{32i8,105i8,54i8,52i8}
            case ElemType.U8 => Vector#{32i8,32i8,117i8,56i8}
            case ElemType.U16 => Vector#{32i8,117i8,49i8,54i8}
            case ElemType.U32 => Vector#{32i8,117i8,51i8,50i8}
            case ElemType.U64 => Vector#{32i8,117i8,54i8,52i8}
            case ElemType.F16 => Vector#{32i8,102i8,49i8,54i8}
            case ElemType.F32 => Vector#{32i8,102i8,51i8,50i8}
            case ElemType.F64 => Vector#{32i8,102i8,54i8,52i8}
            case ElemType.Bool => Vector#{98i8,111i8,111i8,108i8}
        }

    pub def shapeFromBytes(bytes: Vector[Int8]): Shape & r =
        if (Vector.length(bytes) == 0) Nil
        else (Vector.take(8,bytes) |> Vector.foldLeft((acc, d) -> acc*256i64 + Int8.toInt64(d), 0i64))
             :: shapeFromBytes(Vector.drop(8,bytes))

    ///
    /// Read a value from the provides byte array.  Note that we allow unused bytes to follow the value.
    ///
    pub def valueFromBytes(bytes: Vector[Int8]): Result[String,Value] =
        match Vector.toList(Vector.slice(start=0,end=7,bytes)) {
            case 98i8 :: 2i8 :: num_dims :: t0 :: t1 :: t2 :: t3 :: Nil =>
                match elemTypeFromBytes(t0,t1,t2,t3) {
                    case Some(et) =>
                        let num_dims32 = Int8.toInt32(num_dims);
                        let shape = shapeFromBytes(Vector.slice(start=7, end=7+num_dims32*8, bytes));
                        let vt = (shape,et);
                        mkValue(vt,Vector.slice(start=7+num_dims32*8, end=7+num_dims32*8, bytes))
                    case None => Err("Invalid type.")
                }
            case _ => {
                Err("Invalid header length")
            }
        }

    ///
    /// Convert Futhark value to byte array.
    ///
    pub def valueToBytes(v: Value) : Vector[Int8] =
        let shape = valueShape(v);
        let num_dims = Int32.clampToInt8({min=Int8.minValue()}, {max=Int8.maxValue()}, List.length(shape));
        Vector.append(Vector#{98i8,2i8,num_dims}, elemTypeToBytes(valueElemType(v)))

    ///
    /// Read single Futhark value from file.
    ///
    pub def valuesFromFile(f: String): Result[String,Value] & Impure =
        region r { 
            Result.flatMap(Array.toVector >> valueFromBytes, File.readBytes(r, f))
        }
}