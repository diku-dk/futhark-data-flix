namespace FutharkData {
    ///
    /// The shape of a value. Scalars have an empty shape.
    ///
    pub type alias Shape = List[Int64]
    
    /// The element type.
    pub enum ElemType with ToString, Eq, Order {
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
    opaque pub enum Value with ToString, Eq, Order { case Value(ValueType, Bytes) }

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

    ///
    /// The shape of a value.
    ///
    pub def valueShape(v: Value): Shape =
        fst(valueType(v))

    ///
    /// The raw byte representation of the elements of a value.
    ///
    pub def valueBytes(v: Value): Bytes =
        let Value.Value(_, bs) = v;
        bs

    ///
    /// Construct a value from a type and its representation as bytes.  Returns Some if this is a valid value.
    ///
    pub def mkValue(t: ValueType, bs: Bytes): Result[String,Value] =
        let expected = valueTypeSize(t);
        let got = Int32.toInt64(Vector.length(bs));
        if (expected == got) Ok(Value.Value(t, bs))
        else Err("mkValue: expected ${expected} bytes, but only got ${got}.")

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

    def shapeFromBytes(bytes: Bytes): Shape =
        if (Vector.length(bytes) == 0) Nil
        else (Vector.take(8,bytes) |> Vector.foldRight((d,acc) -> acc*256i64 + Int8.toInt64(d), 0i64))
             :: shapeFromBytes(Vector.drop(8,bytes))

    def shapeToBytes(shape: Shape): Bytes =
        region r {
            let arr = Array.new(r, List.length(shape)*8);
            let byte = (x,b) ->
                Int64.clampToInt8(min=-128i8,max=127i8,Int64.logicalRightShift(dist=b*8,x));
            let onDim = (i,x) ->
                {Array.put(byte(x,0), i*8+0, arr);
                 Array.put(byte(x,1), i*8+1, arr);
                 Array.put(byte(x,2), i*8+2, arr);
                 Array.put(byte(x,3), i*8+3, arr);
                 Array.put(byte(x,4), i*8+4, arr);
                 Array.put(byte(x,5), i*8+5, arr);
                 Array.put(byte(x,6), i*8+6, arr);
                 Array.put(byte(x,7), i*8+7, arr)};
            List.forEachWithIndex(onDim, shape);
            Array.toVector(arr)
        }

    ///
    /// Read a value from the provided byte array.
    ///
    pub def valueFromBytes(bytes: Vector[Int8]): Result[String,Value] =
        match Vector.toList(Vector.slice(start=0,end=7,bytes)) {
            case 98i8 :: 2i8 :: num_dims :: t0 :: t1 :: t2 :: t3 :: Nil =>
                match elemTypeFromBytes(t0,t1,t2,t3) {
                    case Some(et) =>
                        let num_dims32 = Int8.toInt32(num_dims);
                        let shape = shapeFromBytes(Vector.slice(start=7, end=7+num_dims32*8, bytes));
                        let vt = (shape,et);
                        mkValue(vt,Vector.drop(7+num_dims32*8, bytes))
                    case None => Err("valueFromBytes: Invalid type.")
                }
            case _ => {
                Err("valueFromBytes: Invalid header")
            }
        }

    ///
    /// Convert Futhark value to byte array.
    ///
    pub def valueToBytes(v: Value) : Vector[Int8] =
        let shape = valueShape(v);
        let num_dims = Int32.clampToInt8({min=Int8.minValue()}, {max=Int8.maxValue()}, List.length(shape));
        Vector.append(
            Vector.append(
                Vector.append(
                    Vector#{98i8,2i8,num_dims},
                    elemTypeToBytes(valueElemType(v))),
                shapeToBytes(shape)),
            valueBytes(v))

    ///
    /// Read single Futhark value from file.
    ///
    pub def valueFromFile(f: String): Result[String,Value] & Impure =
        region r { 
            Result.flatMap(Array.toVector >> valueFromBytes, File.readBytes(r, f))
        }
}