// The array [1,2,3] encoded as bytes.
def i32_123(): Vector[Int8] =
    Vector#{98i8,02i8,01i8,32i8,105i8,51i8,50i8,
            03i8,0i8,0i8,0i8,0i8,0i8,0i8,0i8,
            1i8,0i8,0i8,0i8,
            2i8,0i8,0i8,0i8,
            3i8,0i8,0i8,0i8}

@Test
def test_decode_i32(): Bool =
    match FutharkData.valueFromBytes(i32_123()) {
        case Ok(v) =>
            FutharkData.valueElemType(v) == FutharkData.ElemType.I32 and
            FutharkData.valueShape(v) == (3i64::Nil) and
            FutharkData.valueBytes(v) == Vector#{1i8,0i8,0i8,0i8,
                                                 2i8,0i8,0i8,0i8,
                                                 3i8,0i8,0i8,0i8}
        case Err(_) => false
    }

@Test
def test_decode_encode_i32(): Bool =
    match FutharkData.valueFromBytes(i32_123()) {
        case Ok(v) => debug(Vector.toList(FutharkData.valueToBytes(v))) == debug(Vector.toList(i32_123()))
        case Err(_) => false
    }