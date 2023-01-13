module TestingFITSCards

using FITSCards
using Test

@static if ENDIAN_BOM == 0x04030201
    const BYTE_ORDER = :little_endian
    order_bytes(x) = bswap(x)
elseif ENDIAN_BOM == 0x01020304
    const BYTE_ORDER = :big_endian
    order_bytes(x) = x
else
    error("unsupported byte order")
end

make_FITSKey(str::AbstractString) =
    FITSKey(reinterpret(UInt64,UInt8[c for c in str])[1])

function make_byte_vector(str::AbstractString)
    @assert codeunit(str) === UInt8
    vec = Array{UInt8}(undef, ncodeunits(str))
    I, = axes(vec)
    k = firstindex(str) - first(I)
    for i in I
        vec[i] = codeunit(str, i + k)
    end
    return vec
end

function make_discontinuous_byte_vector(str::AbstractString)
    @assert codeunit(str) === UInt8
    arr = Array{UInt8}(undef, 2, ncodeunits(str))
    I, J = axes(arr)
    i = last(I)
    k = firstindex(str) - first(J)
    for j in J
        arr[i,j] = codeunit(str, j + k)
    end
    return view(arr, i, :)
end

_load(::Type{T}, buf::Vector{UInt8}, off::Integer = 0) where {T} =
    GC.@preserve buf unsafe_load(Base.unsafe_convert(Ptr{T}, buf) + off)
_store!(::Type{T}, buf::Vector{UInt8}, x, off::Integer = 0) where {T} =
    GC.@preserve buf unsafe_store!(Base.unsafe_convert(Ptr{T}, buf) + off, convert(T, x)::T)

@testset "FITSCards.jl" begin
    @testset "Assertions" begin
        # Check that `unsafe_load` and `unsafe_store!` are unaligned operations
        # and that in `pointer + offset` expression the offset is in bytes (not
        # in number of elements).
        let buf = UInt8[b for b in 0x00:0xFF],
            ptr = Base.unsafe_convert(Ptr{UInt64}, buf)
            @test _load(UInt64, buf, 0) === order_bytes(0x0001020304050607)
            @test _load(UInt64, buf, 1) === order_bytes(0x0102030405060708)
            @test _load(UInt64, buf, 2) === order_bytes(0x0203040506070809)
            @test _load(UInt64, buf, 3) === order_bytes(0x030405060708090A)
            @test _load(UInt64, buf, 4) === order_bytes(0x0405060708090A0B)
            @test _load(UInt64, buf, 5) === order_bytes(0x05060708090A0B0C)
            @test _load(UInt64, buf, 6) === order_bytes(0x060708090A0B0C0D)
            @test _load(UInt64, buf, 7) === order_bytes(0x0708090A0B0C0D0E)
            val = order_bytes(0x0102030405060708)
            for i in 0:7
                _store!(UInt64, fill!(buf, 0x00), val, i)
                for j in 0:7
                    @test _load(UInt64, buf, j) === (val >> (8*(j - i)))
                end
            end
        end
        @test sizeof(FITSKey) == 8
        @test FITS_SHORT_KEYWORD_SIZE == 8
        @test FITS_CARD_SIZE == 80
        @test FITS_BLOCK_SIZE == 2880
    end
    @testset "Keywords" begin
        @test convert(Integer, FITSKey()) === zero(UInt64)
        @test UInt64(FITSKey()) === zero(UInt64)
        @test FITS"SIMPLE"   ==  make_FITSKey("SIMPLE  ")
        @test FITS"SIMPLE"   === make_FITSKey("SIMPLE  ")
        @test FITS"BITPIX"   === make_FITSKey("BITPIX  ")
        @test FITS"NAXIS"    === make_FITSKey("NAXIS   ")
        @test FITS"COMMENT"  === make_FITSKey("COMMENT ")
        @test FITS"HISTORY"  === make_FITSKey("HISTORY ")
        @test FITS"HIERARCH" === make_FITSKey("HIERARCH")
        @test FITS""         === make_FITSKey("        ")
        @test FITS"END"      === make_FITSKey("END     ")
        @test_throws Exception FITSCards.parse_keyword("SIMPLE#")
        @test_throws Exception FITSCards.parse_keyword(" SIMPLE")
        @test_throws Exception FITSCards.parse_keyword("SIMPLE ")
        @test_throws Exception FITSCards.parse_keyword("SImPLE")
        @test_throws Exception FITSCards.parse_keyword("TOO  MANY SPACES")
        # Simple (short) FITS keywords.
        @test FITSCards.parse_keyword("SIMPLE") == (FITS"SIMPLE", "SIMPLE")
        @test FITSCards.parse_keyword("HISTORY") == (FITS"HISTORY", "HISTORY")
        # Keywords longer than 8-characters are HIERARCH ones.
        @test FITSCards.parse_keyword("LONG-NAME") == (FITS"HIERARCH", "LONG-NAME")
        @test FITSCards.parse_keyword("HIERARCHY") == (FITS"HIERARCH", "HIERARCHY")
        @test FITSCards.parse_keyword("HIERARCHOLOGIST") == (FITS"HIERARCH", "HIERARCHOLOGIST")
        # Keywords starting by "HIERARCH " are HIERARCH ones.
        for key in ("HIERARCH GIZMO", "HIERARCH MY GIZMO", "HIERARCH MY BIG GIZMO")
            @test FITSCards.parse_keyword(key) == (FITS"HIERARCH", SubString(key, 10:length(key)))
        end
        # Keywords with spaces are HIERARCH ones whatever their lengths.
        for key in ("A B", "A B C", "SOME KEY", "TEST CASE")
            @test FITSCards.parse_keyword(key) == (FITS"HIERARCH", key)
        end
        # The following cases are consequences of the implemented scanner.
        @test FITSCards.parse_keyword("HIERARCH") == (FITS"HIERARCH", "HIERARCH")
        @test FITSCards.parse_keyword("HIERARCH SIMPLE") == (FITS"HIERARCH", "SIMPLE")
    end
    @testset "Parser" begin
        # Trimming of spaces.
        for str in ("", "  a string ", "another string", "  yet  another  string    ")
            @test SubString(str, FITSCards.Parser.trim_leading_spaces(str)) == lstrip(str)
            @test SubString(str, FITSCards.Parser.trim_trailing_spaces(str)) == rstrip(str)
            rng = firstindex(str):ncodeunits(str)
            @test SubString(str, FITSCards.Parser.trim_leading_spaces(str, rng)) == lstrip(str)
            @test SubString(str, FITSCards.Parser.trim_trailing_spaces(str, rng)) == rstrip(str)
        end
        # Representation of a character.
        @test FITSCards.Parser.repr_char(' ') == repr(' ')
        @test FITSCards.Parser.repr_char(0x20) == repr(' ')
        @test FITSCards.Parser.repr_char('\0') == repr(0x00)
        @test FITSCards.Parser.repr_char(0x00) == repr(0x00)
        @test FITSCards.Parser.repr_char('i') == repr('i')
        @test FITSCards.Parser.repr_char(0x69) == repr('i')
        # FITS logical value.
        @test FITSCards.Parser.try_parse_logical_value("T") === true
        @test FITSCards.Parser.try_parse_logical_value("F") === false
        @test FITSCards.Parser.try_parse_logical_value("t") === nothing
        @test FITSCards.Parser.try_parse_logical_value("f") === nothing
        @test FITSCards.Parser.try_parse_logical_value("true") === nothing
        @test FITSCards.Parser.try_parse_logical_value("false") === nothing
        # FITS integer value.
        for val in (zero(Int64), typemin(Int64), typemax(Int64))
            str = "$val"
            @test FITSCards.Parser.try_parse_integer_value(str) == val
            if val > 0
                # Add a few leading zeros.
                str = "000$val"
                @test FITSCards.Parser.try_parse_integer_value(str) == val
            end
            @test FITSCards.Parser.try_parse_integer_value(" "*str) === nothing
            @test FITSCards.Parser.try_parse_integer_value(str*" ") === nothing
        end
        # FITS float value;
        for val in (0.0, 1.0, -1.0, float(π))
            str = "$val"
            @test FITSCards.Parser.try_parse_float_value(str) ≈ val
            @test FITSCards.Parser.try_parse_integer_value(" "*str) === nothing
            @test FITSCards.Parser.try_parse_integer_value(str*" ") === nothing
        end
        # FITS complex value;
        @test FITSCards.Parser.try_parse_float_value("2.3d4") ≈ 2.3e4
        @test FITSCards.Parser.try_parse_float_value("-1.09D3") ≈ -1.09e3
        @test FITSCards.Parser.try_parse_complex_value("(2.3d4,-1.8)") ≈ complex(2.3e4,-1.8)
        @test FITSCards.Parser.try_parse_complex_value("(-1.09e5,7.6D2)") ≈ complex(-1.09e5,7.6e2)
        # FITS string value;
        @test FITSCards.Parser.try_parse_string_value("''") == ""
        @test FITSCards.Parser.try_parse_string_value("'''") === nothing
        @test FITSCards.Parser.try_parse_string_value("''''") == "'"
        @test FITSCards.Parser.try_parse_string_value("'Hello!'") == "Hello!"
        @test FITSCards.Parser.try_parse_string_value("'Hello! '") == "Hello!"
        @test FITSCards.Parser.try_parse_string_value("' Hello!'") == " Hello!"
        @test FITSCards.Parser.try_parse_string_value("' Hello! '") == " Hello!"
        @test FITSCards.Parser.try_parse_string_value("' Hello! '") == " Hello!"
        @test FITSCards.Parser.try_parse_string_value("'Joe''s taxi'") == "Joe's taxi"
        @test FITSCards.Parser.try_parse_string_value("'Joe's taxi'") === nothing
        @test FITSCards.Parser.try_parse_string_value("'Joe'''s taxi'") === nothing
    end
    @testset "Cards from strings" begin
        # Logical FITS cards.
        str = "SIMPLE  =                    T / this is a FITS file                            "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_LOGICAL, FITS"SIMPLE", "SIMPLE", "this is a FITS file")
        @test card.value isa Bool
        @test card.value == true
        @test card.value === card.logical
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === true
        @test isreal(card) === true
        # Integer valued cards.
        str = "BITPIX  =                  -32 / number of bits per data pixel                  "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_INTEGER, FITS"BITPIX", "BITPIX", "number of bits per data pixel")
        @test card.value isa Integer
        @test card.value == -32
        @test card.value === card.integer
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === true
        @test isreal(card) === true
       str = "NAXIS   =                    3 /      number of axes                            "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_INTEGER, FITS"NAXIS", "NAXIS", "number of axes")
        @test card.value isa Integer
        @test card.value == 3
        @test card.value === card.integer
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === true
        @test isreal(card) === true
        # COMMENT and HISTORY.
        str = "COMMENT   Some comments (with leading spaces that should not be removed)        "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_COMMENT, FITS"COMMENT", "COMMENT",
             "  Some comments (with leading spaces that should not be removed)")
        @test card.value isa Nothing
        @test card.value === nothing
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === false
        @test isinteger(card) === false
        @test isreal(card) === false
        str = "HISTORY A new history starts here...                                            "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_COMMENT, FITS"HISTORY", "HISTORY", "A new history starts here...")
        @test card.value isa Nothing
        @test card.value === nothing
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === false
        @test isinteger(card) === false
        @test isreal(card) === false
        @test FITSCards.is_comment(card) == true
        @test FITSCards.is_comment(card.type) == true
        @test FITSCards.is_comment(card.key) == true
        @test FITSCards.is_end(card) == false
        @test FITSCards.is_end(card.type) == false
        @test FITSCards.is_end(card.key) == false
        # String valued card.
        str = "DATE    = '2015-07-07T14:38:51' / file creation date (YYYY-MM-DDThh:mm:ss UT)   "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_STRING, FITS"DATE", "DATE", "file creation date (YYYY-MM-DDThh:mm:ss UT)")
        @test card.value isa AbstractString
        @test card.value == "2015-07-07T14:38:51"
        @test card.value === card.string
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === false
        @test FITSCards.is_comment(card) == false
        @test FITSCards.is_comment(card.type) == false
        @test FITSCards.is_comment(card.key) == false
        @test FITSCards.is_end(card) == false
        @test FITSCards.is_end(card.type) == false
        @test FITSCards.is_end(card.key) == false
        #
        str = "EXTNAME = 'SCIDATA '                                                            "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_STRING, FITS"EXTNAME", "EXTNAME", "")
        @test isinteger(card) === false
        @test isassigned(card) === true
        @test card.value isa AbstractString
        @test card.value == "SCIDATA"
        @test card.value === card.string
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === false
        #
        str = "CRPIX1  =                   1.                                                  "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_FLOAT, FITS"CRPIX1", "CRPIX1", "")
        @test card.value isa AbstractFloat
        @test card.value ≈ 1.0
        @test card.value === card.float
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === true
        #
        str = "CRVAL3  =                 0.96 / CRVAL along 3rd axis                           "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_FLOAT, FITS"CRVAL3", "CRVAL3", "CRVAL along 3rd axis")
        @test card.value isa AbstractFloat
        @test card.value ≈ 0.96
        @test card.value === card.float
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === true
        #
        str = "HIERARCH ESO OBS EXECTIME = +2919 / Expected execution time                     "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_INTEGER, FITS"HIERARCH", "ESO OBS EXECTIME", "Expected execution time")
        @test card.value isa Integer
        @test card.value == +2919
        @test card.value === card.integer
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === true
        @test isreal(card) === true
        # FITS cards with undefined value.
        str = "DUMMY   =                        / no value given                               "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_UNDEFINED, FITS"DUMMY", "DUMMY", "no value given")
        @test card.value isa Missing
        @test card.value === missing
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === false
        @test isinteger(card) === false
        @test isreal(card) === false
        str = "HIERARCH DUMMY   =               / no value given                               "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_UNDEFINED, FITS"HIERARCH", "DUMMY", "no value given")
        @test card.value isa Missing
        @test card.value === missing
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === false
        @test isinteger(card) === false
        @test isreal(card) === false
        # Complex valued cards.
        str = "COMPLEX = (1,0)                  / some complex value                           "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_COMPLEX, FITS"COMPLEX", "COMPLEX", "some complex value")
        @test card.value isa Complex
        @test card.value ≈ complex(1,0)
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === iszero(imag(card.value))
        str = "COMPLEX = (-2.7,+3.1d5)          / some other complex value                           "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_COMPLEX, FITS"COMPLEX", "COMPLEX", "some other complex value")
        @test card.value isa Complex
        @test card.value ≈ complex(-2.7,+3.1e5)
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === true
        @test isinteger(card) === false
        @test isreal(card) === iszero(imag(card.value))
        # END card.
        str = "END                                                                             "
        card = FITSCard(str)
        @test (card.type, card.key, card.name, card.comment) ==
            (FITS_END, FITS"END", "END", "")
        @test card.value isa Nothing
        @test card.value === nothing
        @test valtype(card) === typeof(card.value)
        @test isassigned(card) === false
        @test isinteger(card) === false
        @test isreal(card) === false
        @test FITSCards.is_comment(card) == false
        @test FITSCards.is_comment(card.type) == false
        @test FITSCards.is_comment(card.key) == false
        @test FITSCards.is_end(card) == true
        @test FITSCards.is_end(card.type) == true
        @test FITSCards.is_end(card.key) == true
    end
    @testset "Cards from bytes" begin
        # Logical FITS cards.
        str = "SIMPLE  =                    T / this is a FITS file                            "
        for buf in (make_byte_vector(str), make_discontinuous_byte_vector(str))
            card = FITSCard(buf)
            @test (card.type, card.key, card.name, card.comment) ==
                (FITS_LOGICAL, FITS"SIMPLE", "SIMPLE", "this is a FITS file")
            @test card.value isa Bool
            @test card.value == true
            @test card.value === card.logical
            @test valtype(card) === typeof(card.value)
        end
    end
    @testset "Cards from pairs" begin
        # Logical FITS cards.
        com = "some comment"
        card = FITSCard("SIMPLE" => (true, com))
        @test card.type === FITS_LOGICAL
        @test card.key === FITS"SIMPLE"
        @test card.name === "SIMPLE"
        @test card.value === true
        @test card.comment == com
        card = FITSCard("TWO KEYS" => (π, com))
        @test card.type === FITS_FLOAT
        @test card.key === FITS"HIERARCH"
        @test card.name == "TWO KEYS"
        @test card.value ≈ π
        @test card.comment == com
        card = convert(FITSCard, "HIERARCH NAME" => ("some name", com))
        @test card.type === FITS_STRING
        @test card.key === FITS"HIERARCH"
        @test card.name == "NAME"
        @test card.value == "some name"
        @test card.comment == com
        card = convert(FITSCard, "HIERARCH COMMENT" => (nothing, com))
        @test card.type === FITS_COMMENT
        @test card.key === FITS"HIERARCH"
        @test card.name == "COMMENT"
        @test card.value === nothing
        @test card.comment == com
        card = convert(FITSCard, "COMMENT" => com)
        @test card.type === FITS_COMMENT
        @test card.key === FITS"COMMENT"
        @test card.name == "COMMENT"
        @test card.value === nothing
        @test card.comment == com
        card = convert(FITSCard, "REASON" => undef)
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value === missing
        @test card.comment == ""
        card = convert(FITSCard, "REASON" => (missing, com))
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value === missing
        @test card.comment == com
    end
 end

end # module
