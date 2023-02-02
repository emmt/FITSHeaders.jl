module TestingFITSBase

using FITSBase
using FITSBase: FitsInteger, FitsFloat, FitsComplex

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

make_FitsKey(str::AbstractString) =
    FitsKey(reinterpret(UInt64,UInt8[c for c in str])[1])

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

@testset "FITSBase.jl" begin
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
        @test sizeof(FitsKey) == 8
        @test FITS_SHORT_KEYWORD_SIZE == 8
        @test FITS_CARD_SIZE == 80
        @test FITS_BLOCK_SIZE == 2880
    end
    @testset "FitsCardType" begin
        @test FitsCardType(Bool) === FITS_LOGICAL
        @test FitsCardType(Int16) === FITS_INTEGER
        @test FitsCardType(Float32) === FITS_FLOAT
        @test FitsCardType(ComplexF64) === FITS_COMPLEX
        @test FitsCardType(String) === FITS_STRING
        @test FitsCardType(Nothing) === FITS_COMMENT
        @test FitsCardType(UndefInitializer) === FITS_UNDEFINED
        @test FitsCardType(Missing) === FITS_UNDEFINED
    end
    @testset "Keywords" begin
        @test iszero(FitsKey())
        @test zero(FitsKey()) === FitsKey()
        @test zero(FitsKey) === FitsKey()
        @test convert(Integer, FitsKey()) === zero(UInt64)
        @test UInt64(FitsKey()) === zero(UInt64)
        @test FITS"SIMPLE"   ==  make_FitsKey("SIMPLE  ")
        @test FITS"SIMPLE"   === make_FitsKey("SIMPLE  ")
        @test FITS"BITPIX"   === make_FitsKey("BITPIX  ")
        @test FITS"NAXIS"    === make_FitsKey("NAXIS   ")
        @test FITS"COMMENT"  === make_FitsKey("COMMENT ")
        @test FITS"HISTORY"  === make_FitsKey("HISTORY ")
        @test FITS"HIERARCH" === make_FitsKey("HIERARCH")
        @test FITS""         === make_FitsKey("        ")
        @test FITS"END"      === make_FitsKey("END     ")
        @test String(FITS"") == ""
        @test String(FITS"SIMPLE") == "SIMPLE"
        @test String(FITS"HIERARCH") == "HIERARCH"
        @test repr(FITS"") == "FITS\"\""
        @test repr(FITS"SIMPLE") == "FITS\"SIMPLE\""
        @test repr(FITS"HIERARCH") == "FITS\"HIERARCH\""
        @test repr("text/plain", FITS"") == "FITS\"\""
        @test repr("text/plain", FITS"SIMPLE") == "FITS\"SIMPLE\""
        @test repr("text/plain", FITS"HIERARCH") == "FITS\"HIERARCH\""
        @test_throws Exception FITSBase.keyword("SIMPLE#")
        @test_throws Exception FITSBase.keyword(" SIMPLE")
        @test_throws Exception FITSBase.keyword("SIMPLE ")
        @test_throws Exception FITSBase.keyword("SImPLE")
        @test_throws Exception FITSBase.keyword("TOO  MANY SPACES")
        @test_throws Exception FITSBase.keyword("HIERARCH  A") # more than one space
        # Short FITS keywords.
        @test FITSBase.Parser.parse_keyword("SIMPLE") == (FITS"SIMPLE", false)
        @test FITSBase.keyword("SIMPLE") == "SIMPLE"
        @test FITSBase.Parser.parse_keyword("HISTORY") == (FITS"HISTORY", false)
        @test FITSBase.keyword("HISTORY") == "HISTORY"
        # Keywords longer than 8-characters are HIERARCH ones.
        @test FITSBase.Parser.parse_keyword("LONG-NAME") == (FITS"HIERARCH", true)
        @test FITSBase.keyword("LONG-NAME") == "HIERARCH LONG-NAME"
        @test FITSBase.Parser.parse_keyword("HIERARCHY") == (FITS"HIERARCH", true)
        @test FITSBase.keyword("HIERARCHY") == "HIERARCH HIERARCHY"
        # Keywords starting by "HIERARCH " are HIERARCH ones.
        for key in ("HIERARCH GIZMO", "HIERARCH MY GIZMO", "HIERARCH MY BIG GIZMO")
            @test FITSBase.Parser.parse_keyword(key) == (FITS"HIERARCH", false)
            @test FITSBase.keyword(key) === key # should return the same object
        end
        # Keywords with multiple words are HIERARCH ones whatever their lengths.
        for key in ("A B", "A B C", "SOME KEY", "TEST CASE")
            @test FITSBase.Parser.parse_keyword(key) == (FITS"HIERARCH", true)
            @test FITSBase.keyword(key) == "HIERARCH "*key
        end
        # The following cases are consequences of the implemented scanner.
        @test FITSBase.Parser.parse_keyword("HIERARCH") == (FITS"HIERARCH", false)
        @test FITSBase.keyword("HIERARCH") == "HIERARCH"
        @test FITSBase.Parser.parse_keyword("HIERARCH SIMPLE") == (FITS"HIERARCH", false)
        @test FITSBase.keyword("HIERARCH SIMPLE") == "HIERARCH SIMPLE"
    end
    @testset "Parser" begin
        # Byte order.
        @test FITSBase.Parser.is_big_endian() === (BYTE_ORDER === :big_endian)
        @test FITSBase.Parser.is_little_endian() === (BYTE_ORDER === :little_endian)
        # Character classes according to FITS standard.
        for b in 0x00:0xFF
            c = Char(b)
            @test FITSBase.Parser.is_digit(c) === ('0' ≤ c ≤ '9')
            @test FITSBase.Parser.is_uppercase(c) === ('A' ≤ c ≤ 'Z')
            @test FITSBase.Parser.is_lowercase(c) === ('a' ≤ c ≤ 'z')
            @test FITSBase.Parser.is_space(c) === (c == ' ')
            @test FITSBase.Parser.is_quote(c) === (c == '\'')
            @test FITSBase.Parser.is_equals_sign(c) === (c == '=')
            @test FITSBase.Parser.is_hyphen(c) === (c == '-')
            @test FITSBase.Parser.is_underscore(c) === (c == '_')
            @test FITSBase.Parser.is_comment_separator(c) === (c == '/')
            @test FITSBase.Parser.is_opening_parenthesis(c) === (c == '(')
            @test FITSBase.Parser.is_closing_parenthesis(c) === (c == ')')
            @test FITSBase.Parser.is_restricted_ascii(c) === (' ' ≤ c ≤ '~')
            @test FITSBase.Parser.is_keyword(c) ===
                (('0' ≤ c ≤ '9') | ('A' ≤ c ≤ 'Z') | (c == '-') | (c == '_'))
        end
        # Trimming of spaces.
        for str in ("", "  a string ", "another string", "  yet  another  string    ")
            @test SubString(str, FITSBase.Parser.trim_leading_spaces(str)) == lstrip(str)
            @test SubString(str, FITSBase.Parser.trim_trailing_spaces(str)) == rstrip(str)
            rng = firstindex(str):ncodeunits(str)
            @test SubString(str, FITSBase.Parser.trim_leading_spaces(str, rng)) == lstrip(str)
            @test SubString(str, FITSBase.Parser.trim_trailing_spaces(str, rng)) == rstrip(str)
        end
        # Representation of a character.
        @test FITSBase.Parser.repr_char(' ') == repr(' ')
        @test FITSBase.Parser.repr_char(0x20) == repr(' ')
        @test FITSBase.Parser.repr_char('\0') == repr(0x00)
        @test FITSBase.Parser.repr_char(0x00) == repr(0x00)
        @test FITSBase.Parser.repr_char('i') == repr('i')
        @test FITSBase.Parser.repr_char(0x69) == repr('i')
        # FITS logical value.
        @test FITSBase.Parser.try_parse_logical_value("T") === true
        @test FITSBase.Parser.try_parse_logical_value("F") === false
        @test FITSBase.Parser.try_parse_logical_value("t") === nothing
        @test FITSBase.Parser.try_parse_logical_value("f") === nothing
        @test FITSBase.Parser.try_parse_logical_value("true") === nothing
        @test FITSBase.Parser.try_parse_logical_value("false") === nothing
        # FITS integer value.
        for val in (zero(Int64), typemin(Int64), typemax(Int64))
            str = "$val"
            @test FITSBase.Parser.try_parse_integer_value(str) == val
            if val > 0
                # Add a few leading zeros.
                str = "000$val"
                @test FITSBase.Parser.try_parse_integer_value(str) == val
            end
            @test FITSBase.Parser.try_parse_integer_value(" "*str) === nothing
            @test FITSBase.Parser.try_parse_integer_value(str*" ") === nothing
        end
        # FITS float value;
        for val in (0.0, 1.0, -1.0, float(π))
            str = "$val"
            @test FITSBase.Parser.try_parse_float_value(str) ≈ val
            @test FITSBase.Parser.try_parse_integer_value(" "*str) === nothing
            @test FITSBase.Parser.try_parse_integer_value(str*" ") === nothing
        end
        # FITS complex value;
        @test FITSBase.Parser.try_parse_float_value("2.3d4") ≈ 2.3e4
        @test FITSBase.Parser.try_parse_float_value("-1.09D3") ≈ -1.09e3
        @test FITSBase.Parser.try_parse_complex_value("(2.3d4,-1.8)") ≈ complex(2.3e4,-1.8)
        @test FITSBase.Parser.try_parse_complex_value("(-1.09e5,7.6D2)") ≈ complex(-1.09e5,7.6e2)
        # FITS string value;
        @test FITSBase.Parser.try_parse_string_value("''") == ""
        @test FITSBase.Parser.try_parse_string_value("'''") === nothing
        @test FITSBase.Parser.try_parse_string_value("''''") == "'"
        @test FITSBase.Parser.try_parse_string_value("'Hello!'") == "Hello!"
        @test FITSBase.Parser.try_parse_string_value("'Hello! '") == "Hello!"
        @test FITSBase.Parser.try_parse_string_value("' Hello!'") == " Hello!"
        @test FITSBase.Parser.try_parse_string_value("' Hello! '") == " Hello!"
        @test FITSBase.Parser.try_parse_string_value("' Hello! '") == " Hello!"
        @test FITSBase.Parser.try_parse_string_value("'Joe''s taxi'") == "Joe's taxi"
        @test FITSBase.Parser.try_parse_string_value("'Joe's taxi'") === nothing
        @test FITSBase.Parser.try_parse_string_value("'Joe'''s taxi'") === nothing
        # Units.
        let com = ""
            @test FITSBase.Parser.get_units_part(com) == ""
            @test FITSBase.Parser.get_unitless_part(com) == ""
        end
        let com = "some comment"
            @test FITSBase.Parser.get_units_part(com) == ""
            @test FITSBase.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[]some comment"
            @test FITSBase.Parser.get_units_part(com) == ""
            @test FITSBase.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[] some comment"
            @test FITSBase.Parser.get_units_part(com) == ""
            @test FITSBase.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[some units]some comment"
            @test FITSBase.Parser.get_units_part(com) == "some units"
            @test FITSBase.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[  some units   ]  some comment"
            @test FITSBase.Parser.get_units_part(com) == "some units"
            @test FITSBase.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[some comment"
            @test FITSBase.Parser.get_units_part(com) == ""
            @test FITSBase.Parser.get_unitless_part(com) == "[some comment"
        end
    end
    @testset "Cards from strings" begin
        # Errors...
        @test_throws Exception FitsCard("END     nothing allowed here")
        @test_throws Exception FitsCard("VALUE   =     # / invalid character")
        @test_throws Exception FitsCard("VALUE   =  .-123 / invalid number")
        @test_throws Exception FitsCard("VALUE   =  -12x3 / invalid number")
        @test_throws Exception FitsCard("VALUE   = (1,3.0 / unclosed complex")
        @test_throws Exception FitsCard("VALUE   =   (1,) / bad complex")
        @test_throws Exception FitsCard("VALUE   =   (,1) / bad complex")
        @test_throws Exception FitsCard("VALUE   = 'hello / unclosed string")
        @test_throws Exception FitsCard("VALUE   = 'Joe's taxi' / unescaped quote")
        # Logical FITS cards.
        let card = FitsCard("SIMPLE  =                    T / this is a FITS file                     ")
            @test :type ∈ propertynames(card)
            @test :name ∈ propertynames(card)
            @test :key ∈ propertynames(card)
            @test :value ∈ propertynames(card)
            @test :comment ∈ propertynames(card)
            @test :units ∈ propertynames(card)
            @test :unitless ∈ propertynames(card)
            @test :logical ∈ propertynames(card)
            @test :integer ∈ propertynames(card)
            @test :float ∈ propertynames(card)
            @test :complex ∈ propertynames(card)
            @test :string ∈ propertynames(card)
            @test card.type === FITS_LOGICAL
            @test FitsCardType(card) === FITS_LOGICAL
            @test card.key == FITS"SIMPLE"
            @test card.name == "SIMPLE"
            @test card.comment == "this is a FITS file"
            @test card.value() isa Bool
            @test card.value() == true
            @test card.value() === card.logical
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            @test_throws Exception card.key = FITS"HISTORY"
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Bool)          === convert(Bool,        card.value())
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FitsInteger, card.value())
            @test card.value(Real)          === convert(FitsFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FitsFloat,   card.value())
            @test card.value(Complex)       === convert(FitsComplex, card.value())
            @test_throws Exception card.value(String)
            @test_throws Exception card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(typeof(card.value), card.value) === card.value
            @test convert(valtype(card), card.value) === card.value()
            @test convert(Bool,          card.value) === card.value(Bool)
            @test convert(Int16,         card.value) === card.value(Int16)
            @test convert(Integer,       card.value) === card.value(Integer)
            @test convert(Real,          card.value) === card.value(Real)
            @test convert(AbstractFloat, card.value) === card.value(AbstractFloat)
            @test convert(Complex,       card.value) === card.value(Complex)
            @test_throws Exception convert(String,         card.value)
            @test_throws Exception convert(AbstractString, card.value)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
        end
        # Integer valued cards.
        let card = FitsCard("BITPIX  =                  -32 / number of bits per data pixel           ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"BITPIX"
            @test card.name == "BITPIX"
            @test card.comment == "number of bits per data pixel"
            @test card.value() isa FitsInteger
            @test card.value() == -32
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FitsInteger, card.value())
            @test card.value(Real)          === convert(FitsFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FitsFloat,   card.value())
            @test card.value(Complex)       === convert(FitsComplex, card.value())
            @test_throws InexactError card.value(Bool)
            @test_throws Exception    card.value(String)
            @test_throws Exception    card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test convert(Int16,         card.value) === card.value(Int16)
            @test convert(Integer,       card.value) === card.value(Integer)
            @test convert(Real,          card.value) === card.value(Real)
            @test convert(AbstractFloat, card.value) === card.value(AbstractFloat)
            @test convert(Complex,       card.value) === card.value(Complex)
            @test_throws InexactError convert(Bool,           card.value)
            @test_throws Exception    convert(String,         card.value)
            @test_throws Exception    convert(AbstractString, card.value)
             # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
        end
        let card = FitsCard("NAXIS   =                    3 /      number of axes                      ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"NAXIS"
            @test card.name == "NAXIS"
            @test card.comment == "number of axes"
            @test card.units == ""
            @test card.unitless == "number of axes"
            @test card.value() isa FitsInteger
            @test card.value() == 3
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FitsInteger, card.value())
            @test card.value(Real)          === convert(FitsFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FitsFloat,   card.value())
            @test card.value(Complex)       === convert(FitsComplex, card.value())
            @test_throws InexactError card.value(Bool)
            @test_throws Exception    card.value(String)
            @test_throws Exception    card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test convert(Int16,         card.value) === card.value(Int16)
            @test convert(Integer,       card.value) === card.value(Integer)
            @test convert(Real,          card.value) === card.value(Real)
            @test convert(AbstractFloat, card.value) === card.value(AbstractFloat)
            @test convert(Complex,       card.value) === card.value(Complex)
            @test_throws InexactError convert(Bool,           card.value)
            @test_throws Exception    convert(String,         card.value)
            @test_throws Exception    convert(AbstractString, card.value)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
        end
        # COMMENT and HISTORY.
        let card = FitsCard("COMMENT   Some comments (with leading spaces that should not be removed) ")
            @test card.type == FITS_COMMENT
            @test card.key == FITS"COMMENT"
            @test card.name == "COMMENT"
            @test card.comment == "  Some comments (with leading spaces that should not be removed)"
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test_throws Exception card.value(Bool)
            @test_throws Exception card.value(Int16)
            @test_throws Exception card.value(Integer)
            @test_throws Exception card.value(Real)
            @test_throws Exception card.value(AbstractFloat)
            @test_throws Exception card.value(Complex)
            @test_throws Exception card.value(String)
            @test_throws Exception card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test_throws Exception convert(Bool,           card.value)
            @test_throws Exception convert(Int16,          card.value)
            @test_throws Exception convert(Integer,        card.value)
            @test_throws Exception convert(Real,           card.value)
            @test_throws Exception convert(AbstractFloat,  card.value)
            @test_throws Exception convert(Complex,        card.value)
            @test_throws Exception convert(String,         card.value)
            @test_throws Exception convert(AbstractString, card.value)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            # is_comment(), is_end(), ...
            @test FITSBase.is_comment(card) === true
            @test FITSBase.is_comment(card.type) === true
            @test FITSBase.is_comment(card.key) === true # standard comment
            @test FITSBase.is_end(card) === false
            @test FITSBase.is_end(card.type) === false
            @test FITSBase.is_end(card.key) === false
        end
        let card = FitsCard("HISTORY A new history starts here...                                     ")
            @test card.type == FITS_COMMENT
            @test card.key == FITS"HISTORY"
            @test card.name == "HISTORY"
            @test card.comment == "A new history starts here..."
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test_throws Exception card.value(Bool)
            @test_throws Exception card.value(Int16)
            @test_throws Exception card.value(Integer)
            @test_throws Exception card.value(Real)
            @test_throws Exception card.value(AbstractFloat)
            @test_throws Exception card.value(Complex)
            @test_throws Exception card.value(String)
            @test_throws Exception card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test_throws Exception convert(Bool,           card.value)
            @test_throws Exception convert(Int16,          card.value)
            @test_throws Exception convert(Integer,        card.value)
            @test_throws Exception convert(Real,           card.value)
            @test_throws Exception convert(AbstractFloat,  card.value)
            @test_throws Exception convert(Complex,        card.value)
            @test_throws Exception convert(String,         card.value)
            @test_throws Exception convert(AbstractString, card.value)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            # is_comment(), is_end(), ...
            @test FITSBase.is_comment(card) === true
            @test FITSBase.is_comment(card.type) === true
            @test FITSBase.is_comment(card.key) === true # standard comment
            @test FITSBase.is_end(card) === false
            @test FITSBase.is_end(card.type) === false
            @test FITSBase.is_end(card.key) === false
        end
        # Non standard commentary card.
        let card = FitsCard("NON-STANDARD COMMENT" => (nothing, "some comment"))
            @test card.type == FITS_COMMENT
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH NON-STANDARD COMMENT"
            @test card.comment == "some comment"
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test_throws Exception card.value(Bool)
            @test_throws Exception card.value(Int16)
            @test_throws Exception card.value(Integer)
            @test_throws Exception card.value(Real)
            @test_throws Exception card.value(AbstractFloat)
            @test_throws Exception card.value(Complex)
            @test_throws Exception card.value(String)
            @test_throws Exception card.value(AbstractString)
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test_throws Exception convert(Bool,           card.value)
            @test_throws Exception convert(Int16,          card.value)
            @test_throws Exception convert(Integer,        card.value)
            @test_throws Exception convert(Real,           card.value)
            @test_throws Exception convert(AbstractFloat,  card.value)
            @test_throws Exception convert(Complex,        card.value)
            @test_throws Exception convert(String,         card.value)
            @test_throws Exception convert(AbstractString, card.value)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            # is_comment(), is_end(), ...
            @test FITSBase.is_comment(card) === true
            @test FITSBase.is_comment(card.type) === true
            @test FITSBase.is_comment(card.key) === false # non-standard comment
            @test FITSBase.is_end(card) === false
            @test FITSBase.is_end(card.type) === false
            @test FITSBase.is_end(card.key) === false
        end
        # String valued card.
        let card = FitsCard("REMARK  = 'Joe''s taxi'        / a string with an embedded quote         ")
            @test card.type == FITS_STRING
            @test card.key == FITS"REMARK"
            @test card.name == "REMARK"
            @test card.comment == "a string with an embedded quote"
            @test card.value() isa String
            @test card.value() == "Joe's taxi"
            @test card.value() === card.string
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === false

            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test_throws Exception card.value(Bool)
            @test_throws Exception card.value(Int16)
            @test_throws Exception card.value(Integer)
            @test_throws Exception card.value(Real)
            @test_throws Exception card.value(AbstractFloat)
            @test_throws Exception card.value(Complex)
            @test card.value(String)         === convert(String,         card.value())
            @test card.value(AbstractString) === convert(AbstractString, card.value())
            # Convert callable value object by calling `convert`.
            @test convert(valtype(card), card.value) === card.value()
            @test_throws Exception convert(Bool,           card.value)
            @test_throws Exception convert(Int16,          card.value)
            @test_throws Exception convert(Integer,        card.value)
            @test_throws Exception convert(Real,           card.value)
            @test_throws Exception convert(AbstractFloat,  card.value)
            @test_throws Exception convert(Complex,        card.value)
            @test convert(String,         card.value) === card.value(String)
            @test convert(AbstractString, card.value) === card.value(AbstractString)
            # Various string representations.
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
             # is_comment(), is_end(), ...
            @test FITSBase.is_comment(card) == false
            @test FITSBase.is_comment(card.type) == false
            @test FITSBase.is_comment(card.key) == false
            @test FITSBase.is_end(card) == false
            @test FITSBase.is_end(card.type) == false
            @test FITSBase.is_end(card.key) == false
        end
        #
        let card = FitsCard("EXTNAME = 'SCIDATA ' ")
            @test card.type == FITS_STRING
            @test card.key == FITS"EXTNAME"
            @test card.name == "EXTNAME"
            @test card.comment == ""
            @test isinteger(card) === false
            @test isassigned(card) === true
            @test card.value() isa String
            @test card.value() == "SCIDATA"
            @test card.value() === card.string
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        #
        let card = FitsCard("CRPIX1  =                   1. ")
            @test card.type == FITS_FLOAT
            @test card.key == FITS"CRPIX1"
            @test card.name == "CRPIX1"
            @test card.comment == ""
            @test card.value() isa FitsFloat
            @test card.value() ≈ 1.0
            @test card.value() === card.float
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === true
        end
        #
        let card = FitsCard("CRVAL3  =                 0.96 / CRVAL along 3rd axis ")
            @test card.type == FITS_FLOAT
            @test card.key == FITS"CRVAL3"
            @test card.name == "CRVAL3"
            @test card.comment == "CRVAL along 3rd axis"
            @test card.value() isa FitsFloat
            @test card.value() ≈ 0.96
            @test card.value() === card.float
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === true
        end
        #
        let card = FitsCard("HIERARCH ESO OBS EXECTIME = +2919 / Expected execution time ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH ESO OBS EXECTIME"
            @test card.comment == "Expected execution time"
            @test card.value() isa FitsInteger
            @test card.value() == +2919
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
        end
        # FITS cards with undefined value.
        let card = FitsCard("DUMMY   =                        / no value given ")
            @test card.type == FITS_UNDEFINED
            @test card.key == FITS"DUMMY"
            @test card.name == "DUMMY"
            @test card.comment == "no value given"
            @test card.value() isa Missing
            @test card.value() === missing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        let card = FitsCard("HIERARCH DUMMY   =               / no value given ")
            @test card.type == FITS_UNDEFINED
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH DUMMY"
            @test card.comment == "no value given"
            @test card.value() isa Missing
            @test card.value() === missing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        # Complex valued cards.
        let card = FitsCard("COMPLEX = (1,0)                  / [km/s] some complex value ")
            @test card.type == FITS_COMPLEX
            @test card.key == FITS"COMPLEX"
            @test card.name == "COMPLEX"
            @test card.comment == "[km/s] some complex value"
            @test card.units == "km/s"
            @test card.unitless == "some complex value"
            @test card.value() isa FitsComplex
            @test card.value() ≈ complex(1,0)
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === iszero(imag(card.value()))
        end
        let card = FitsCard("COMPLEX = (-2.7,+3.1d5)          / some other complex value ")
            @test card.type == FITS_COMPLEX
            @test card.key == FITS"COMPLEX"
            @test card.name == "COMPLEX"
            @test card.comment == "some other complex value"
            @test card.value() isa FitsComplex
            @test card.value() ≈ complex(-2.7,+3.1e5)
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test card.value(Complex{Float32}) === Complex{Float32}(card.value())
            @test convert(Complex{Float32}, card.value) === Complex{Float32}(card.value())
            @test_throws InexactError card.value(Float64)
            @test_throws InexactError convert(Float32, card.value)
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === iszero(imag(card.value()))
        end
        # END card.
        let card = FitsCard("END                           ")
            @test card.type == FITS_END
            @test card.key == FITS"END"
            @test card.name == "END"
            @test card.comment == ""
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
            @test card.value(valtype(card)) === card.value()
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
            @test FITSBase.is_comment(card) == false
            @test FITSBase.is_comment(card.type) == false
            @test FITSBase.is_comment(card.key) == false
            @test FITSBase.is_end(card) == true
            @test FITSBase.is_end(card.type) == true
            @test FITSBase.is_end(card.key) == true
        end
    end
    @testset "Cards from bytes" begin
        # Logical FITS cards.
        str = "SIMPLE  =                    T / this is a FITS file                            "
        for buf in (make_byte_vector(str), make_discontinuous_byte_vector(str))
            card = FitsCard(buf)
                        @test card.type == FITS_LOGICAL
            @test card.key == FITS"SIMPLE"
            @test card.name == "SIMPLE"
            @test card.comment == "this is a FITS file"
            @test card.value() isa Bool
            @test card.value() == true
            @test card.value() === card.logical
            @test valtype(card) === typeof(card.value())
        end
        # "END", empty string "" or out of range offset yield an END card.
        let card = FitsCard("END")
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FitsCard("")
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FitsCard("xEND"; offset=1)
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FitsCard("SOMETHING"; offset=250)
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
    end
    @testset "Cards from pairs" begin
        # Logical FITS cards.
        com = "some comment"
        pair = "SIMPLE" => (true, com)
        card = FitsCard(pair)
        @test FitsCard(card) === card
        @test convert(FitsCard, card) === card
        @test convert(FitsCard, pair) == card
        @test convert(Pair, card) == pair
        @test Pair(card) == pair
        #@test Pair{String}(card) == pair
        @test Pair{String,Tuple{Bool,String}}(card) === pair
        @test Pair{String,Tuple{Int,String}}(card) === (card.name => (card.value(Int), card.comment))
        @test card.type === FITS_LOGICAL
        @test card.key === FITS"SIMPLE"
        @test card.name === "SIMPLE"
        @test card.value() === true
        @test card.comment == com
        card = FitsCard("TWO KEYS" => (π, com))
        @test card.type === FITS_FLOAT
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH TWO KEYS"
        @test card.value() ≈ π
        @test card.comment == com
        card = convert(FitsCard, "HIERARCH NAME" => ("some name", com))
        @test card.type === FITS_STRING
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH NAME"
        @test card.value() == "some name"
        @test card.comment == com
        card = convert(FitsCard, "HIERARCH COMMENT" => (nothing, com))
        @test card.type === FITS_COMMENT
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH COMMENT"
        @test card.value() === nothing
        @test card.comment == com
        card = convert(FitsCard, "COMMENT" => com)
        @test card.type === FITS_COMMENT
        @test card.key === FITS"COMMENT"
        @test card.name == "COMMENT"
        @test card.value() === nothing
        @test card.comment == com
        card = convert(FitsCard, "REASON" => undef)
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value() === missing
        @test card.comment == ""
        card = convert(FitsCard, "REASON" => (missing, com))
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value() === missing
        @test card.comment == com
    end
    @testset "Headers" begin
        dims = (4,5,6)
        h = FitsHeader("SIMPLE" => (true, "FITS file"),
                       "BITPIX" => (-32, "bits per pixels"),
                       "NAXIS" => (length(dims), "number of dimensions"))
        @test length(h) == 3
        @test sort(collect(keys(h))) == sort(["SIMPLE", "BITPIX", "NAXIS"])
        # Same object:
        @test convert(FitsHeader, h) === h
        # Same contents but different objects:
        hp = convert(FitsHeader, h.cards); @test hp !== h && hp == h
        hp = FitsHeader(h); @test  hp !== h && hp == h
        hp = copy(h); @test  hp !== h && hp == h
        @test IndexStyle(h) === IndexLinear()
        @test h["SIMPLE"] === h[1]
        @test h[1].key === FITS"SIMPLE"
        @test h[1].value() == true
        @test h["BITPIX"] === h[2]
        @test h[2].key === FITS"BITPIX"
        @test h[2].value() == -32
        @test h["NAXIS"] === h[3]
        @test h[3].key === FITS"NAXIS"
        @test h[3].value() == length(dims)
        for i in eachindex(dims)
            push!(h, "NAXIS$i" => (dims[i], "length of dimension # $i"))
        end
        push!(h, "COMMENT" => "Some comment.")
        h["CCD GAIN"] = (3.2, "[ADU/e-] detector gain")
        h["HIERARCH CCD BIAS"] = -15
        h["BSCALE"] = 1.0
        h["BZERO"] = 0.0
        h["COMMENT"] = "Another comment."
        h["COMMENT"] = "Yet another comment."
        # Test indexing by integer/name.
        i = findfirst("BITPIX", h)
        @test i isa Integer && h[i].name == "BITPIX"
        @test h["BITPIX"] === h[i]
        @test h["BSCALE"].value(Real) ≈ 1
        @test h["BSCALE"].comment == ""
        # Test HIERARCH records.
        @test get(h, 0, nothing) === nothing
        @test get(h, 1, nothing) === h[1]
        card = get(h, "HIERARCH CCD GAIN", nothing)
        @test card isa FitsCard
        if card !== Nothing
            @test card.key === FITS"HIERARCH"
            @test card.name == "HIERARCH CCD GAIN"
            @test card.value() ≈ 3.2
            @test card.units == "ADU/e-"
            @test card.unitless == "detector gain"
        end
        card = get(h, "CCD BIAS", nothing)
        @test card isa FitsCard
        if card !== Nothing
            @test card.key === FITS"HIERARCH"
            @test card.name == "HIERARCH CCD BIAS"
            @test card.value() == -15
        end
        # Change existing record, by name and by index.
        n = length(h)
        h["BSCALE"] = (1.1, "better value")
        @test length(h) == n
        @test h["BSCALE"].value(Real) ≈ 1.1
        @test h["BSCALE"].comment == "better value"
        i = findfirst("BITPIX", h)
        @test i isa Integer
        h[i] = (h[i].name => (-64, h[i].comment))
        @test h["BITPIX"].value() == -64
        # Append non-existing record.
        n = length(h)
        h["GIZMO"] = ("Joe's taxi", "what?")
        @test length(h) == n+1
        @test h["GIZMO"].value() == "Joe's taxi"
        @test h["GIZMO"].comment == "what?"
        # Test search failure when: (i) keyword is valid but does not exists,
        # (ii) keyword is invalid, and (iii) pattern is unsupported.
        @test findfirst("NON-EXISTING KEYWORD", h) === nothing
        @test findfirst("Invalid keyword", h) === nothing
        @test findfirst(π, h) === nothing
        # Forward search.
        i = findfirst("COMMENT", h)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Some comment."
        i = findnext(h[i], h, i + 1)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Another comment."
        i = findnext(h[i], h, i + 1)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Yet another comment."
        @test findnext(h[i], h, i + 1) isa Nothing
        # Backward search.
        i = findlast("COMMENT", h)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Yet another comment."
        i = findprev(h[i], h, i - 1)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Another comment."
        i = findprev(h[i], h, i - 1)
        @test i isa Integer
        @test h[i].type === FITS_COMMENT
        @test h[i].comment == "Some comment."
        @test findprev(h[i], h, i - 1) isa Nothing
        # Check that non-commentary records are unique.
        for i in eachindex(h)
            card = h[i]
            card.type === FITS_COMMENT && continue
            @test findnext(card, h, i + 1) isa Nothing
        end
        # Search with a predicate.
        @test findfirst(card -> card.type === FITS_END, h) === nothing
        @test findlast(card -> card.name == "BITPIX", h) === 2
    end
end

end # module
