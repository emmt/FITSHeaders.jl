module TestingFITSCards

using FITSCards
using FITSCards: FITSInteger, FITSFloat, FITSComplex

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
        @test iszero(FITSKey())
        @test zero(FITSKey()) === FITSKey()
        @test zero(FITSKey) === FITSKey()
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
        @test String(FITS"") == ""
        @test String(FITS"SIMPLE") == "SIMPLE"
        @test String(FITS"HIERARCH") == "HIERARCH"
        @test repr(FITS"") == "FITS\"\""
        @test repr(FITS"SIMPLE") == "FITS\"SIMPLE\""
        @test repr(FITS"HIERARCH") == "FITS\"HIERARCH\""
        @test repr("text/plain", FITS"") == "FITS\"\""
        @test repr("text/plain", FITS"SIMPLE") == "FITS\"SIMPLE\""
        @test repr("text/plain", FITS"HIERARCH") == "FITS\"HIERARCH\""
        @test_throws Exception FITSCards.keyword("SIMPLE#")
        @test_throws Exception FITSCards.keyword(" SIMPLE")
        @test_throws Exception FITSCards.keyword("SIMPLE ")
        @test_throws Exception FITSCards.keyword("SImPLE")
        @test_throws Exception FITSCards.keyword("TOO  MANY SPACES")
        @test_throws Exception FITSCards.keyword("HIERARCH  A") # more than one space
        # Short FITS keywords.
        @test FITSCards.Parser.parse_keyword("SIMPLE") == (FITS"SIMPLE", false)
        @test FITSCards.keyword("SIMPLE") == "SIMPLE"
        @test FITSCards.Parser.parse_keyword("HISTORY") == (FITS"HISTORY", false)
        @test FITSCards.keyword("HISTORY") == "HISTORY"
        # Keywords longer than 8-characters are HIERARCH ones.
        @test FITSCards.Parser.parse_keyword("LONG-NAME") == (FITS"HIERARCH", true)
        @test FITSCards.keyword("LONG-NAME") == "HIERARCH LONG-NAME"
        @test FITSCards.Parser.parse_keyword("HIERARCHY") == (FITS"HIERARCH", true)
        @test FITSCards.keyword("HIERARCHY") == "HIERARCH HIERARCHY"
        # Keywords starting by "HIERARCH " are HIERARCH ones.
        for key in ("HIERARCH GIZMO", "HIERARCH MY GIZMO", "HIERARCH MY BIG GIZMO")
            @test FITSCards.Parser.parse_keyword(key) == (FITS"HIERARCH", false)
            @test FITSCards.keyword(key) === key # should return the same object
        end
        # Keywords with multiple words are HIERARCH ones whatever their lengths.
        for key in ("A B", "A B C", "SOME KEY", "TEST CASE")
            @test FITSCards.Parser.parse_keyword(key) == (FITS"HIERARCH", true)
            @test FITSCards.keyword(key) == "HIERARCH "*key
        end
        # The following cases are consequences of the implemented scanner.
        @test FITSCards.Parser.parse_keyword("HIERARCH") == (FITS"HIERARCH", false)
        @test FITSCards.keyword("HIERARCH") == "HIERARCH"
        @test FITSCards.Parser.parse_keyword("HIERARCH SIMPLE") == (FITS"HIERARCH", false)
        @test FITSCards.keyword("HIERARCH SIMPLE") == "HIERARCH SIMPLE"
    end
    @testset "Parser" begin
        # Character classes according to FITS standard.
        for b in 0x00:0xFF
            c = Char(b)
            @test FITSCards.Parser.is_digit(c) === ('0' ≤ c ≤ '9')
            @test FITSCards.Parser.is_uppercase(c) === ('A' ≤ c ≤ 'Z')
            @test FITSCards.Parser.is_lowercase(c) === ('a' ≤ c ≤ 'z')
            @test FITSCards.Parser.is_space(c) === (c == ' ')
            @test FITSCards.Parser.is_quote(c) === (c == '\'')
            @test FITSCards.Parser.is_equals_sign(c) === (c == '=')
            @test FITSCards.Parser.is_hyphen(c) === (c == '-')
            @test FITSCards.Parser.is_underscore(c) === (c == '_')
            @test FITSCards.Parser.is_comment_separator(c) === (c == '/')
            @test FITSCards.Parser.is_opening_parenthesis(c) === (c == '(')
            @test FITSCards.Parser.is_closing_parenthesis(c) === (c == ')')
            @test FITSCards.Parser.is_restricted_ascii(c) === (' ' ≤ c ≤ '~')
            @test FITSCards.Parser.is_keyword(c) ===
                (('0' ≤ c ≤ '9') | ('A' ≤ c ≤ 'Z') | (c == '-') | (c == '_'))
        end
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
        # Units.
        let com = ""
            @test FITSCards.Parser.get_units_part(com) == ""
            @test FITSCards.Parser.get_unitless_part(com) == ""
        end
        let com = "some comment"
            @test FITSCards.Parser.get_units_part(com) == ""
            @test FITSCards.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[]some comment"
            @test FITSCards.Parser.get_units_part(com) == ""
            @test FITSCards.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[] some comment"
            @test FITSCards.Parser.get_units_part(com) == ""
            @test FITSCards.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[some units]some comment"
            @test FITSCards.Parser.get_units_part(com) == "some units"
            @test FITSCards.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[  some units   ]  some comment"
            @test FITSCards.Parser.get_units_part(com) == "some units"
            @test FITSCards.Parser.get_unitless_part(com) == "some comment"
        end
        let com = "[some comment"
            @test FITSCards.Parser.get_units_part(com) == ""
            @test FITSCards.Parser.get_unitless_part(com) == "[some comment"
        end
    end
    @testset "Cards from strings" begin
        # Errors...
        @test_throws Exception FITSCard("END     nothing allowed here")
        @test_throws Exception FITSCard("VALUE   =     # / invalid character")
        @test_throws Exception FITSCard("VALUE   =  .-123 / invalid number")
        @test_throws Exception FITSCard("VALUE   =  -12x3 / invalid number")
        @test_throws Exception FITSCard("VALUE   = (1,3.0 / unclosed complex")
        @test_throws Exception FITSCard("VALUE   =   (1,) / bad complex")
        @test_throws Exception FITSCard("VALUE   =   (,1) / bad complex")
        @test_throws Exception FITSCard("VALUE   = 'hello / unclosed string")
        @test_throws Exception FITSCard("VALUE   = 'Joe's taxi' / unescaped quote")
        # Logical FITS cards.
        let card = FITSCard("SIMPLE  =                    T / this is a FITS file                     ")
            @test card.type == FITS_LOGICAL
            @test card.key == FITS"SIMPLE"
            @test card.name == "SIMPLE"
            @test card.comment == "this is a FITS file"
            @test card.value() isa Bool
            @test card.value() == true
            @test card.value() === card.logical
            @test valtype(card) === typeof(card.value())
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Bool)          === convert(Bool,        card.value())
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FITSInteger, card.value())
            @test card.value(Real)          === convert(FITSFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FITSFloat,   card.value())
            @test card.value(Complex)       === convert(FITSComplex, card.value())
            @test_throws Exception card.value(String)
            @test_throws Exception card.value(AbstractString)
            # Convert callable value object by calling `convert`.
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
        let card = FITSCard("BITPIX  =                  -32 / number of bits per data pixel           ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"BITPIX"
            @test card.name == "BITPIX"
            @test card.comment == "number of bits per data pixel"
            @test card.value() isa FITSInteger
            @test card.value() == -32
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FITSInteger, card.value())
            @test card.value(Real)          === convert(FITSFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FITSFloat,   card.value())
            @test card.value(Complex)       === convert(FITSComplex, card.value())
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
        let card = FITSCard("NAXIS   =                    3 /      number of axes                      ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"NAXIS"
            @test card.name == "NAXIS"
            @test card.comment == "number of axes"
            @test card.units == ""
            @test card.unitless == "number of axes"
            @test card.value() isa FITSInteger
            @test card.value() == 3
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
            # Convert callable value object by calling the object itself.
            @test card.value(valtype(card)) === card.value()
            @test card.value(Int16)         === convert(Int16,       card.value())
            @test card.value(Integer)       === convert(FITSInteger, card.value())
            @test card.value(Real)          === convert(FITSFloat,   card.value())
            @test card.value(AbstractFloat) === convert(FITSFloat,   card.value())
            @test card.value(Complex)       === convert(FITSComplex, card.value())
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
        let card = FITSCard("COMMENT   Some comments (with leading spaces that should not be removed) ")
            @test card.type == FITS_COMMENT
            @test card.key == FITS"COMMENT"
            @test card.name == "COMMENT"
            @test card.comment == "  Some comments (with leading spaces that should not be removed)"
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
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
            @test FITSCards.is_comment(card) === true
            @test FITSCards.is_comment(card.type) === true
            @test FITSCards.is_comment(card.key) === true # standard comment
            @test FITSCards.is_end(card) === false
            @test FITSCards.is_end(card.type) === false
            @test FITSCards.is_end(card.key) === false
        end
        let card = FITSCard("HISTORY A new history starts here...                                     ")
            @test card.type == FITS_COMMENT
            @test card.key == FITS"HISTORY"
            @test card.name == "HISTORY"
            @test card.comment == "A new history starts here..."
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
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
            @test FITSCards.is_comment(card) === true
            @test FITSCards.is_comment(card.type) === true
            @test FITSCards.is_comment(card.key) === true # standard comment
            @test FITSCards.is_end(card) === false
            @test FITSCards.is_end(card.type) === false
            @test FITSCards.is_end(card.key) === false
        end
        # Non standard commentary card.
        let card = FITSCard("NON-STANDARD COMMENT" => (nothing, "some comment"))
            @test card.type == FITS_COMMENT
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH NON-STANDARD COMMENT"
            @test card.comment == "some comment"
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
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
            @test FITSCards.is_comment(card) === true
            @test FITSCards.is_comment(card.type) === true
            @test FITSCards.is_comment(card.key) === false # non-standard comment
            @test FITSCards.is_end(card) === false
            @test FITSCards.is_end(card.type) === false
            @test FITSCards.is_end(card.key) === false
        end
        # String valued card.
        let card = FITSCard("REMARK  = 'Joe''s taxi'        / a string with an embedded quote         ")
            @test card.type == FITS_STRING
            @test card.key == FITS"REMARK"
            @test card.name == "REMARK"
            @test card.comment == "a string with an embedded quote"
            @test card.value() isa String
            @test card.value() == "Joe's taxi"
            @test card.value() === card.string
            @test valtype(card) === typeof(card.value())
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
            @test FITSCards.is_comment(card) == false
            @test FITSCards.is_comment(card.type) == false
            @test FITSCards.is_comment(card.key) == false
            @test FITSCards.is_end(card) == false
            @test FITSCards.is_end(card.type) == false
            @test FITSCards.is_end(card.key) == false
        end
        #
        let card = FITSCard("EXTNAME = 'SCIDATA ' ")
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
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        #
        let card = FITSCard("CRPIX1  =                   1. ")
            @test card.type == FITS_FLOAT
            @test card.key == FITS"CRPIX1"
            @test card.name == "CRPIX1"
            @test card.comment == ""
            @test card.value() isa FITSFloat
            @test card.value() ≈ 1.0
            @test card.value() === card.float
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === true
        end
        #
        let card = FITSCard("CRVAL3  =                 0.96 / CRVAL along 3rd axis ")
            @test card.type == FITS_FLOAT
            @test card.key == FITS"CRVAL3"
            @test card.name == "CRVAL3"
            @test card.comment == "CRVAL along 3rd axis"
            @test card.value() isa FITSFloat
            @test card.value() ≈ 0.96
            @test card.value() === card.float
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === true
        end
        #
        let card = FITSCard("HIERARCH ESO OBS EXECTIME = +2919 / Expected execution time ")
            @test card.type == FITS_INTEGER
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH ESO OBS EXECTIME"
            @test card.comment == "Expected execution time"
            @test card.value() isa FITSInteger
            @test card.value() == +2919
            @test card.value() === card.integer
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === true
            @test isreal(card) === true
        end
        # FITS cards with undefined value.
        let card = FITSCard("DUMMY   =                        / no value given ")
            @test card.type == FITS_UNDEFINED
            @test card.key == FITS"DUMMY"
            @test card.name == "DUMMY"
            @test card.comment == "no value given"
            @test card.value() isa Missing
            @test card.value() === missing
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        let card = FITSCard("HIERARCH DUMMY   =               / no value given ")
            @test card.type == FITS_UNDEFINED
            @test card.key == FITS"HIERARCH"
            @test card.name == "HIERARCH DUMMY"
            @test card.comment == "no value given"
            @test card.value() isa Missing
            @test card.value() === missing
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === false
            @test isinteger(card) === false
            @test isreal(card) === false
        end
        # Complex valued cards.
        let card = FITSCard("COMPLEX = (1,0)                  / [km/s] some complex value ")
            @test card.type == FITS_COMPLEX
            @test card.key == FITS"COMPLEX"
            @test card.name == "COMPLEX"
            @test card.comment == "[km/s] some complex value"
            @test card.units == "km/s"
            @test card.unitless == "some complex value"
            @test card.value() isa FITSComplex
            @test card.value() ≈ complex(1,0)
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
            @test isassigned(card) === true
            @test isinteger(card) === false
            @test isreal(card) === iszero(imag(card.value()))
        end
        let card = FITSCard("COMPLEX = (-2.7,+3.1d5)          / some other complex value ")
            @test card.type == FITS_COMPLEX
            @test card.key == FITS"COMPLEX"
            @test card.name == "COMPLEX"
            @test card.comment == "some other complex value"
            @test card.value() isa FITSComplex
            @test card.value() ≈ complex(-2.7,+3.1e5)
            @test valtype(card) === typeof(card.value())
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
        let card = FITSCard("END                           ")
            @test card.type == FITS_END
            @test card.key == FITS"END"
            @test card.name == "END"
            @test card.comment == ""
            @test card.value() isa Nothing
            @test card.value() === nothing
            @test valtype(card) === typeof(card.value())
            @test repr(card) isa String
            @test repr("text/plain", card) isa String
            @test repr(card.value) isa String
            @test repr("text/plain", card.value) isa String
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
    end
    @testset "Cards from bytes" begin
        # Logical FITS cards.
        str = "SIMPLE  =                    T / this is a FITS file                            "
        for buf in (make_byte_vector(str), make_discontinuous_byte_vector(str))
            card = FITSCard(buf)
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
        let card = FITSCard("END")
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FITSCard("")
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FITSCard("xEND"; offset=1)
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
        let card = FITSCard("SOMETHING"; offset=250)
            @test card.type === FITS_END
            @test card.key === FITS"END"
        end
    end
    @testset "Cards from pairs" begin
        # Logical FITS cards.
        com = "some comment"
        card = FITSCard("SIMPLE" => (true, com))
        @test card.type === FITS_LOGICAL
        @test card.key === FITS"SIMPLE"
        @test card.name === "SIMPLE"
        @test card.value() === true
        @test card.comment == com
        card = FITSCard("TWO KEYS" => (π, com))
        @test card.type === FITS_FLOAT
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH TWO KEYS"
        @test card.value() ≈ π
        @test card.comment == com
        card = convert(FITSCard, "HIERARCH NAME" => ("some name", com))
        @test card.type === FITS_STRING
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH NAME"
        @test card.value() == "some name"
        @test card.comment == com
        card = convert(FITSCard, "HIERARCH COMMENT" => (nothing, com))
        @test card.type === FITS_COMMENT
        @test card.key === FITS"HIERARCH"
        @test card.name == "HIERARCH COMMENT"
        @test card.value() === nothing
        @test card.comment == com
        card = convert(FITSCard, "COMMENT" => com)
        @test card.type === FITS_COMMENT
        @test card.key === FITS"COMMENT"
        @test card.name == "COMMENT"
        @test card.value() === nothing
        @test card.comment == com
        card = convert(FITSCard, "REASON" => undef)
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value() === missing
        @test card.comment == ""
        card = convert(FITSCard, "REASON" => (missing, com))
        @test card.type === FITS_UNDEFINED
        @test card.key === FITS"REASON"
        @test card.name == "REASON"
        @test card.value() === missing
        @test card.comment == com
    end
 end

end # module
