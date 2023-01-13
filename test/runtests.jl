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

_load(::Type{T}, buf::Vector{UInt8}, off::Integer = 0) where {T} =
    GC.@preserve buf unsafe_load(Base.unsafe_convert(Ptr{T}, buf) + off)
_store!(::Type{T}, buf::Vector{UInt8}, x, off::Integer = 0) where {T} =
    GC.@preserve buf unsafe_store!(Base.unsafe_convert(Ptr{T}, buf) + off, convert(T, x)::T)

@testset "FITSCards.jl" begin
    @testset "unsafe_load/unsafe_store!" begin
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
    end
    @testset "FITS constants" begin
        @test FITS_SHORT_KEYWORD_SIZE == 8
        @test FITS_CARD_SIZE == 80
        @test FITS_BLOCK_SIZE == 2880
    end
    @testset "FITS keywords" begin
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
    end
    @testset "Parser" begin
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
 end

end # module
