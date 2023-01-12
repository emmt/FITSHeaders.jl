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
        @test FITS_CARD_SIZE == 80
        @test FITS_BLOCK_SIZE == 2880
    end
    @testset "FITS keywords" begin
        @test FITS"SIMPLE"   === make_FITSKey("SIMPLE  ")
        @test FITS"BITPIX"   === make_FITSKey("BITPIX  ")
        @test FITS"NAXIS"    === make_FITSKey("NAXIS   ")
        @test FITS"COMMENT"  === make_FITSKey("COMMENT ")
        @test FITS"HISTORY"  === make_FITSKey("HISTORY ")
        @test FITS"HIERARCH" === make_FITSKey("HIERARCH")
        @test FITS""         === make_FITSKey("        ")
        @test FITS"END"      === make_FITSKey("END     ")
    end
end

end # module
