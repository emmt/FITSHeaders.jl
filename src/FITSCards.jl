"""
    FITSCards

A package implementing methods to store and parse FITS header cards.

"""
module FITSCards

export
    # Fast FITS keyword:
    @FITS_str,
    FITSKey,

    # FITS cards:
    FITSCard,

    # Cards types:
    FITSCardType,
    FITS_LOGICAL,
    FITS_INTEGER,
    FITS_FLOAT,
    FITS_STRING,
    FITS_COMPLEX,
    FITS_COMMENT,
    FITS_UNDEFINED,
    FITS_END,

    # Sizes:
    FITS_CARD_SIZE,
    FITS_BLOCK_SIZE



using Requires

"""
    FITS_CARD_SIZE

is the number of bytes per FITS header card.

"""
const FITS_CARD_SIZE = 80

"""
    FITS_BLOCK_SIZE

is the number of bytes per FITS header/data block.

"""
const FITS_BLOCK_SIZE = 36*FITS_CARD_SIZE # 2880

# Enumeration of keyword value type identifiers.
@enum FITSCardType::Cint begin
    FITS_LOGICAL   = 0
    FITS_INTEGER   = 1
    FITS_FLOAT     = 2
    FITS_STRING    = 3
    FITS_COMPLEX   = 4
    FITS_COMMENT   = 5
    FITS_UNDEFINED = 6 # no value given
    FITS_END       = 7 # END card
end

struct FITSKey
    val::UInt64
end

"""
    FITSKey(buf, off=0, n=last_byte_index(buf))

encodes the, at most, first 8 bytes (or ASCII characters) of `buf`, starting at
offset `off`, in a 64-bit integer value which is exactly equal to the first
8 bytes of a FITS keyword as stored in a FITS header. Argument `buf` may be a
string or a vector of bytes.

Optional argument `n` is the index of the last byte available in `buf`. If
fewer than 8 bytes are available (that is, if `off + 8 > n`), the result is as
if `buf` has been padded with ASCII spaces (hexadecimal code 0x20).

The only operation that makes sense with an instance of `FITSKey` is comparison
for equality for fast searching of keywords in a FITS header.

The caller may use `@inbounds` macro if it certain that bytes in the range
`off+1:n` are in bounds for `buf`.

For the fastest, but unsafe, computations call:

    FITSKey(Val(:full), buf, off)
    FITSKey(Val(:trunc), buf, off, n)

where first argument should be `Val(:full)` if there are at least 8 bytes
available after `off`, and `Val(:trunc)` otherwise. These variants do not
perfom bounds checking, it is the caller's responsibility to insure that the
arguments are consistent.

"""
FITSKey() = FITSKey(zero(UInt64))
# NOTE: Other constructors are implemented in parser.jl

Base.iszero(key::FITSKey) = iszero(key.val)
Base.:(==)(a::FITSKey, b::FITSKey) = a.val === b.val

"""
    @FITS_str

A macro to construct a 64-bit identifier equivalent to the FITS keyword given
in argument and as it is stored in the header of a FITS file. The argument must
be a simple FITS keyword (e.g., not a `HIERARCH` one) specified as a literal
string of, at most, 8 ASCII characters with no trailing spaces. For example
`FITS"SIMPLE"` or `FITS"NAXIS2"`.

The result is the same as that computed by `FITSKey` but since the identifier
is given by a string macro, it is like a constant computed at compile time with
no runtime penalty.

"""
macro FITS_str(str::String)
    FITSKey(check_simple_key(str))
end

"""
    FITSCards.check_simple_key(str) -> str

returns the string `str` throwing an exception if `str` is not a simple FITS
keyword consisting in, at most, 8 ASCII characters from the restricted set of
upper case letters (bytes 0x41 to 0x5A), decimal digits (hexadecimal codes 0x30
to 0x39), hyphen (hexadecimal code 0x2D), or underscore (hexadecimal code
0x5F).

""" function check_simple_key end
# NOTE: This function is implemented in parser.jl

include("cards.jl")
import .Cards: FITSCard

include("parser.jl")

function __init__()
    @require MappedBuffers="010f96a2-bf57-4630-84b9-647e6f9999c4" begin
        @inline FITSKey(val::Val, buf::MappedBuffers.MappedBuffer, args...) =
            FITSKey(val, MappedBuffers.storage(buf), args...)
    end
end

end # module
