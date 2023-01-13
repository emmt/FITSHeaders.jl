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
    FITS_BLOCK_SIZE,
    FITS_SHORT_KEYWORD_SIZE


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

"""
    FITS_SHORT_KEYWORD_SIZE

is the number of bytes in a short FITS keyword, that is all FITS keyword but
the `HIERARCH` ones. If a FITS keyword is shorther than this, it is equivalent
to pad it with ASCII spaces (hexadecimal code 0x20).

"""
const FITS_SHORT_KEYWORD_SIZE = 8

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

"""
    FITSKey(buf, off=0, n=last_byte_index(buf))

encodes the, at most, first $FITS_SHORT_KEYWORD_SIZE bytes (or ASCII
characters) of `buf`, starting at offset `off`, in a 64-bit integer value which
is exactly equal to the first $FITS_SHORT_KEYWORD_SIZE bytes of a FITS keyword
as stored in a FITS header. Argument `buf` may be a string or a vector of
bytes.

Optional argument `n` is the index of the last byte available in `buf`. If
fewer than $FITS_SHORT_KEYWORD_SIZE bytes are available (that is, if `off +
$FITS_SHORT_KEYWORD_SIZE > n`), the result is as if `buf` has been padded with
ASCII spaces (hexadecimal code 0x20).

The only operation that makes sense with an instance of `FITSKey` is comparison
for equality for fast searching of keywords in a FITS header.

The caller may use `@inbounds` macro if it certain that bytes in the range
`off+1:n` are in bounds for `buf`.

For the fastest, but unsafe, computations call:

    FITSKey(Val(:full), buf, off)
    FITSKey(Val(:pad), buf, off, n)

where first argument should be `Val(:full)` if there are at least
$FITS_SHORT_KEYWORD_SIZE bytes available after `off`, and `Val(:pad)`
otherwise. These variants do not perfom bounds checking, it is the caller's
responsibility to insure that the arguments are consistent.

"""
struct FITSKey
    val::UInt64
end

@assert sizeof(FITSKey) == FITS_SHORT_KEYWORD_SIZE

FITSKey() = FITSKey(zero(UInt64))
# NOTE: Other constructors are implemented in parser.jl

Base.iszero(key::FITSKey) = iszero(key.val)
Base.:(==)(a::FITSKey, b::FITSKey) = a.val === b.val
Base.convert(::Type{T}, key::FITSKey) where {T<:Integer} = convert(T, key.val)
Base.UInt64(key::FITSKey) = key.val

"""
    @FITS_str

A macro to construct a 64-bit identifier equivalent to the FITS keyword given
in argument and as it is stored in the header of a FITS file. The argument must
be a short FITS keyword (e.g., not a `HIERARCH` one) specified as a literal
string of, at most, $FITS_SHORT_KEYWORD_SIZE ASCII characters with no trailing
spaces. For example `FITS"SIMPLE"` or `FITS"NAXIS2"`.

The result is the same as that computed by `FITSKey` but since the identifier
is given by a string macro, it is like a constant computed at compile time with
no runtime penalty.

"""
macro FITS_str(str::String)
    FITSKey(check_short_keyword(str))
end

function check_short_keyword end # NOTE: this function is implemented in parser.jl

include("cards.jl")
import .Cards: FITSCard, FITSInteger, FITSFloat, FITSComplex

include("parser.jl")

function __init__()
    @require MappedBuffers="010f96a2-bf57-4630-84b9-647e6f9999c4" begin
        FITSCards.Parser.PointerCapability(::Type{<:MappedBuffers.MappedBuffer}) =
            FITSCards.Parser.PointerFull()
    end
end

end # module
