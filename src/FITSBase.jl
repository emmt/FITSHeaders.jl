"""
    FITSBase

A package implementing methods to store and parse FITS header cards.

"""
module FITSBase

export
    # Quick FITS keyword:
    @Fits_str,
    FitsKey,

    # FITS cards and headers:
    FitsCard,
    FitsHeader,

    # Cards types:
    FitsCardType,
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

# Enumeration of keyword value type identifiers.
@enum FitsCardType::Cint begin
    FITS_LOGICAL   = 0
    FITS_INTEGER   = 1
    FITS_FLOAT     = 2
    FITS_STRING    = 3
    FITS_COMPLEX   = 4
    FITS_COMMENT   = 5
    FITS_UNDEFINED = 6 # no value given
    FITS_END       = 7 # END card
end

const FitsInteger = Int64
const FitsFloat   = Float64
const FitsComplex = Complex{FitsFloat}

include("parser.jl")
import .Parser:
    @Fits_str,
    FITS_CARD_SIZE,
    FITS_BLOCK_SIZE,
    FITS_SHORT_KEYWORD_SIZE,
    FitsKey,
    check_short_keyword,
    check_keyword,
    try_parse_keyword,
    is_comment,
    is_end,
    keyword

include("cards.jl")
import .Cards:
    FitsCard

include("headers.jl")
import .Headers:
    FitsHeader

function __init__()
    @require MappedBuffers="010f96a2-bf57-4630-84b9-647e6f9999c4" begin
        FITSBase.Parser.PointerCapability(::Type{<:MappedBuffers.MappedBuffer}) =
            FITSBase.Parser.PointerFull()
    end
end

end # module
