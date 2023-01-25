"""
    FITSCards

A package implementing methods to store and parse FITS header cards.

"""
module FITSCards

export
    # Quick FITS keyword:
    @FITS_str,
    FITSKey,

    # FITS cards and headers:
    FITSCard,
    FITSHeader,

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

const FITSInteger = Int64
const FITSFloat   = Float64
const FITSComplex = Complex{FITSFloat}

include("parser.jl")
import .Parser:
    @FITS_str,
    FITS_CARD_SIZE,
    FITS_BLOCK_SIZE,
    FITS_SHORT_KEYWORD_SIZE,
    FITSKey,
    check_short_keyword,
    check_keyword,
    is_comment,
    is_end,
    keyword

include("cards.jl")
import .Cards:
    FITSCard

include("headers.jl")
import .Headers:
    FITSHeader

function __init__()
    @require MappedBuffers="010f96a2-bf57-4630-84b9-647e6f9999c4" begin
        FITSCards.Parser.PointerCapability(::Type{<:MappedBuffers.MappedBuffer}) =
            FITSCards.Parser.PointerFull()
    end
end

end # module
