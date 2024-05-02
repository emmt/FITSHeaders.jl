"""
    FITSHeaders

A package implementing methods to store and parse FITS header cards.

"""
module FITSHeaders

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

# Julia types for card values.
const FitsInteger = Int64
const FitsFloat   = Float64
const FitsComplex = Complex{FitsFloat}

# Types equivalent to undefined FITS card value.
const Undefined = Union{Missing,UndefInitializer}

# Allowed types for FITS card names.
const CardName = Union{AbstractString,Symbol}

# Allowed types for FITS card values (including commentary cards).
const CardValue = Union{Real,AbstractString,Complex,Undefined,Nothing}

# Allowed types for FITS card comments.
const CardComment = Union{AbstractString,Nothing}

# Signature of pairs that may possibly be converted into FITS cards.
#
#NOTE: The value type is purposely unspecific to allow for collections of card
# pairs mixing different value types.
const CardPair{K<:CardName,V} = Pair{K,V}

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
    is_structural,
    is_comment,
    is_naxis,
    is_end,
    keyword

include("cards.jl")
import .Cards:
    FitsCard

include("headers.jl")
import .Headers:
    FitsHeader,
    FullName

function __init__()
    @require MappedBuffers="010f96a2-bf57-4630-84b9-647e6f9999c4" begin
        FITSHeaders.Parser.PointerCapability(::Type{<:MappedBuffers.MappedBuffer}) =
            FITSHeaders.Parser.PointerFull()
    end
end

end # module
