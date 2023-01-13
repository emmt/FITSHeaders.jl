"""
    FITSCards.Cards

A sub-module of the `FITSCards` package implementing the methods and properties
for FITS header cards.

"""
module Cards

export FITSCard

using ..FITSCards
import ..FITSCards: FITSCardType

const FITSInteger = Int64
const FITSFloat   = Float64
const FITSComplex = Complex{FITSFloat}

const Undefined = Union{Missing,UndefInitializer}
const EMPTY_STRING = ""
const UNDEF_LOGICAL = false
const UNDEF_INTEGER = zero(FITSInteger)
const UNDEF_COMPLEX = FITSComplex(NaN,NaN)
const UNDEF_FLOAT = FITSComplex(NaN,0.0)
const UNDEF_STRING = EMPTY_STRING

"""
    card = FITSCard(key, val, com=$EMPTY_STRING)

builds a FITS header card associating keyword `key` with value `val` and
comment `com`. The value `val` may be:

- a boolean to yield a card of type `FITS_LOGICAL`;
- an integer to yield a card of type `FITS_INTEGER`;
- a real to yield a card of type `FITS_FLOAT`;
- a complex to yield a card of type `FITS_COMPLEX`;
- a string to yield a card of type `FITS_STRING`;
- `nothing` to yield a card of type `FITS_COMMENT`;
- `missing` or `undef` to yield a card of type `FITS_UNDEFINED`.

A FITS card can also be built from a pair:

    card = FITSCard(key => (val, com))
    card = FITSCard(key => val::Number)
    card = FITSCard(key => str::AbstractString)

In the second case, the comment is assumed to be empty. In the third case, the
string `str` is assumed to be the card comment if `key` is `"COMMENT"` or
`"HISTORY"` and the card value otherwise.

FITS cards have properties:

    card.type    # type of card: FITS_LOGICAL, FITS_INTEGER, etc.
    card.name    # name of card
    card.value   # value of card
    card.comment # comment of card

Beware that `card.value` does not yield a *type-stable* result. To retrieve the
card value with a known type, use one of:

    card.logical   # value of card as a Bool
    card.integer   # value of card as an $FITSInteger
    card.float     # value of card as a $FITSFloat
    card.complex   # value of card as a $FITSComplex
    card.string    # value of card as a String

With these properties, conversion is automatically attempted if the actual card
value is of a different type, throwing an error if the conversion is not
possible or inexact.

`valtype(card)` yields the type of the value of `card`. `isassigned(card)`
yields whether `card` has a value (that is whether it is neither a commentary
card nor a card with an undefined value).

"""
struct FITSCard
    type::FITSCardType
    value_logical::Bool
    value_integer::FITSInteger
    value_complex::FITSComplex
    value_string::String
    name::String
    comment::String
    FITSCard(key::AbstractString, val::Bool, com::AbstractString=EMPTY_STRING) =
        new(FITS_LOGICAL, val, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, key, com)
    FITSCard(key::AbstractString, val::Integer, com::AbstractString=EMPTY_STRING) =
        new(FITS_INTEGER, UNDEF_LOGICAL, val, UNDEF_COMPLEX, UNDEF_STRING, key, com)
    FITSCard(key::AbstractString, val::Real, com::AbstractString=EMPTY_STRING) =
         new(FITS_FLOAT, UNDEF_LOGICAL, UNDEF_INTEGER, val, UNDEF_STRING, key, com)
    FITSCard(key::AbstractString, val::Complex, com::AbstractString=EMPTY_STRING) =
         new(FITS_COMPLEX, UNDEF_LOGICAL, UNDEF_INTEGER, val, UNDEF_STRING, key, com)
    FITSCard(key::AbstractString, val::AbstractString, com::AbstractString=EMPTY_STRING) =
        new(FITS_STRING, UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, val, key, com)
    FITSCard(key::AbstractString, ::Undefined, com::AbstractString=EMPTY_STRING) =
        new(FITS_UNDEFINED, UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, key, com)
    FITSCard(key::AbstractString, ::Nothing, com::AbstractString=EMPTY_STRING) =
        new(FITS_COMMENT, UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, key, com)
end

# If the FITSCard structure changes, it should be almost sufficient to change
# the following simple accessors.
get_type(         A::FITSCard) = getfield(A, :type)
get_name(         A::FITSCard) = getfield(A, :name)
get_comment(      A::FITSCard) = getfield(A, :comment)
get_value_logical(A::FITSCard) = getfield(A, :value_logical)
get_value_integer(A::FITSCard) = getfield(A, :value_integer)
get_value_complex(A::FITSCard) = getfield(A, :value_complex)
get_value_float(  A::FITSCard) = real(get_value_complex(A))
get_value_string( A::FITSCard) = getfield(A, :value_string)
get_value(        A::FITSCard) = begin
    type = get_type(A)
    type == FITS_LOGICAL   ? get_value_logical(A) :
    type == FITS_INTEGER   ? get_value_integer(A) :
    type == FITS_FLOAT     ? get_value_float(  A) :
    type == FITS_STRING    ? get_value_string( A) :
    type == FITS_COMPLEX   ? get_value_complex(A) :
    type == FITS_UNDEFINED ? missing :
    nothing # FITS_COMMENT or FITS_END
end
get_value(::Type{Missing}, A::FITSCard) =
    get_type(A) == FITS_UNDEFINED ? missing : conversion_error(Missing, A)
get_value(::Type{Nothing}, A::FITSCard) =
    get_type(A) == FITS_COMMENT ? nothing : conversion_error(Missing, A)
get_value(::Type{String}, A::FITSCard) =
    get_type(A) == FITS_STRING ? get_value_string(A) : conversion_error(String, A)
get_value(::Type{Bool}, A::FITSCard) = begin
    type = get_type(A)
    type == FITS_LOGICAL  ?               get_value_logical(A)  :
    type == FITS_INTEGER  ? convert(Bool, get_value_integer(A)) :
    type == FITS_FLOAT    ? convert(Bool, get_value_float(  A)) :
    type == FITS_COMPLEX  ? convert(Bool, get_value_complex(A)) :
    conversion_error(Bool, A)
end
get_value(::Type{FITSInteger}, A::FITSCard) = begin
    type = get_type(A)
    type == FITS_INTEGER  ?              get_value_integer(A)  :
    type == FITS_LOGICAL  ? convert(FITSInteger, get_value_logical(A)) :
    type == FITS_FLOAT    ? convert(FITSInteger, get_value_float(  A)) :
    type == FITS_COMPLEX  ? convert(FITSInteger, get_value_complex(A)) :
    conversion_error(FITSInteger, A)
end
get_value(::Type{FITSFloat}, A::FITSCard) = begin
    type = get_type(A)
    type == FITS_FLOAT    ?                    get_value_float(  A)  :
    type == FITS_LOGICAL  ? convert(FITSFloat, get_value_logical(A)) :
    type == FITS_INTEGER  ? convert(FITSFloat, get_value_integer(A)) :
    type == FITS_COMPLEX  ? convert(FITSFloat, get_value_complex(A)) :
    conversion_error(FITSFloat, A)
end
get_value(::Type{FITSComplex}, A::FITSCard) = begin
    type = get_type(A)
    type == FITS_COMPLEX  ?                      get_value_complex(A)  :
    type == FITS_FLOAT    ? convert(FITSComplex, get_value_float(  A)) :
    type == FITS_LOGICAL  ? convert(FITSComplex, get_value_logical(A)) :
    type == FITS_INTEGER  ? convert(FITSComplex, get_value_integer(A)) :
    conversion_error(FITSComplex, A)
end
get_value(::Type{T}, A::FITSCard) where {T<:Number} = begin
    type = get_type(A)
    type == FITS_LOGICAL  ? convert(T, get_value_logical(A)) :
    type == FITS_INTEGER  ? convert(T, get_value_integer(A)) :
    type == FITS_FLOAT    ? convert(T, get_value_float(  A)) :
    type == FITS_COMPLEX  ? convert(T, get_value_complex(A)) :
    conversion_error(T, A)
end
get_value(T::Type, A::FITSCard) = conversion_error(T, A) # catch errors
@noinline conversion_error(T::Type, A::FITSCard) =
    error("value of FITS keyword $(get_name(A)) cannot be converted to $T")

# Properties.
Base.propertynames(A::FITSCard) =
    (:type, :name, :value, :comment, :logical, :integer, :float, :string, :complex)
Base.getproperty(A::FITSCard, sym::Symbol) = getproperty(A, Val(sym))
Base.getproperty(A::FITSCard, ::Val{:type   }) = get_type(A)
Base.getproperty(A::FITSCard, ::Val{:name   }) = get_name(A)
Base.getproperty(A::FITSCard, ::Val{:value  }) = get_value(A)
Base.getproperty(A::FITSCard, ::Val{:comment}) = get_comment(A)
Base.getproperty(A::FITSCard, ::Val{:logical}) = get_value(Bool, A)
Base.getproperty(A::FITSCard, ::Val{:integer}) = get_value(FITSInteger, A)
Base.getproperty(A::FITSCard, ::Val{:float  }) = get_value(FITSFloat, A)
Base.getproperty(A::FITSCard, ::Val{:string }) = get_value(String, A)
Base.getproperty(A::FITSCard, ::Val{:complex}) = get_value(FITSComplex, A)
@noinline Base.setproperty!(A::FITSCard, sym::Symbol, x) =
    error("attempt to set read-only property of FITS card")

FITSCardType(A::FITSCard) = get_type(A)

Base.isassigned(A::FITSCard) = (A.type != FITS_COMMENT) & (A.type != FITS_UNDEFINED)

Base.isinteger(A::FITSCard) = (A.type == FITS_INTEGER) | (A.type == FITS_LOGICAL)

Base.isreal(A::FITSCard) =
    (A.type == FITS_FLOAT) |
    (A.type == FITS_INTEGER) |
    (A.type == FITS_LOGICAL) |
    (A.type == FITS_COMPLEX && iszero(imag(getfield(A, :value_complex))))

Base.valtype(A::FITSCard) = valtype(A.type)
Base.valtype(type::FITSCardType) =
    type == FITS_LOGICAL   ? Bool :
    type == FITS_INTEGER   ? FITSInteger :
    type == FITS_FLOAT     ? FITSFloat :
    type == FITS_STRING    ? String :
    type == FITS_COMPLEX   ? FITSComplex :
    type == FITS_UNDEFINED ? Missing :
    Nothing # FITS_COMMENT or FITS_END

# FIXME: use encode_key
is_comment(key::AbstractString) = (key == "HISTORY" || key == "COMMENT")
is_comment(A::FITSCard) = (A.type == FITS_COMMENT)

# FITS cards can be specified as pairs and conversely.
Base.convert(::Type{T}, A::FITSCard) where {T<:FITSCard} = A
Base.convert(::Type{T}, A::FITSCard) where {T<:Pair} = T(A)
Base.convert(::Type{T}, pair::Pair) where {T<:FITSCard} = T(pair)
Base.Pair(A::FITSCard) = Pair(A.name, (A.value, A.comment))
Base.Pair{K}(A::FITSCard) where {K} = Pair{K}(A.name, (A.value, A.comment))
Base.Pair{K,V}(A::FITSCard) where {K,V} = Pair{K,V}(A.name, (A.value, A.comment))
function FITSCard(pair::Pair{<:AbstractString,
                             <:Tuple{Union{AbstractString,Number,Undefined,Nothing},
                                     AbstractString}})
    return FITSCard(first(pair), last(pair)...)
end
function FITSCard(pair::Pair{<:AbstractString, <:Union{Number,Undefined}})
    return FITSCard(first(pair), last(pair))
end
function FITSCard(pair::Pair{<:AbstractString, <:AbstractString})
    key = first(pair)
    return is_comment(key) ? FITSCard(key, nothing, last(pair)) : FITSCard(key, last(pair))
end

end # module
