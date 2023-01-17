"""
    FITSCards.Cards

A sub-module of the `FITSCards` package implementing the methods and properties
for FITS header cards.

"""
module Cards

export FITSCard

using ..FITSCards
using ..FITSCards:
    FITSInteger,
    FITSFloat,
    FITSComplex
import ..FITSCards:
    FITSCardType
using ..FITSCards.Parser:
    EMPTY_STRING,
    ByteBuffer,
    check_keyword,
    make_string,
    parse_complex_value,
    parse_float_value,
    parse_integer_value,
    parse_logical_value,
    parse_string_value,
    scan_card
import ..FITSCards.Parser:
    is_comment,
    is_end

const Undefined = Union{Missing,UndefInitializer}
const END_STRING = "END"
const UNDEF_LOGICAL = false
const UNDEF_INTEGER = zero(FITSInteger)
const UNDEF_COMPLEX = FITSComplex(NaN,NaN)
const UNDEF_FLOAT = FITSComplex(NaN,0.0)
const UNDEF_STRING = EMPTY_STRING

"""
    card = FITSCard(key => (val, com))

builds a FITS header card associating keyword `key` with value `val` and
comment string `com`. The value `val` may be:

- a boolean to yield a card of type `FITS_LOGICAL`;
- an integer to yield a card of type `FITS_INTEGER`;
- a real to yield a card of type `FITS_FLOAT`;
- a complex to yield a card of type `FITS_COMPLEX`;
- a string to yield a card of type `FITS_STRING`;
- `nothing` to yield a card of type `FITS_COMMENT`;
- `missing` or `undef` to yield a card of type `FITS_UNDEFINED`.

The comment may be omitted for a normal FITS card and the value may be omitted
for a commentary FITS card:

    card = FITSCard(key => val::Number)
    card = FITSCard(key => str::AbstractString)

In the 1st case, the comment is assumed to be empty. In the 2nd case, the
string `str` is assumed to be the card comment if `key` is `"COMMENT"` or
`"HISTORY"` and the card value otherwise.

FITS cards have properties:

    card.type    # type of card: FITS_LOGICAL, FITS_INTEGER, etc.
    card.key     # quick key of card: FITS"BITPIX", FITS"HIERARCH", etc.
    card.name    # name of card
    card.value   # callable object representing the card value
    card.comment # comment of card

As the values of FITS keywords have different types, `card.value` does not
yield a Julia value but a callable object. Called without any argument, this
object yields the actual card value:

    card.value() -> val::Union{Bool,$FITSInteger,$FITSFloat,$FITSComplex,String,Nothing,Missing}

but such a call is not *type-stable* as indicated by the type assertion with an
`Union{...}` above. For a type-stable result, the card value can be converted
to a given data type `T`:

    card.value(T)
    convert(T, card.value)

both yield the value of `card` converted to type `T`. For readability, `T` may
be an abstract type: `card.value(Integer)` yields the same result as
`card.value($FITSInteger)`, `card.value(Real)` or `card.value(AbstractFloat)`
yield the same result as `card.value($FITSFloat)`, `card.value(Complex)` yields
the same result as `card.value($FITSComplex)`, and `card.value(AbstractString)`
yields the same result as `card.value(String)`.

To make things easier, a few properties are aliases that yield the card value
converted to a specific type:

    card.logical :: Bool       # is an alias for card.value(Bool)
    card.integer :: $FITSInteger      # is an alias for card.value(Integer)
    card.float   :: $FITSFloat    # is an alias for card.value(Real)
    card.complex :: $FITSComplex # is an alias for card.value(Complex)
    card.string  :: String     # is an alias for card.value(String)

Conversion is automatically attempted if the actual card value is of a
different type, throwing an error if the conversion is not possible or inexact.

`valtype(card)` yields the type of the value of `card`. `isassigned(card)`
yields whether `card` has a value (that is whether it is neither a commentary
card nor a card with an undefined value).

"""
struct FITSCard
    key::FITSKey
    type::FITSCardType
    value_logical::Bool
    value_integer::FITSInteger
    value_complex::FITSComplex
    value_string::String
    name::String
    comment::String
    FITSCard(key::FITSKey, name::AbstractString, val::Bool, com::AbstractString) =
        new(key, FITS_LOGICAL, val, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, name, com)
    FITSCard(key::FITSKey, name::AbstractString, val::Integer, com::AbstractString) =
        new(key, FITS_INTEGER, UNDEF_LOGICAL, val, UNDEF_COMPLEX, UNDEF_STRING, name, com)
    FITSCard(key::FITSKey, name::AbstractString, val::Real, com::AbstractString) =
         new(key, FITS_FLOAT, UNDEF_LOGICAL, UNDEF_INTEGER, val, UNDEF_STRING, name, com)
    FITSCard(key::FITSKey, name::AbstractString, val::Complex, com::AbstractString) =
         new(key, FITS_COMPLEX, UNDEF_LOGICAL, UNDEF_INTEGER, val, UNDEF_STRING, name, com)
    FITSCard(key::FITSKey, name::AbstractString, val::AbstractString, com::AbstractString) =
        new(key, FITS_STRING, UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, val, name, com)
    FITSCard(key::FITSKey, name::AbstractString, ::Undefined, com::AbstractString) =
        new(key, FITS_UNDEFINED, UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, name, com)
    FITSCard(key::FITSKey, name::AbstractString, ::Nothing, com::AbstractString) =
        new(key, key === FITS"END" ? FITS_END : FITS_COMMENT,
            UNDEF_LOGICAL, UNDEF_INTEGER, UNDEF_COMPLEX, UNDEF_STRING, name, com)
end

"""
    FITSCard(buf; offset=0)

yields a `FITSCard` object built by parsing the FITS header card stored in the
string or vector of bytes `buf`. Keyword `offset` can be used to specify the
number of bytes to skip at the beginning of `buf`, so that it is possible to
extract a specific FITS header card, not just the first one. At most, the
$FITS_CARD_SIZE first bytes after the offset are scanned to build the
`FITSCard` object. The next FITS card to parse is then at `offset +
$FITS_CARD_SIZE` and so on.

The considered card may be shorter than $FITS_CARD_SIZE bytes, the result being
exactly the same as if the missing bytes were spaces. If there are no bytes
left, a `FITSCard` object equivalent to the final `END` card of a FITS header
is returned.

"""
function FITSCard(buf::ByteBuffer; offset::Int = 0)
    type, key, name_rng, val_rng, com_rng = scan_card(buf, offset)
    name = type == FITS_END ? END_STRING : make_string(buf, name_rng)
    com = make_string(buf, com_rng)
    if type == FITS_LOGICAL
        return FITSCard(key, name, parse_logical_value(buf, val_rng), com)
    elseif type == FITS_INTEGER
        return FITSCard(key, name, parse_integer_value(buf, val_rng), com)
    elseif type == FITS_FLOAT
        return FITSCard(key, name, parse_float_value(buf, val_rng), com)
    elseif type == FITS_STRING
        return FITSCard(key, name, parse_string_value(buf, val_rng), com)
    elseif type == FITS_COMPLEX
        return FITSCard(key, name, parse_complex_value(buf, val_rng), com)
    elseif type == FITS_UNDEFINED
        return FITSCard(key, name, missing, com)
    else # must be commentary or END card
        return FITSCard(key, name, nothing, com)
    end
end

is_comment(card::FITSCard) = is_comment(card.type)
is_end(card::FITSCard) = is_end(card.type)

# This version shall print something equivalent to Julia code to produce the
# same object. We try to use the most concise syntax.
function Base.show(io::IO, A::FITSCard)
    print(io, "FITSCard(\"")
    print(io, A.name, "\"")
    if A.type != FITS_END
        if A.type == FITS_COMMENT
            if A.key === FITS"COMMENT" || A.key === FITS"HISTORY"
                print(io, " => ")
                show(io, A.comment)
            else
                print(io, " => (nothing, ")
                show(io, A.comment)
                print(io, "=> (nothing, ")
            end
        else
            commented = !isempty(A.comment)
            if commented
                print(io, " => (")
            else
                print(io, " => ")
            end
            if A.type == FITS_LOGICAL
                print(io, A.logical ? "true" : "false")
            elseif A.type == FITS_INTEGER
                show(io, A.integer)
            elseif A.type == FITS_FLOAT
                show(io, A.float)
            elseif A.type == FITS_COMPLEX
                show(io, A.complex)
            elseif A.type == FITS_STRING
                show(io, A.string)
            elseif A.type == FITS_UNDEFINED
                print(io, "missing")
            end
            if commented
                print(io, ", ")
                show(io, A.comment)
                print(io, ")")
            end
        end
    end
    print(io, ")")
end

# This version is for the REPL. We try to approximate FITS syntax.
function Base.show(io::IO, mime::MIME"text/plain", A::FITSCard)
    print(io, "FITSCard: ")
    print(io, A.name)
    if A.type != FITS_END
        if A.key === FITS"HIERARCH"
            print(io, ' ')
        else
            n = ncodeunits(A.name)
            while n < FITS_SHORT_KEYWORD_SIZE
                print(io, ' ')
                n += 1
            end
        end
    end
    if A.type == FITS_COMMENT
        print(io, A.comment)
    elseif A.type != FITS_END
        print(io, "= ")
        if A.type == FITS_LOGICAL
            print(io, A.logical ? 'T' : 'F')
        elseif A.type == FITS_INTEGER
            show(io, A.integer)
        elseif A.type == FITS_FLOAT
            show(io, A.float)
        elseif A.type == FITS_COMPLEX
            print(io, "(")
            show(io, real(A.complex))
            print(io, ", ")
            show(io, imag(A.complex))
            print(io, ")")
        elseif A.type == FITS_STRING
            q = '\''
            print(io, q)
            for c in A.string
                if c == q
                    print(io, q, q)
                else
                    print(io, c)
                end
            end
            print(io, q)
        elseif A.type == FITS_UNDEFINED
            print(io, "<undefined>")
        end
        if !isempty(A.comment)
            print(io, " / ", A.comment)
        end
    end
end

# Callable object representing a FITS card value.
struct FITSCardValue
    parent::FITSCard
end
Base.parent(A::FITSCardValue) = getfield(A, :parent)
(A::FITSCardValue)() = get_value(parent(A))
Base.convert(::Type{T}, A::FITSCardValue) where {T<:FITSCardValue} = A
for T in (Number, Integer, Real, AbstractFloat, Complex,
          AbstractString, String, Nothing, Missing)
    if T === Number
        @eval (A::FITSCardValue)(::Type{T}) where {T<:$T} = get_value(T, parent(A))
        @eval Base.convert(::Type{T}, A::FITSCardValue) where {T<:$T} = A(T)
    else
        @eval (A::FITSCardValue)(::Type{$T}) = get_value($T, parent(A))
        @eval Base.convert(::Type{$T}, A::FITSCardValue) = A($T)
    end
end
Base.show(io::IO, A::FITSCardValue) = show(io, A())
Base.show(io::IO, mime::MIME"text/plain", A::FITSCardValue) = show(io, mime, A())

# If the FITSCard structure changes, it should be almost sufficient to change
# the following simple accessors.
get_type(         A::FITSCard) = getfield(A, :type)
get_key(          A::FITSCard) = getfield(A, :key)
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
    type == FITS_INTEGER  ?                      get_value_integer(A)  :
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
get_value(::Type{Integer},        A::FITSCard) = get_value(FITSInteger, A)
get_value(::Type{Real},           A::FITSCard) = get_value(FITSFloat,   A)
get_value(::Type{AbstractFloat},  A::FITSCard) = get_value(FITSFloat,   A)
get_value(::Type{Complex},        A::FITSCard) = get_value(FITSComplex, A)
get_value(::Type{AbstractString}, A::FITSCard) = get_value(String,      A)
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
    error("value of FITS keyword \"$(get_name(A))\" cannot be converted to `$T`")

# Properties.
Base.propertynames(A::FITSCard) =
    (:type, :key, :name, :value, :comment, :logical, :integer, :float, :string, :complex)
Base.getproperty(A::FITSCard, sym::Symbol) = getproperty(A, Val(sym))
Base.getproperty(A::FITSCard, ::Val{:type   }) = get_type(A)
Base.getproperty(A::FITSCard, ::Val{:key    }) = get_key(A)
Base.getproperty(A::FITSCard, ::Val{:name   }) = get_name(A)
Base.getproperty(A::FITSCard, ::Val{:value  }) = FITSCardValue(A)
Base.getproperty(A::FITSCard, ::Val{:comment}) = get_comment(A)
Base.getproperty(A::FITSCard, ::Val{:logical}) = get_value(Bool, A)
Base.getproperty(A::FITSCard, ::Val{:integer}) = get_value(FITSInteger, A)
Base.getproperty(A::FITSCard, ::Val{:float  }) = get_value(FITSFloat, A)
Base.getproperty(A::FITSCard, ::Val{:string }) = get_value(String, A)
Base.getproperty(A::FITSCard, ::Val{:complex}) = get_value(FITSComplex, A)
@noinline Base.setproperty!(A::FITSCard, sym::Symbol, x) =
    error("attempt to set read-only property of FITS card")

FITSCardType(A::FITSCard) = get_type(A)

Base.isassigned(A::FITSCard) =
    (A.type != FITS_COMMENT) & (A.type != FITS_UNDEFINED) & (A.type != FITS_END)

Base.isinteger(A::FITSCard) =
    (A.type == FITS_INTEGER) | (A.type == FITS_LOGICAL)

Base.isreal(A::FITSCard) =
    (A.type == FITS_FLOAT) |
    (A.type == FITS_INTEGER) |
    (A.type == FITS_LOGICAL) |
    (A.type == FITS_COMPLEX && iszero(imag(get_value_complex(A))))

Base.valtype(A::FITSCard) = valtype(A.type)
Base.valtype(type::FITSCardType) =
    type == FITS_LOGICAL   ? Bool :
    type == FITS_INTEGER   ? FITSInteger :
    type == FITS_FLOAT     ? FITSFloat :
    type == FITS_STRING    ? String :
    type == FITS_COMPLEX   ? FITSComplex :
    type == FITS_UNDEFINED ? Missing :
    Nothing # FITS_COMMENT or FITS_END

# FITS cards can be specified as pairs and conversely.
Base.convert(::Type{T}, A::FITSCard) where {T<:FITSCard} = A
Base.convert(::Type{T}, A::FITSCard) where {T<:Pair} = T(A)
Base.convert(::Type{T}, pair::Pair) where {T<:FITSCard} = T(pair)
Base.Pair(A::FITSCard) = Pair(A.name, (A.value(), A.comment))
Base.Pair{K}(A::FITSCard) where {K} = Pair{K}(A.name, (A.value(), A.comment))
Base.Pair{K,V}(A::FITSCard) where {K,V} = Pair{K,V}(A.name, (A.value(), A.comment))
function FITSCard(pair::Pair{<:AbstractString,
                             <:Tuple{Union{AbstractString,Number,Undefined,Nothing},
                                     AbstractString}})
    key, name = check_keyword(first(pair))
    val, com = last(pair)
    return FITSCard(key, name, val, com)
end
function FITSCard(pair::Pair{<:AbstractString, <:Union{Number,Undefined}})
    key, name = check_keyword(first(pair))
    val = last(pair)
    return FITSCard(key, name, val, EMPTY_STRING)
end
function FITSCard(pair::Pair{<:AbstractString, <:AbstractString})
    key, name = check_keyword(first(pair))
    val_or_com = last(pair)
    return is_comment(key) ?
        FITSCard(key, name, nothing, val_or_com) :
        FITSCard(key, name, val_or_com, EMPTY_STRING)
end

end # module
