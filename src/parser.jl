"""
    FITSCards.Parser

A sub-module of the `FITSCards` package implementing methods for parsing FITS
header cards.

"""
module Parser

using ..FITSCards
using ..FITSCards: FITSInteger, FITSFloat, FITSComplex
import ..FITSCards: FITSCard, FITSKey, check_short_keyword

using ..FITSCards.Cards: EMPTY_STRING

using Base: @propagate_inbounds

"""
    FITSCards.Parser.PointerCapability(T) -> Union{PointerNone,PointerFull}

yields whether `Base.unsafe_convert(Ptr{UInt8},obj)` and
`Base.cconvert(Ptr{UInt8},obj)` are fully implemented for an object `obj` of
type `T`. This also means that the object is stored in memory for some
contiguous range of addresses.

"""
abstract type PointerCapability end
struct PointerNone <: PointerCapability end
struct PointerFull <: PointerCapability end
PointerCapability(A::Any) = PointerCapability(typeof(A))
PointerCapability(::Type{<:Union{Array,String,SubString{String}}}) = PointerFull()
PointerCapability(::Type) = PointerNone()

"""
    FITSCards.Parser.ByteString

is the union of types of strings that can be considered as vectors of bytes to
implement fast parsing methods (see [`FITSCards.Parser.ByteBuffer`](@ref)).

FITS header cards consist in character from the restricted set of ASCII
characters from `' '` to `'~'` (hexadecimal codes 0x20 to 0x7E). Hence Julia
strings (encoded in ASCII or in UTF8) can be treated as vectors of bytes.

"""
const ByteString = Union{String,SubString{String}}

"""
    FITSCards.Parser.ByteVector

is an alias for types that can be considered as vectors of bytes to
implement fast parsing methods (see [`FITSCards.Parser.ByteBuffer`](@ref)).

"""
const ByteVector = AbstractVector{UInt8}

"""
    FITSCards.Parser.ByteBuffer

is the union of types that can be considered as buffers of bytes and that can
treated as vectors of bytes to parse FITS header cards using the following
helper functions (assuming `A isa ByteBuffer` holds):

    first_byte_index(A) # yields the index of the first byte in A
    last_byte_index(A)  # yields the index of the last byte in A
    byte_index_range(A) # yields the range of byte indices in A
    get_byte(A,i)       # yields the i-th byte from A

See [`FITSCards.Parser.ByteString`](@ref) and [`FITSCards.Parser.ByteVector`](@ref).

"""
const ByteBuffer = Union{ByteString,ByteVector}

"""
    FITSCards.Parser.get_byte(T = UInt8, A, i)

yields the `i`-th byte of `A` which may be a vector of bytes or a string.
Optional first argument `T` is to specify the data type of the returned value.

When parsing a FITS header or keyword, it is possible to specify the index `n`
of the last available byte in `A` and call:

   FITSCards.Parser.get_byte(T = UInt8, A, i, n)

which yields the `i`-th byte of `A` if `i ≤ n` and `0x20` (an ASCII space)
otherwise.

This function propagates the `@inbounds` macro.

"""
@inline @propagate_inbounds get_byte(A::ByteVector, i::Int) = getindex(A, i)
@inline @propagate_inbounds get_byte(A::ByteString, i::Int) = codeunit(A, i)::UInt8
@inline @propagate_inbounds get_byte(A, i::Int, n::Int) = (i ≤ n ? get_byte(A, i) : 0x20)
@inline @propagate_inbounds get_byte(::Type{T}, args...) where {T<:Unsigned} = get_byte(args...) % T

"""
    FITSCards.Parser.first_byte_index(A)

yields the index of the first byte in `A`.

"""
@inline first_byte_index(A::ByteString) = firstindex(A)
@inline first_byte_index(A::ByteVector) = firstindex(A)

"""
    FITSCards.Parser.last_byte_index(A)

yields the index of the last byte in `A`.

"""
@inline last_byte_index(A::ByteString) = ncodeunits(A) + (firstindex(A) - 1)
@inline last_byte_index(A::ByteVector) = lastindex(A)

"""
    FITSCards.Parser.byte_index_range(A)

yields the range of byte indices in `A`.

"""
@inline byte_index_range(buf::ByteBuffer) = first_byte_index(buf):last_byte_index(buf)

# Yields an empty index range starting at given index.
empty_range(i::Int = 1) = i:i-1

@inline function check_byte_index(buf::ByteBuffer, i::Int)
    i_first, i_last = first_byte_index(buf), last_byte_index(buf)
    (i_first ≤ i ≤ i_last) || throw(BoundsError(buf, i))
end

@inline function check_byte_index(buf::ByteBuffer, rng::AbstractUnitRange{Int},
                                  n::Int = last_byte_index(buf))
    check_byte_index(buf, first(rng), last(rng), n)
end

@inline function check_byte_index(buf::ByteBuffer, i::Int, j::Int,
                                        n::Int = last_byte_index(buf))
    ((i > j) | ((i ≥ first_byte_index(buf)) & (j ≤ n))) || throw(BoundsError(buf, i:j))
end

function check_short_keyword(str::ByteString)
    rng = byte_index_range(str)
    @inbounds for i in rng
        is_keyword(get_byte(str, i)) || error("invalid character in short FITS keyword \"$str\"")
    end
    length(rng) ≤ FITS_SHORT_KEYWORD_SIZE || error("too many characters in FITS keyword \"$str\"")
    return str
end

equal(b::UInt8, c::Char) = equal(b, UInt8(c))
equal(b::T, c::T) where {T} = b === c

between(x::UInt8, lo::Char, hi::Char) = between(x, UInt8(lo), UInt8(hi))
between(x::T, lo::T, hi::T) where {T} = (lo ≤ x) & (x ≤ hi)

is_digit(c::Union{Char,UInt8}) = between(c, '0', '9')
is_uppercase(c::Union{Char,UInt8}) = between(c, 'A', 'Z')
is_lowercase(c::Union{Char,UInt8}) = between(c, 'a', 'z')
is_space(c::Union{Char,UInt8}) = equal(c, ' ')
is_quote(c::Union{Char,UInt8}) = equal(c, '\'')
is_equals_sign(c::Union{Char,UInt8}) = equal(c, '=')
is_hyphen(c::Union{Char,UInt8}) = equal(c, '-')
is_underscore(c::Union{Char,UInt8}) = equal(c, '_')
is_comment_separator(c::Union{Char,UInt8}) = equal(c, '/')
is_opening_parenthesis(c::Union{Char,UInt8}) = equal(c, '(')
is_closing_parenthesis(c::Union{Char,UInt8}) = equal(c, ')')
is_restricted_ascii(c::Union{Char,UInt8}) = between(c, ' ', '~')
is_keyword(c::Union{Char,UInt8}) = is_digit(c) | is_uppercase(c) | is_hyphen(c) | is_underscore(c)

@inline function FITSKey(buf::ByteBuffer, off::Int = 0)
    n = last_byte_index(buf)
    @boundscheck check_byte_index(buf, off+1, min(off+FITS_SHORT_KEYWORD_SIZE, n), n)
    off + FITS_SHORT_KEYWORD_SIZE ≤ n ? FITSKey(Val(:full), buf, off) : FITSKey(Val(:pad), buf, off, n)
end

@inline function FITSKey(buf::ByteBuffer, off::Int, n::Int)
    @boundscheck check_byte_index(buf, off+1, min(off+FITS_SHORT_KEYWORD_SIZE, n))
    off + FITS_SHORT_KEYWORD_SIZE ≤ n ? FITSKey(Val(:full), buf, off) : FITSKey(Val(:pad), buf, off, n)
end

@inline function Base.checkbounds(::Type{FITSKey}, buf::ByteBuffer, i::Int, j::Int,
                                  n::Int = last_byte_index(buf))
    ((i > j) | (i ≥ first_byte_index(buf)) & (j ≤ n)) || throw(BoundsError(buf, i:j))
end

@inline FITSKey(val::Val{:full}, buf::ByteBuffer, off::Int) =
    FITSKey(PointerCapability(buf), val, buf, off)

@inline FITSKey(::PointerFull, ::Val{:full}, buf::Vector{UInt8}, off::Int) =
    GC.@preserve buf unsafe_load(Base.unsafe_convert(Ptr{FITSKey}, buf) + off)

@inline function FITSKey(::PointerCapability, ::Val{:full}, buf::ByteBuffer, off::Int)
    @inbounds begin
        @static if ENDIAN_BOM == 0x04030201
            # Little-endian byte order.
            k = (get_byte(UInt64, buf, off + 1) <<  0) |
                (get_byte(UInt64, buf, off + 2) <<  8) |
                (get_byte(UInt64, buf, off + 3) << 16) |
                (get_byte(UInt64, buf, off + 4) << 24) |
                (get_byte(UInt64, buf, off + 5) << 32) |
                (get_byte(UInt64, buf, off + 6) << 40) |
                (get_byte(UInt64, buf, off + 7) << 48) |
                (get_byte(UInt64, buf, off + 8) << 56)
        elseif ENDIAN_BOM == 0x01020304
            # Big-endian byte order.
            k = (get_byte(UInt64, buf, off + 1) << 56) |
                (get_byte(UInt64, buf, off + 2) << 48) |
                (get_byte(UInt64, buf, off + 3) << 40) |
                (get_byte(UInt64, buf, off + 4) << 32) |
                (get_byte(UInt64, buf, off + 5) << 24) |
                (get_byte(UInt64, buf, off + 6) << 16) |
                (get_byte(UInt64, buf, off + 7) <<  8) |
                (get_byte(UInt64, buf, off + 8) <<  0)
        else
            error("unsupported byte order")
        end
    end
    return FITSKey(k)
end

@inline function FITSKey(::Val{:pad}, buf::ByteBuffer, off::Int, n::Int)
    @inbounds begin
        @static if ENDIAN_BOM == 0x04030201
            # Little-endian byte order.
            k = (get_byte(UInt64, buf, off + 1, n) <<  0) |
                (get_byte(UInt64, buf, off + 2, n) <<  8) |
                (get_byte(UInt64, buf, off + 3, n) << 16) |
                (get_byte(UInt64, buf, off + 4, n) << 24) |
                (get_byte(UInt64, buf, off + 5, n) << 32) |
                (get_byte(UInt64, buf, off + 6, n) << 40) |
                (get_byte(UInt64, buf, off + 7, n) << 48) |
                (get_byte(UInt64, buf, off + 8, n) << 56)
        elseif ENDIAN_BOM == 0x01020304
            # Big-endian byte order.
            k = (get_byte(UInt64, buf, off + 1, n) << 56) |
                (get_byte(UInt64, buf, off + 2, n) << 48) |
                (get_byte(UInt64, buf, off + 3, n) << 40) |
                (get_byte(UInt64, buf, off + 4, n) << 32) |
                (get_byte(UInt64, buf, off + 5, n) << 24) |
                (get_byte(UInt64, buf, off + 6, n) << 16) |
                (get_byte(UInt64, buf, off + 7, n) <<  8) |
                (get_byte(UInt64, buf, off + 8, n) <<  0)
        else
            error("unsupported byte order")
        end
    end
    return FITSKey(k)
end

is_comment(key::FITSKey) = (key == FITS"COMMENT") | (key == FITS"HISTORY")

for sym in (:logical, :integer, :float, :string, :complex)
    parse_func = Symbol("parse_$(sym)_value")
    try_parse_func = Symbol("try_parse_$(sym)_value")
    mesg = "failed to parse value of $sym FITS card"
    @eval begin
        @inline function $parse_func(buf::ByteBuffer, rng::AbstractUnitRange{Int})
            val = $try_parse_func(buf, rng)
            isnothing(val) && throw(ArgumentError($mesg))
            return val
        end
        $try_parse_func(buf::ByteBuffer) =
            @inbounds $try_parse_func(buf, byte_index_range(buf))
    end
end

function try_parse_logical_value(buf::ByteBuffer,
                                 rng::AbstractUnitRange{Int})
    i = first(rng)
    last(rng) == i || return nothing
    @boundscheck check_byte_index(buf, i)
    @inbounds b = get_byte(buf, i)
    return equal(b, 'T') ? true : equal(b, 'F') ? false : nothing
end

function try_parse_integer_value(buf::ByteBuffer,
                                 rng::AbstractUnitRange{Int})
    len = length(rng)
    len > 0 || return nothing
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        # Proceed as if the value was negative to avoid overflows, because
        # abs(typemin(Int)) > typemax(Int).
        i_first, i_last = first(rng), last(rng)
        b = get_byte(buf, i_first)
        negate = true
        if equal(b, '-')
            negate = false
            i_first += 1
        elseif equal(b, '+')
            i_first += 1
        end
        i_first ≤ i_last || return nothing # no digits
        val = zero(FITSInteger)
        off = oftype(val, '0')
        ten = oftype(val, 10)
        for i in i_first:i_last
            b = get_byte(buf, i)
            is_digit(b) || return nothing
            val = ten*val - (oftype(val, b) - off)
            val ≤ zero(val) || return nothing # integer overflow
        end
        return negate ? -val : val
    end
end

# Replace 'd' or 'D' by 'e' and leave other characters unchanged.
filter_character_in_float_value(c::Union{UInt8,Char}) =
    ifelse(equal(c, 'D')|equal(c, 'd'), oftype(c, 'e'), c)

# Similar to try_parse_float_value but characters have been filtered
# and `len` is the number of bytes to process.
function try_parse_float(wrk::Vector{UInt8}, len::Int = length(wrk))
    len > 0 || return nothing
    obj = Base.cconvert(Ptr{UInt8}, wrk) # object to be preserved
    ptr = Base.unsafe_convert(Ptr{UInt8}, obj)
    str = GC.@preserve obj unsafe_string(ptr, len)
    return tryparse(FITSFloat, str)
end

function try_parse_float_value(buf::ByteBuffer,
                               rng::AbstractUnitRange{Int})
    len = length(rng)
    len > 0 || return nothing
    @boundscheck check_byte_index(buf, rng)
    # Use a temporary array to copy the range of bytes replacing 'd' and 'D' by 'e'.
    wrk = Array{UInt8}(undef, len)
    off = first(rng) - firstindex(wrk)
    @inbounds for i in eachindex(wrk)
        wrk[i] = filter_character_in_float_value(get_byte(buf, off + i))
    end
    return try_parse_float(wrk, len)
end

function try_parse_float_value(buf::ByteBuffer,
                               rng::AbstractUnitRange{Int},
                               wrk::Vector{UInt8})
    len = length(rng)
    len > 0 || return nothing
    @boundscheck check_byte_index(buf, rng)
    # Use a temporary array to copy the range of bytes replacing 'd' and 'D' by 'e'.
    length(wrk) ≥ len || resize!(wrk, len)
    i_first = firstindex(wrk)
    i_last = i_first - 1 + len
    off = first(rng) - i_first
    @inbounds for i in i_first:i_last
        wrk[i] = filter_character_in_float_value(get_byte(buf, off + i))
    end
    return try_parse_float(wrk, len)
end

function try_parse_string_value(buf::ByteBuffer,
                                rng::AbstractUnitRange{Int})
    len = length(rng)
    len > 0 || return nothing
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        # Check whether we do have a quoted string.
        i_first, i_last = first(rng), last(rng)
        (len ≥ 2 &&
            is_quote(get_byte(buf, i_first)) &&
            is_quote(get_byte(buf, i_last))) || return nothing
        i_first += 1 # remove opening quote
        i_last -= 1 # remove closing quote
        # Get rid of trailing spaces.
        while i_last ≥ i_first && is_space(get_byte(buf, i_last))
            i_last -= 1
        end
        i_last ≥ i_first || return EMPTY_STRING
        # Copy the string into a temporary buffer taking care of escaped
        # quotes. Trailing spaces have already been stripped, so it is not
        # necessary to treat spaces specially.
        #
        # NOTE: We cannot use a simple for-loop because indices may have to be
        #       incremented inside the loop.
        wrk = Array{UInt8}(undef, i_last - i_first + 1)
        i = i_first - 1
        j = firstindex(wrk) - 1
        while i < i_last
            b = get_byte(buf, i += 1)
            if is_quote(b)
                # Next character must aslo be a quote.
                i < i_last || return nothing # error
                b = get_byte(buf, i += 1)
                is_quote(b) || return nothing # error
            end
            wrk[j += 1] = b
        end
        len = j - firstindex(wrk) + 1
        if len ≤ 0
            return EMPTY_STRING
        else
            # Convert temporary buffer into a string. Cannot use String(wrk)
            # because string length may be smaller tahn taht of the buffer.
            obj = Base.cconvert(Ptr{UInt8}, wrk) # object to be preserved
            ptr = Base.unsafe_convert(Ptr{UInt8}, obj)
            return GC.@preserve obj unsafe_string(ptr, len)
        end
    end
end

function try_parse_complex_value(buf::ByteBuffer,
                                 rng::AbstractUnitRange{Int})
    len = length(rng)
    len > 0 || return nothing
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        # Check whether we do have a string like "(re,im)", hence with at least
        # 5 characters, starting with '(' and ending with ')'.
        i_first, i_last = first(rng), last(rng)
        (len ≥ 5 &&
            is_opening_parenthesis(get_byte(buf, i_first)) &&
            is_closing_parenthesis(get_byte(buf, i_last))) || return nothing
        i_first += 1 # remove opening parenthesis
        i_last -= 1 # remove closing parenthesis
        len -= 2
        # Copy the real and imaginary parts into a temporary buffer taking care
        # of the exponent.
        #
        # NOTE: We cannot use a simple for-loop because indices may have to be
        #       incremented inside the loop.
        wrk = Array{UInt8}(undef, len)
        i = i_first - 1
        j = firstindex(wrk) - 1
        re = NaN
        state = 1
        while i < i_last
            b = get_byte(buf, i += 1)
            if equal(b, ',')
                # Parse real part.
                state == 1 || break
                val = try_parse_float(wrk, j - firstindex(wrk) + 1)
                isnothing(val) && break
                re = oftype(re, val)
                state = 2
                j = firstindex(wrk) - 1
            else
                wrk[j += 1] = filter_character_in_float_value(b)
            end
        end
        if state == 2
            # Parse imaginary part.
            im = try_parse_float(wrk, j - firstindex(wrk) + 1)
            isnothing(im) || return FITSComplex(re, im)
        end
        return nothing
    end
end

# Extend FITSCard constructor. # FIXME: speedup string allocation.
function FITSCard(buf::ByteBuffer, off::Int = 0)
    type, key, key_rng, val_rng, com_rng = scan_card(buf, off)
    name = make_string(buf, key_rng)
    com = make_string(buf, com_rng)
    if type == FITS_LOGICAL
        return FITSCard(name, parse_logical_value(buf, val_rng), com)
    elseif type == FITS_INTEGER
        return FITSCard(name, parse_integer_value(buf, val_rng), com)
    elseif type == FITS_FLOAT
        return FITSCard(name, parse_float_value(buf, val_rng), com)
    elseif type == FITS_STRING
        return FITSCard(name, parse_string_value(buf, val_rng), com)
    elseif type == FITS_COMPLEX
        return FITSCard(name, parse_complex_value(buf, val_rng), com)
    elseif type == FITS_UNDEFINED
        return FITSCard(name, missing, com)
    else # must be commentary or END card
        return FITSCard(name, nothing, com)
    end
end

"""
    FITSCards.Parser.make_string(buf, rng) -> str::String

yields a string from the bytes of `buf` in the range `rng`.

"""
@inline function make_string(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    len = length(rng)
    iszero(len) && return EMPTY_STRING
    @boundscheck check_byte_index(buf, rng)
    if PointerCapability(buf) === PointerFull()
        # Directly build a string from the buffer.
        off = first(rng) - 1
        obj = Base.cconvert(Ptr{UInt8}, buf) # object to be preserved
        ptr = Base.unsafe_convert(Ptr{UInt8}, obj) + off
        return GC.@preserve obj unsafe_string(ptr, len)
    else
        # Use a temporary array to copy the range of bytes.
        tmp = Array{UInt8}(undef, len)
        off = first(rng) - firstindex(tmp)
        @inbounds for i in eachindex(tmp)
            tmp[i] = get_byte(buf, off + i)
        end
        if true # FIXME: select which one is the fastest
            obj = Base.cconvert(Ptr{UInt8}, tmp) # object to be preserved
            ptr = Base.unsafe_convert(Ptr{UInt8}, obj)
            return GC.@preserve obj unsafe_string(ptr, len)
        else
            return String(tmp)
        end
    end
end

"""
    FITSCards.Parser.scan_card(A, off=0) -> type, key, key_rng, val_rng, com_rng

parses a FITS header card `A` as it is written in a FITS file. `A` may be a
string or a vector of bytes. Optional argument `off` is an offset in bytes
where to start the parsing. At most, $FITS_CARD_SIZE bytes after `off` are
considered in `A` which may thus belong to a larger piece of data (e.g., a FITS
header). The result is a 5-tuple:

- `type::FITSCardType` is the type of the card value.

- `key::FITSKey` is the code corresponding to the short keyword of the card.

- `key_rng` is the range of bytes containing the keyword name without trailing
  spaces. For `HIERARCH` keywords, the leading `"HIERARCH "` part has been
  removed from `key_rng` and `key` is equal to the constant `FITS"HIERARCH"`.

- `val_rng` is the range of bytes containing the unparsed value part, without
  leading and trailing spaces but with parenthesis or quote delimiters for a
  complex or a string card. This range is empty for a commentary card, the
  `END` card, or if the card has an undefined value.

- `com_rng` is the range of bytes containing the comment part without
  non-significant spaces.

"""
function scan_card(buf::ByteBuffer, off::Int = 0)
    off ≥ 0 || throw(ArgumentError("offset must be nonnegative"))
    i_first = off + first_byte_index(buf)
    i_last = min(last_byte_index(buf), i_first - 1 + FITS_CARD_SIZE)
    @inbounds begin # NOTE: above settings warrant that
        key, key_rng, i_next = scan_keyword(buf, i_first:i_last)
        if !is_comment(key)
            # May be a non-commentary FITS card.
            if key == FITS"END"
                # Remaining part shall contains only spaces
                com_rng = trim_leading_spaces(buf, i_next:i_last)
                isempty(com_rng) || bad_character_in_end_card(get_byte(buf, first(com_rng)))
                val_rng = empty_range(i_last)
                return FITS_END, key, key_rng, val_rng, com_rng
            elseif i_first ≤ i_next ≤ i_last - 1 &&
                is_equals_sign(get_byte(buf, i_next)) &&
                is_space(get_byte(buf, i_next + 1))
                # Value marker found, scan for the value and comment parts.
                type, val_rng, com_rng = scan_value_and_comment(buf, i_next+2:i_last)
                return type, key, key_rng, val_rng, com_rng
            end
        end
        # Commentary card: no value and a comment in bytes 9-80.
        val_rng = empty_range(i_next)
        com_rng = trim_trailing_spaces(buf, i_next:i_last)
        return FITS_COMMENT, key, key_rng, val_rng, com_rng
    end
end

"""
    FITSCards.Parser.scan_short_keyword(A, rng) -> key_rng

scans the first bytes of `A` in the index range `rng` for a valid short FITS
keyword and returns the index range to this keyword. A short FITS keyword
consists in, at most, $FITS_SHORT_KEYWORD_SIZE ASCII characters from the
restricted set of upper case letters (bytes 0x41 to 0x5A), decimal digits
(hexadecimal codes 0x30 to 0x39), hyphen (hexadecimal code 0x2D), or underscore
(hexadecimal code 0x5F). Trailing spaces are ignored.

The following relations hold:

    first(key_rng) == first(rng)
    length(key_rng) ≤ min(length(rng), $FITS_SHORT_KEYWORD_SIZE)

In case scanning shall be pursued, the next token to scan starts at or after
index:

    i_next = first(key_rng) + $FITS_SHORT_KEYWORD_SIZE

"""
@inline function scan_short_keyword(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        i_first = first(rng)
        i_last = min(i_first + (FITS_SHORT_KEYWORD_SIZE - 1), last(rng))
        nspaces = 0
        for i in i_first:i_last
            b = get_byte(buf, i)
            if is_space(b)
                nspaces += 1
            elseif is_keyword(b)
                iszero(nspaces) || bad_character_in_keyword(' ')
            else
                bad_character_in_keyword(b)
            end
        end
        return i_first : i_last - nspaces
    end
end

"""
    FITSCards.Parser.scan_keyword(A, rng) -> key, key_rng, i_next

parses a the keyword part of FITS header card stored in bytes `rng` of `A`.
Returns `key` the keyword code, `key_rng` the byte index range for the keyword
name (with leading "HIERARCH "` and trailing spaces removed), and `i_next` the
index of the first byte where next token (value marker of comment) may start.

"""
function scan_keyword(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    # Scan for the short FITS keyword part.
    key_rng = scan_short_keyword(buf, rng)

    # Retrieve limits for byte indices and index of next token assuming a short
    # FITS keyword for now.
    i_first, i_last = first(rng), last(rng)
    i_next = i_first  + FITS_SHORT_KEYWORD_SIZE

    # NOTE: scan_short_keyword() has already checked the range for us.
    @inbounds begin
        # Compute fast code equivalent to the short FITS keyword.
        off = i_first - 1
        len = length(rng)
        key = len ≥ FITS_SHORT_KEYWORD_SIZE ? FITSKey(Val(:full), buf, off) :
            FITSKey(Val(:pad), buf, off, len)

        if key == FITS"HIERARCH" && i_first ≤ i_next ≤ i_last - 2 && is_space(get_byte(buf, i_next))
            # Parse HIERARCH keyword. Errors are deferred until the value
            # marker "= " is eventually found.
            i_error = i_first - 1 # index of first bad character
            nspaces = 1 # to count the number of consecutive spaces so far
            key_first = i_next + 1 # byte index where long FITS keyword may start
            key_last = i_next      # byte index where long FITS keyword may end
            for i in key_first:i_last
                b = get_byte(buf, i)
                if is_space(b)
                    nspaces += 1
                elseif is_keyword(b)
                    # Update last index of long keyword to that of the last non-space.
                    key_last = i
                    if (nspaces > 1) & (i_error < i_first)
                        # Having more than one consecutive space is forbidden.
                        i_error = i
                    end
                    nspaces = 0
                elseif is_equals_sign(b)
                    if key_last ≥ key_first && i < i_last && is_space(get_byte(buf, i+1))
                        # Value marker found. The index for the next token is
                        # that of the = sign.
                        if i_error ≥ i_first
                            # Some illegal character was found.
                            bad_character_in_long_keyword(get_byte(buf, i_error))
                        end
                        return key, key_first:key_last, i
                    else
                        # This is not a long keyword, it will result in a
                        # commentary HIERARCH card.
                        break
                    end
                elseif i_error < i_first
                    i_error = i
                end
            end
        end
    end

    # Short FITS keyword.
    return key, key_rng, i_next
end

"""
    FITSCards.Parser.scan_value_and_comment(buf, rng) -> type, val_rng, com_rng

scans the range `rng` of bytes to find the value and comment of a FITS card
stored in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the
first byte where the value may be found, that is right after the value
indicator `"= "`, and `last(rng)` shall be the index of the last byte to scan.
For speed, these are not checked. The result is a tuple with `type` the type of
the FITS card value, `val_rng` the index range for the unparsed value part, and
`com_rng` the index range of the comment part without leading spaces.

"""
function scan_value_and_comment(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    # Skip leading spaces.
    i, k = first(rng), last(rng)
    @inbounds while i ≤ k && is_space(get_byte(buf, i))
        i += 1
    end
    if i > k
        # There were only spaces: the value is undefined and the comment is
        # empty.
        return FITS_UNDEFINED, i:k, i:k
    end
    # Guess card type based on first non-space byte.
    b = get_byte(buf, i)
    if equal(b, '+') | equal(b, '-') | equal(b, '.') | is_digit(b)
        # Integer or float value.
        type = equal(b, '.') ? FITS_FLOAT : FITS_INTEGER
        for j in i+1:k
            b = get_byte(buf, j)
            if is_digit(b)
                continue
            elseif is_space(b) | is_comment_separator(b)
                return type, i:j-1, scan_comment(buf, j:k)
            else
                type = FITS_FLOAT
            end
        end
        # Value with no comment.
        return type, i:k, k+1:k
    elseif is_quote(b)
        # Quoted string value. Find the closing quote. NOTE: The search loop
        # cannot be a for-loop because running index j has to be incremented
        # twice when an escaped quote is encountered.
        j = i
        while j < k
            j += 1
            if is_quote(get_byte(buf, j))
                j += 1
                if j > k || !is_quote(get_byte(buf, j))
                    # Closing quote found.
                    return FITS_STRING, i:j-1, scan_comment(buf, j:k)
                end
            end
        end
        error("no closing quote in string value of FITS header card")
    elseif equal(b, 'F') | equal(b, 'T')
        return FITS_LOGICAL, i:i, scan_comment(buf, i+1:k)
    elseif is_opening_parenthesis(b)
        # Complex value.
        for j in i+1:k
            if is_closing_parenthesis(get_byte(buf, j))
                return FITS_COMPLEX, i:j, scan_comment(buf, j+1:k)
            end
        end
        error("no closing parenthesis in complex value of FITS header card")
    elseif is_comment_separator(b)
        # Comment marker found before value, the value is undefined.
        return FITS_UNDEFINED, i:i-1, scan_comment(buf, i:k)
    else
        error("unexpected character in FITS header card")
    end
end

"""
    FITSCards.Parser.scan_comment(buf, rng) -> com_rng

scans the range `rng` of bytes to find the comment part of a FITS card stored
in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the first
byte where the comment separator may be found, that is right after the last
byte of the value part, and `last(rng)` shall be the index of the last byte to
scan. For speed, these are not checked. The result is the index range for the
comment part.

This method honors the bound-checking state.

"""
@inline function scan_comment(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        # Find beginning of comment skipping all spaces before and after the
        # comment separator.
        i_first, i_last = first(rng), last(rng)
        while i_first ≤ i_last
            b = get_byte(buf, i_first)
            i_first += 1
            if is_comment_separator(b)
                # Skip spaces after the comment separator.
                while i_first ≤ i_last && is_space(get_byte(buf, i_first))
                    i_first += 1
                end
                # Skip trailing spaces.
                while i_last ≥ i_first && is_space(get_byte(buf, i_last))
                    i_last -= 1
                end
                return i_first:i_last
            elseif !is_space(b)
                error("non-space before comment separator in FITS header card")
            end
        end
        return empty_space(i_first)
    end
end

# Remove trailing spaces.
@inline function trim_trailing_spaces(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        i_first, i_last = first(rng), last(rng)
        while i_last ≥ i_first && is_space(get_byte(buf, i_last))
            i_last -= 1
        end
        return i_first:i_last
    end
end

# Remove leading spaces.
@inline function trim_leading_spaces(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    @boundscheck check_byte_index(buf, rng)
    @inbounds begin
        i_first, i_last = first(rng), last(rng)
        while i_first ≤ i_last && is_space(get_byte(buf, i_first))
            i_first += 1
        end
        return i_first:i_last
    end
end

@noinline bad_character_in_end_card(c::Union{Char,UInt8}) =
    error("invalid non-space character $c in FITS END card")

@noinline bad_character_in_long_keyword(c::Union{Char,UInt8}) =
    is_space(c) ?  error("only single space separators are allowed in HIERARCH keywords") :
    error("invalid character $c in FITS HIERARCH keyword")

@noinline bad_character_in_keyword(c::Union{Char,UInt8}) =
    is_space(c) ? error("invalid space character in FITS keyword") :
    error("invalid character $c in FITS keyword")

end # module
