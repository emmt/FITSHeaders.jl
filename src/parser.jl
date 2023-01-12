"""
    FITSCards.Parser

A sub-module of the `FITSCards` package implementing methods for parsing FITS
header cards.

"""
module Parser

using ..FITSCards
import ..FITSCards: FITSKey, check_simple_key

using Base: @propagate_inbounds

const CARD_SIZE = 80 # FIXME: export as FITS_CARD_SIZE?

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
@inline byte_index_range(A::ByteString) = first_byte_index(A):last_byte_index(A)

function check_simple_key(str::ByteString)
    rng = byte_index_range(str)
    @inbounds for i in rng
        is_keyword(get_byte(str, i)) || error("invalid character in simple FITS keyword \"$str\"")
    end
    length(rng) ≤ 8 || error("too many characters in FITS keyword \"$str\"")
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
    @boundscheck checkbounds(FITSKey, buf, off+1, min(off+8, n), n)
    off + 8 ≤ n ? FITSKey(Val(:full), buf, off) : FITSKey(Val(:trunc), buf, off, n)
end

@inline function FITSKey(buf::ByteBuffer, off::Int, n::Int)
    @boundscheck checkbounds(FITSKey, buf, off+1, min(off+8, n))
    off + 8 ≤ n ? FITSKey(Val(:full), buf, off) : FITSKey(Val(:trunc), buf, off, n)
end

@inline function Base.checkbounds(::Type{FITSKey}, buf::ByteBuffer, i::Int, j::Int,
                                  n::Int = last_byte_index(buf))
    ((i > j) | (i ≥ first_byte_index(buf)) & (j ≤ n)) || throw(BoundsError(buf, i:j))
end

@inline FITSKey(::Val{:full}, buf::Vector{UInt8}, off::Int) =
    GC.@preserve buf unsafe_load(Base.unsafe_convert(Ptr{FITSKey}, buf) + off)

@inline function FITSKey(::Val{:full}, buf::ByteBuffer, off::Int)
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

@inline function FITSKey(::Val{:trunc}, buf::ByteBuffer, off::Int, n::Int)
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

"""
    FITSCards.Parser.find_value_and_comment(buf, rng) -> type, val_rng, com_rng

scans the range `rng` of bytes to find the value and comment of a FITS card
stored in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the
first byte where the value may be found, that is right after the value
indicator `"= "`, and `last(rng)` shall be the index of the last byte to scan.
For speed, these are not checked. The result is a tuple with `type` the type of
the FITS card value, `val_rng` the index range for the unparsed value part, and
`com_rng` the index range of the comment part without leading spaces.

"""
function find_value_and_comment(buf::ByteBuffer, rng::AbstractUnitRange{Int})
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
                return type, i:j-1, find_comment(buf, j:k)
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
                    return FITS_STRING, i:j-1, find_comment(buf, j:k)
                end
            end
        end
        error("no closing quote in string value of FITS header card")
    elseif equal(b, 'F') | equal(b, 'T')
        return FITS_LOGICAL, i:i, find_comment(buf, i+1:k)
    elseif is_opening_parenthesis(b)
        # Complex value.
        for j in i+1:k
            if is_closing_parenthesis(get_byte(buf, j))
                return FITS_COMPLEX, i:j, find_comment(buf, j+1:k)
            end
        end
        error("no closing parenthesis in complex value of FITS header card")
    elseif is_comment_separator(b)
        # Comment marker found before value, the value is undefined.
        return FITS_UNDEFINED, i:i-1, find_comment(buf, i:k)
    else
        error("unexpected character in FITS header card")
    end
end

"""
    FITSCards.Parser.find_comment(buf, rng) -> com_rng

scans the range `rng` of bytes to find the comment part of a FITS card stored
in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the first
byte where the comment separator may be found, that is right after the last
byte of the value part, and `last(rng)` shall be the index of the last byte to
scan. For speed, these are not checked. The result is the index range for the
comment part.

"""
function find_comment(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    # Find beginning of comment skipping all spaces before and after the
    # comment separator.
    i, k = first(rng), last(rng)
    @inbounds while i ≤ k
        b = get_byte(buf, i)
        i += 1
        if is_comment_separator(b)
            # Skip spaces after the comment separator.
            while i ≤ k && is_space(get_byte(buf, i))
                i += 1
            end
            break
        elseif !is_space(b)
            error("non-space before comment separator in FITS header card")
        end
    end
    return i:k
end

end # module
