"""
    FITSCards.Parser

A sub-module of the `FITSCards` package implementing methods for parsing FITS
header cards.

"""
module Parser

using ..FITSCards
using ..FITSCards:
    FITSInteger,
    FITSFloat,
    FITSComplex

using Compat
using Base: @propagate_inbounds

const EMPTY_STRING = ""

@inline is_little_endian() = (ENDIAN_BOM === 0x04030201)
@inline is_big_endian()    = (ENDIAN_BOM === 0x01020304)
is_little_endian() || is_big_endian() || error("unsupported byte order")

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

"""
    FITSCards.Parser.get_workspace(len) -> wrk

yields a per-thread small workspace buffer with at least `len` bytes.

""" get_workspace
const WORKSPACES = Vector{UInt8}[]
function get_workspace(len::Int)
    i = Threads.threadid()
    while length(WORKSPACES) < i
        push!(WORKSPACES, Vector{UInt8}(undef, FITS_CARD_SIZE+1))
    end
    wrk = @inbounds WORKSPACES[i]
    length(wrk) < len && resize!(wrk, len)
    return wrk
end

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

"""
    FITSKey(buf, off=0, i_last=last_byte_index(buf))

encodes the, at most, first $FITS_SHORT_KEYWORD_SIZE bytes (or ASCII
characters) of `buf`, starting at offset `off`, in a 64-bit integer value which
is exactly equal to the first $FITS_SHORT_KEYWORD_SIZE bytes of a FITS keyword
as stored in a FITS header. Argument `buf` may be a string or a vector of
bytes.

Optional argument `i_last` is the index of the last byte available in `buf`. If
fewer than $FITS_SHORT_KEYWORD_SIZE bytes are available (that is, if `off +
$FITS_SHORT_KEYWORD_SIZE > i_last`), the result is as if `buf` has been padded
with ASCII spaces (hexadecimal code 0x20).

The only operation that makes sense with an instance of `FITSKey` is comparison
for equality for fast searching of keywords in a FITS header.

The caller may use `@inbounds` macro if it is certain that bytes in the range
`off+1:i_last` are in bounds for `buf`.

For the fastest, but unsafe, computations call:

    FITSKey(Val(:full), buf, off)
    FITSKey(Val(:pad), buf, off, i_last)

where first argument should be `Val(:full)` if there are at least
$FITS_SHORT_KEYWORD_SIZE bytes available after `off`, and `Val(:pad)`
otherwise. These variants do not perfom bounds checking, it is the caller's
responsibility to insure that the arguments are consistent.

"""
struct FITSKey
    val::UInt64
end

@assert sizeof(FITSKey) == FITS_SHORT_KEYWORD_SIZE

"""
    FITSKey()
    zero(FITSKey)

yield a null FITS key, that is whose bytes are all 0. This can be asserted by
calling `issero` on the returned key. Since any valid FITS key cannot contain
null bytes, a null FITS key may be useful for searching keys.

"""
FITSKey() = FITSKey(zero(UInt64))
# NOTE: Other constructors are implemented in parser.jl

Base.iszero(key::FITSKey) = iszero(key.val)
Base.zero(::Union{FITSKey,Type{FITSKey}}) = FITSKey()
Base.:(==)(a::FITSKey, b::FITSKey) = a.val === b.val
Base.convert(::Type{T}, key::FITSKey) where {T<:Integer} = convert(T, key.val)
Base.UInt64(key::FITSKey) = key.val

function Base.String(key::FITSKey)
    buf = Vector{UInt8}(undef, FITS_SHORT_KEYWORD_SIZE)
    len = @inbounds decode!(buf, key; offset = 0)
    ptr = pointer(buf)
    return GC.@preserve buf unsafe_string(ptr, len)
end

Base.show(io::IO, mime::MIME"text/plain", key::FITSKey) = show(io, key)
function Base.show(io::IO, key::FITSKey)
    buf = Vector{UInt8}(undef, FITS_SHORT_KEYWORD_SIZE + 6)
    buf[1] = 'F'
    buf[2] = 'I'
    buf[3] = 'T'
    buf[4] = 'S'
    buf[5] = '"'
    len = @inbounds decode!(buf, key; offset = 5) + 1
    buf[len] = '"'
    if len < length(buf)
        write(io, view(buf, Base.OneTo(len)))
    else
        write(io, buf)
    end
end

@inline function decode!(buf::AbstractVector{UInt8},
                         key::FITSKey;
                         offset::Int = 0)
    i_first = (offset + firstindex(buf))::Int
    i_last = (i_first + (FITS_SHORT_KEYWORD_SIZE - 1))::Int
    I = i_first:i_last
    @boundscheck ((offset ≥ 0) & (i_last ≤ lastindex(buf))) || throw(BoundsError(buf, I))
    val = key.val
    shft = is_little_endian() ? 0 : 8*(FITS_SHORT_KEYWORD_SIZE-1)
    incr = is_little_endian() ? +8 : -8
    i_last = offset
    @inbounds for i in I
        byte = (val >> shft) % UInt8
        shft += incr
        if byte != 0x20
            i_last = i
        end
        buf[i] = byte
    end
    return i_last
end

"""
    @FITS_str

A macro to construct a 64-bit quick key equivalent to the FITS keyword given in
argument and as it is stored in the header of a FITS file. The argument must be
a short FITS keyword (e.g., not a `HIERARCH` one) specified as a literal string
of, at most, $FITS_SHORT_KEYWORD_SIZE ASCII characters with no trailing spaces.
For example `FITS"SIMPLE"` or `FITS"NAXIS2"`.

The result is the same as that computed by `FITSKey` but since the quick key is
given by a string macro, it is like a constant computed at compile time with no
runtime penalty.

"""
macro FITS_str(str::String)
    FITSKey(check_short_keyword(str))
end

"""
    FITSCards.check_short_keyword(str) -> str

returns the string `str` throwing an exception if `str` is not a short FITS
keyword consisting in, at most, $FITS_SHORT_KEYWORD_SIZE ASCII characters from
the restricted set of upper case letters (bytes 0x41 to 0x5A), decimal digits
(hexadecimal codes 0x30 to 0x39), hyphen (hexadecimal code 0x2D), or underscore
(hexadecimal code 0x5F).

"""
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
    i_last = last_byte_index(buf)
    @boundscheck check_byte_index(buf, off+1, min(off+FITS_SHORT_KEYWORD_SIZE, i_last), i_last)
    off + FITS_SHORT_KEYWORD_SIZE ≤ i_last ? FITSKey(Val(:full), buf, off) :
        FITSKey(Val(:pad), buf, off, i_last)
end

@inline function FITSKey(buf::ByteBuffer, off::Int, i_last::Int)
    @boundscheck check_byte_index(buf, off+1, min(off+FITS_SHORT_KEYWORD_SIZE, i_last))
    off + FITS_SHORT_KEYWORD_SIZE ≤ i_last ? FITSKey(Val(:full), buf, off) :
        FITSKey(Val(:pad), buf, off, i_last)
end

@inline FITSKey(val::Val{:full}, buf::ByteBuffer, off::Int) =
    FITSKey(PointerCapability(buf), val, buf, off)

@inline FITSKey(::PointerFull, ::Val{:full}, buf::Vector{UInt8}, off::Int) =
    GC.@preserve buf unsafe_load(Base.unsafe_convert(Ptr{FITSKey}, buf) + off)

@inline function FITSKey(::PointerCapability, ::Val{:full}, buf::ByteBuffer, off::Int)
    @inbounds begin
        @static if is_little_endian()
            # Little-endian byte order.
            k = (get_byte(UInt64, buf, off + 1) <<  0) |
                (get_byte(UInt64, buf, off + 2) <<  8) |
                (get_byte(UInt64, buf, off + 3) << 16) |
                (get_byte(UInt64, buf, off + 4) << 24) |
                (get_byte(UInt64, buf, off + 5) << 32) |
                (get_byte(UInt64, buf, off + 6) << 40) |
                (get_byte(UInt64, buf, off + 7) << 48) |
                (get_byte(UInt64, buf, off + 8) << 56)
        else
            # Big-endian byte order.
            k = (get_byte(UInt64, buf, off + 1) << 56) |
                (get_byte(UInt64, buf, off + 2) << 48) |
                (get_byte(UInt64, buf, off + 3) << 40) |
                (get_byte(UInt64, buf, off + 4) << 32) |
                (get_byte(UInt64, buf, off + 5) << 24) |
                (get_byte(UInt64, buf, off + 6) << 16) |
                (get_byte(UInt64, buf, off + 7) <<  8) |
                (get_byte(UInt64, buf, off + 8) <<  0)
        end
    end
    return FITSKey(k)
end

@inline function FITSKey(::Val{:pad}, buf::ByteBuffer, off::Int, i_last::Int)
    @inbounds begin
        @static if is_little_endian()
            # Little-endian byte order.
            k = (get_byte(UInt64, buf, off + 1, i_last) <<  0) |
                (get_byte(UInt64, buf, off + 2, i_last) <<  8) |
                (get_byte(UInt64, buf, off + 3, i_last) << 16) |
                (get_byte(UInt64, buf, off + 4, i_last) << 24) |
                (get_byte(UInt64, buf, off + 5, i_last) << 32) |
                (get_byte(UInt64, buf, off + 6, i_last) << 40) |
                (get_byte(UInt64, buf, off + 7, i_last) << 48) |
                (get_byte(UInt64, buf, off + 8, i_last) << 56)
        else
            # Big-endian byte order.
            k = (get_byte(UInt64, buf, off + 1, i_last) << 56) |
                (get_byte(UInt64, buf, off + 2, i_last) << 48) |
                (get_byte(UInt64, buf, off + 3, i_last) << 40) |
                (get_byte(UInt64, buf, off + 4, i_last) << 32) |
                (get_byte(UInt64, buf, off + 5, i_last) << 24) |
                (get_byte(UInt64, buf, off + 6, i_last) << 16) |
                (get_byte(UInt64, buf, off + 7, i_last) <<  8) |
                (get_byte(UInt64, buf, off + 8, i_last) <<  0)
        end
    end
    return FITSKey(k)
end

"""
    FITSCards.is_comment(A::Union{FITSCardType,FITSCard})

yields whether `A` indicates a, possibly non-standard, commentary FITS keyword.

    FITSCards.is_comment(key::FITSKey)

yields whether `key` is `FITS"COMMENT"` or `FITS"HISTORY"` which corresponds to
a standard commentary FITS keyword.

"""
is_comment(key::FITSKey) = (key == FITS"COMMENT") | (key == FITS"HISTORY")
is_comment(type::FITSCardType) = type === FITS_COMMENT

"""
    FITSCards.is_end(A::Union{FITSKey,FITSCardType,FITSCard})

yields whether `A` indicates the END FITS keyword.

"""
is_end(key::FITSKey) = (key == FITS"END")
is_end(type::FITSCardType) = type === FITS_END

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
    wrk = get_workspace(len)
    off = first(rng) - firstindex(wrk)
    @inbounds for i in eachindex(wrk)
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
        len > 0 || return EMPTY_STRING
        # Convert temporary buffer into a string. Cannot use String(wrk)
        # because string length may be smaller tahn taht of the buffer.
        obj = Base.cconvert(Ptr{UInt8}, wrk) # object to be preserved
        ptr = Base.unsafe_convert(Ptr{UInt8}, obj)
        return GC.@preserve obj unsafe_string(ptr, len)
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

"""
    FITSCards.Parser.make_string(buf, rng) -> str::String

yields a string from the bytes of `buf` in the range of indices `rng`.

"""
@inline function make_string(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    len = length(rng)
    iszero(len) && return EMPTY_STRING
    @boundscheck check_byte_index(buf, rng)
    return unsafe_make_string(PointerCapability(buf), buf, rng)
end

function unsafe_make_string(::PointerFull, buf::ByteBuffer,
                            rng::AbstractUnitRange{Int})
    # Directly build a string from the buffer.
    len = length(rng)
    off = first(rng) - 1
    obj = Base.cconvert(Ptr{UInt8}, buf) # object to be preserved
    ptr = Base.unsafe_convert(Ptr{UInt8}, obj) + off
    return GC.@preserve obj unsafe_string(ptr, len)
end

function unsafe_make_string(::PointerCapability, buf::ByteBuffer,
                            rng::AbstractUnitRange{Int})
    # Use a temporary workspace to copy the range of bytes.
    len = length(rng)
    wrk = Array{UInt8}(undef, len)
    off = first(rng) - firstindex(wrk)
    @inbounds for i in eachindex(wrk)
        wrk[i] = get_byte(buf, off + i)
    end
    # FIXME: other possibility: String(wrk)
    obj = Base.cconvert(Ptr{UInt8}, wrk) # object to be preserved
    ptr = Base.unsafe_convert(Ptr{UInt8}, obj)
    return GC.@preserve obj unsafe_string(ptr, len)
end

"""
    FITSCards.Parser.scan_card(A, off=0) -> type, key, name_rng, val_rng, com_rng

parses a FITS header card `A` as it is written in a FITS file. `A` may be a
string or a vector of bytes. Optional argument `off` is an offset in bytes
where to start the parsing. At most, $FITS_CARD_SIZE bytes after `off` are
considered in `A` which may thus belong to a larger piece of data (e.g., a FITS
header). The result is a 5-tuple:

- `type::FITSCardType` is the type of the card value.

- `key::FITSKey` is the quick key corresponding to the short keyword of the
  card.

- `name_rng` is the range of bytes containing the keyword name without trailing
  spaces.

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
    if i_first > i_last
        # Empty range, return the parameters of an END card to reflect that
        # except that the name range is empty.
        rng = empty_range(i_first)
        return FITS_END, FITS"END", rng, rng, rng
    end
    @inbounds begin # NOTE: above settings warrant that
        key, name_rng, i_next = scan_keyword_part(buf, i_first:i_last)
        if !is_comment(key)
            # May be a non-commentary FITS card.
            if key == FITS"END"
                # Remaining part shall contains only spaces
                com_rng = trim_leading_spaces(buf, i_next:i_last)
                isempty(com_rng) || nonspace_in_end_card(get_byte(buf, first(com_rng)))
                val_rng = empty_range(i_last)
                return FITS_END, key, name_rng, val_rng, com_rng
            elseif i_first ≤ i_next ≤ i_last - 1 &&
                is_equals_sign(get_byte(buf, i_next)) &&
                is_space(get_byte(buf, i_next + 1))
                # Value marker found, scan for the value and comment parts.
                type, val_rng, com_rng = scan_value_comment_parts(buf, i_next+2:i_last)
                return type, key, name_rng, val_rng, com_rng
            end
        end
        # Commentary card: no value and a comment in bytes 9-80.
        val_rng = empty_range(i_next)
        com_rng = trim_trailing_spaces(buf, i_next:i_last)
        return FITS_COMMENT, key, name_rng, val_rng, com_rng
    end
end

"""
    FITSCards.Parser.scan_short_keyword_part(A, rng) -> name_rng

scans the first bytes of `A` in the index range `rng` for a valid short FITS
keyword and returns the index range to this keyword. A short FITS keyword
consists in, at most, $FITS_SHORT_KEYWORD_SIZE ASCII characters from the
restricted set of upper case letters (bytes 0x41 to 0x5A), decimal digits
(hexadecimal codes 0x30 to 0x39), hyphen (hexadecimal code 0x2D), or underscore
(hexadecimal code 0x5F). Trailing spaces are ignored.

The following relations hold:

    first(name_rng) == first(rng)
    length(name_rng) ≤ min(length(rng), $FITS_SHORT_KEYWORD_SIZE)

In case scanning shall be pursued, the next token to scan starts at or after
index:

    i_next = first(name_rng) + $FITS_SHORT_KEYWORD_SIZE

"""
@inline function scan_short_keyword_part(buf::ByteBuffer, rng::AbstractUnitRange{Int})
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
    FITSCards.Parser.scan_keyword_part(A, rng) -> key, name_rng, i_next

parses a the keyword part of FITS header card stored in bytes `rng` of `A`.
Returns `key` the keyword quick key, `name_rng` the byte index range for the
keyword name (with leading "HIERARCH "` and trailing spaces removed), and
`i_next` the index of the first byte where next token (value marker of comment)
may start.

"""
function scan_keyword_part(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    # Scan for the short FITS keyword part.
    name_rng = scan_short_keyword_part(buf, rng)

    # Retrieve limits for byte indices and index of next token assuming a short
    # FITS keyword for now.
    i_first, i_last = first(rng), last(rng)
    i_next = i_first + FITS_SHORT_KEYWORD_SIZE

    # NOTE: scan_short_keyword_part() has already checked the range for us.
    @inbounds begin
        # Compute fast code equivalent to the short FITS keyword.
        off = i_first - 1
        key = off + FITS_SHORT_KEYWORD_SIZE ≤ i_last ? FITSKey(Val(:full), buf, off) :
            FITSKey(Val(:pad), buf, off, i_last)

        if key == FITS"HIERARCH" && i_first ≤ i_next ≤ i_last - 2 && is_space(get_byte(buf, i_next))
            # Parse HIERARCH keyword. Errors are deferred until the value
            # marker "= " is eventually found.
            i_error = i_first - 1 # index of first bad character
            nspaces = 1           # to count consecutive spaces
            i_mark = i_first - 1  # index where long FITS keyword may end
            for i in i_next+1:i_last
                b = get_byte(buf, i)
                if is_space(b)
                    nspaces += 1
                elseif is_keyword(b)
                    # Update last index of long keyword to that of the last non-space.
                    i_mark = i
                    if (nspaces > 1) & (i_error < i_first)
                        # Having more than one consecutive space is forbidden.
                        i_error = i
                    end
                    nspaces = 0
                elseif is_equals_sign(b)
                    if i_mark ≥ i_first && i < i_last && is_space(get_byte(buf, i+1))
                        # Value marker found. The index for the next token is
                        # that of the = sign.
                        if i_error ≥ i_first
                            # Some illegal character was found.
                            bad_character_in_keyword(get_byte(buf, i_error))
                        end
                        return key, i_first:i_mark, i
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
    return key, name_rng, i_next
end

"""
    FITSCards.keyword(name) -> full_name

yields the full FITS keyword corresponding to `name`, throwing an exception if
`name` is not a valid FITS keyword.  The result is equal to either `name` or
to `"HIERARCH "*name`.

Examples:

``` jldoctest
julia> FITSCards.keyword("GIZMO")
"GIZMO"

julia> FITSCards.keyword("HIERARCH GIZMO")
"HIERARCH GIZMO"

julia> FITSCards.keyword("GIZ MO")
"HIERARCH GIZ MO"

julia> FITSCards.keyword("VERYLONGNAME")
"HIERARCH VERYLONGNAME"
```

where the 1st one is a short FITS keyword (with less than
$FITS_SHORT_KEYWORD_SIZE characters), the 3rd one is explictely a `HIERARCH`
keyword, while the 3rd and 4th ones are automatically turned into `HIERARCH`
keywords because the 3rd one contains a space and because the 4th one is longer
than $FITS_SHORT_KEYWORD_SIZE characters.

See also [`FITSCards.check_keyword`](@ref).

"""
keyword(name::AbstractString) = check_keyword(name)[2]

"""
    FITSCards.check_keyword(name) -> key, full_name

checks the FITS keyword `name` and returns the corresponding quick key and full
keyword name throwing an exception if `name` is not a valid FITS keyword. The
full keyword name is a `string` instance either equal to `name` or to `"HIERARCH "*name`.

See also [`FITSCards.keyword`](@ref), [`FITSCards.parse_keyword`](@ref).

"""
function check_keyword(name::AbstractString)
    key, pfx = parse_keyword(name)
    return key, pfx ? "HIERARCH "*name : String(name)
end

"""
    FITSCards.Parser.parse_keyword(A, rng=byte_index_range(A)) -> key, pfx

parses the FITS keyword given by `A`, a string or a vector of bytes, throwing
an exception if the name is invalid. The result is a 2-tuple: `key` is the
quick keyword key and `pfx` is a boolean indicating whether the `"HIERARCH "`
prefix should be prepended to `A` to form the full keyword name. If the range
of bytes is longer than $FITS_SHORT_KEYWORD_SIZE or if any single space
separator occurs in the range of bytes, a `HIERARCH` keyword is assumed even
though the first bytes are not `"HIERARCH "`. Leading and trailing spaces are
not allowed.

The returned `key` is `FITS"HIERARCH"` in 4 cases:

- The first bytes of the sequence are `"HIERARCH "` followed by a name,
  possibly slit in several parts and possibly longer than
  $FITS_SHORT_KEYWORD_SIZE, `pfx` is `false`.

- The sequence does not starts by `"HIERARCH "` but at least one single space
  separator occurs in the sequence, `pfx` is true.

- The sequence does not starts by `"HIERARCH "` but the sequence is longer than
  $FITS_SHORT_KEYWORD_SIZE, `pfx` is true.

- The byte sequence is `"HIERARCH"`, `pfx` is `false`.

""" parse_keyword

# FIXME: This function should only be used on strings.
@inline function unsafe_parse_keyword(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    @inbounds begin
        # Compute quick key of the short FITS keyword, this is a cheap way to
        # figure out whether the sequence of bytes starts with "HIERARCH". This
        # does not check for the validity of the leading bytes, unless the key
        # is FITS"HIERARCH".
        i_first, i_last = first(rng), last(rng)
        any_space = false # any space found so far?
        pfx = false # add "HIERARCH " prefix?
        off = i_first - 1
        len = length(rng)
        key = len ≥ FITS_SHORT_KEYWORD_SIZE ? FITSKey(Val(:full), buf, off) :
            FITSKey(Val(:pad), buf, off, len)
        if key == FITS"HIERARCH"
            # Byte sequence starts with "HIERARCH". There are 3 possibilities:
            #
            # 1. The keyword is exatly "HIERARCH".
            #
            # 2. The keyword starts with "HIERARCH " and is thus a regular
            #    HIERARCH keyword.
            #
            # 2. The keyword starts with "HIERARCHx" where x is any valid
            #    non-space character. The keyword is thus too long to be a
            #    simple FITS keyword and the HIERARCH convention must be used.
            #    The prefix "HIERARCH " must be prepended for that.
            #
            # Which of these apply requires to look at next character. In any
            # case, the leading FITS_SHORT_KEYWORD_SIZE bytes are valid, so we
            # increment i_first to not re-check this part.
            i_first += FITS_SHORT_KEYWORD_SIZE
            # If at least one more byte is available, we are in cases 2 or 3; in
            # case 1 otherwise.
            if i_first ≤ i_last
                b = get_byte(buf, i_first)
                i_first += 1
                if is_space(b)
                    # Case 2: the sequence starts with "HIERARCH ".
                    any_space = true
                elseif is_keyword(b)
                    # Case 3: the keyword too long and a "HIERARCH " prefix must
                    # be prepended.
                    pfx = true
                else
                    bad_character_in_keyword(b)
                end
            end
        elseif len ≥ 1
            # We must verify that the first byte is valid (not a space).
            b = get_byte(buf, i_first)
            i_first += 1
            is_keyword(b) || bad_character_in_keyword(b)
            # A long keyword implies using the HIERARCH convention.
            if len > FITS_SHORT_KEYWORD_SIZE
                key = FITS"HIERARCH"
                pfx = true
            end
        end
        #println("i_first = $i_first, pfx = $pfx, key = $key")
        # Check remaining bytes.
        for i in i_first:i_last
            b = get_byte(buf, i)
            if is_space(b)
                # It is an error to have 2 or more consecutive spaces.
                any_space && bad_character_in_keyword(b)
                any_space = true
                # Keyword must be a HIERARCH one because it has at least one
                # space separator. If this was not already detected, the
                # "HIERARCH " is missing.
                if key != FITS"HIERARCH"
                    key = FITS"HIERARCH"
                    pfx = true
                end
            elseif is_keyword(b)
                # Not a space.
                any_space = false
            else
                bad_character_in_keyword(b)
            end
        end
        any_space && bad_character_in_keyword(' ')
        return key, pfx
    end
end

"""
    FITSCards.Parser.scan_value_comment_parts(buf, rng) -> type, val_rng, com_rng

scans the range `rng` of bytes to find the value and comment of a FITS card
stored in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the
first byte where the value may be found, that is right after the value
indicator `"= "`, and `last(rng)` shall be the index of the last byte to scan.
For speed, these are not checked. The result is a tuple with `type` the type of
the FITS card value, `val_rng` the index range for the unparsed value part, and
`com_rng` the index range of the comment part without leading spaces.

"""
function scan_value_comment_parts(buf::ByteBuffer, rng::AbstractUnitRange{Int})
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
                return type, i:j-1, scan_comment_part(buf, j:k)
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
                    return FITS_STRING, i:j-1, scan_comment_part(buf, j:k)
                end
            end
        end
        error("no closing quote in string value of FITS header card")
    elseif equal(b, 'F') | equal(b, 'T')
        return FITS_LOGICAL, i:i, scan_comment_part(buf, i+1:k)
    elseif is_opening_parenthesis(b)
        # Complex value.
        for j in i+1:k
            if is_closing_parenthesis(get_byte(buf, j))
                return FITS_COMPLEX, i:j, scan_comment_part(buf, j+1:k)
            end
        end
        error("no closing parenthesis in complex value of FITS header card")
    elseif is_comment_separator(b)
        # Comment marker found before value, the value is undefined.
        return FITS_UNDEFINED, i:i-1, scan_comment_part(buf, i:k)
    else
        error("unexpected character in FITS header card")
    end
end

"""
    FITSCards.Parser.scan_comment_part(buf, rng) -> com_rng

scans the range `rng` of bytes to find the comment part of a FITS card stored
in `buf`. If `rng` is not empty, `first(rng)` shall be the index of the first
byte where the comment separator may be found, that is right after the last
byte of the value part, and `last(rng)` shall be the index of the last byte to
scan. For speed, these are not checked. The result is the index range for the
comment part.

This method honors the bound-checking state.

"""
@inline function scan_comment_part(buf::ByteBuffer, rng::AbstractUnitRange{Int})
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
        return empty_range(i_first)
    end
end

# Units in comments.
function scan_units_marks(str::Union{String,SubString{String}})
    @inbounds begin
        # It is assumed that leading spaces have been trimmed.
        i_first, i_last = first_byte_index(str), last_byte_index(str)
        if i_first ≤ i_last && get_byte(str, i_first) == UInt8('[')
            i = i_first
            while i < i_last
                i += 1
                if get_byte(str, i) == UInt8(']')
                    return i_first:i
                end
            end
        end
        return empty_range(i_first) # yields i_first:i_first-1
    end
end

# Yields the units part of a parsed comment.
function get_units_part(str::Union{String,SubString{String}})
    @inbounds begin
        rng = scan_units_marks(str)
        i_first, i_last = first(rng), last(rng)
        if i_first < i_last # is there at least 2 bytes (ASCII characters)?
            i_first += 1 # skip opening [
            i_last  -= 1 # skip closing ]
            # Trim leading spaces.
            while i_first ≤ i_last && is_space(get_byte(str, i_first))
                i_first += 1
            end
            # Trim trailing spaces.
            while i_first ≤ i_last && is_space(get_byte(str, i_last))
                i_last -= 1
            end
            if i_first ≤ i_last
                return SubString(str, i_first, i_last)
            end
        end
        return SubString(str, empty_range(first(rng)))
    end
end

# Yields the unitless part of a parsed comment.
function get_unitless_part(str::Union{String,SubString{String}})
    @inbounds begin
        rng = scan_units_marks(str)
        i_first, i_last = first(rng), last(rng)
        if i_first > i_last
            # No units.
            return SubString(str, first_byte_index(str), last_byte_index(str))
        else
            # There are units.
            #
            # NOTE: In fact, scan_units_marks() warrants that last(rng)+1 is
            #       always the first byte of the unitless part whether there
            #       are units or not, so the code could be simplified.
            i_first = i_last + 1
            i_last = last_byte_index(str)
            # Trim leading spaces.
            while i_first ≤ i_last && is_space(get_byte(str, i_first))
                i_first += 1
            end
            if i_first > i_last
                # Empty unitless part.
                i_first = first_byte_index(str)
                i_last = i_first - 1
            end
            return SubString(str, i_first, i_last)
        end
    end
end

"""
    FITSCards.Parser.trim_leading_spaces(buf[, rng]) -> sub_rng

yields the range `sub_rng` of byte indices in `buf` (a string or a vector of
bytes) without the leading spaces in `buf`. Optional argument `rng` is to
specify the range of byte indices to consider in `buf`. If `rng` is not
provided, all the bytes of `buf` are considered. If `rng` is provided,
`sub_rng` is such that:

    first(sub_rng) ≥ first(rng)
    last(sub_rng) == last(rng)

""" trim_leading_spaces

@inline function unsafe_trim_leading_spaces(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    i_first, i_last = first(rng), last(rng)
    @inbounds while i_first ≤ i_last && is_space(get_byte(buf, i_first))
        i_first += 1
    end
    return i_first:i_last
end

"""
    FITSCards.Parser.trim_trailing_spaces(buf[, rng]) -> sub_rng

yields the range `sub_rng` of byte indices in `buf` (a string or a vector of
bytes) without the trailing spaces in `buf`. Optional argument `rng` is to
specify the range of byte indices to consider in `buf`. If `rng` is not
provided, all the bytes of `buf` are considered. If `rng` is provided,
`sub_rng` is such that:

    first(sub_rng) == first(rng)
    last(sub_rng) ≤ last(rng)

""" trim_trailing_spaces

@inline function unsafe_trim_trailing_spaces(buf::ByteBuffer, rng::AbstractUnitRange{Int})
    i_first, i_last = first(rng), last(rng)
    @inbounds while i_last ≥ i_first && is_space(get_byte(buf, i_last))
        i_last -= 1
    end
    return i_first:i_last
end

# Implement higher level "safe" methods.
for func in (:parse_keyword, :trim_leading_spaces, :trim_trailing_spaces)
    unsafe_func = Symbol("unsafe_$func")
    @eval begin
        $func(buf::ByteBuffer) = $unsafe_func(buf, byte_index_range(buf))
        function $func(buf::ByteBuffer, rng::AbstractUnitRange{Int})
            @boundscheck check_byte_index(buf, rng)
            return $unsafe_func(buf, rng)
        end
    end
end

# Human readable Representation of a character.
repr_char(b::UInt8) = is_restricted_ascii(b) ? repr(Char(b)) : repr(b)
repr_char(c::Char) = (is_restricted_ascii(c) || ! isascii(c)) ? repr(c) : repr(UInt8(c))

@noinline nonspace_in_end_card(c::Union{Char,UInt8}) =
    error("invalid non-space character $(repr_char(c)) in FITS END card")

@noinline bad_character_in_keyword(c::Union{Char,UInt8}) =
    is_space(c) ? error("extra space character in FITS keyword") :
    error("invalid character $(repr_char(c)) in FITS keyword")

end # module
