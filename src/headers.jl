module Headers

export FITSHeader

using ..FITSCards
using ..FITSCards: check_keyword, is_comment

"""
    FITSHeader(args...) -> A

yields a FITS header object initialized with records `args..`. If the only
argument is an instance of `Vector{FITSCard}`, `A` directly uses it for its own
storage.

A FITS header object behaves as a vector of [`FITSCard`](@ref) elements with
integer or keyword (string) indices. When indexed by keywords, a FITS header
object is similar to a dictionary except that the order of records is preserved
and that commentary records (with keywords `"COMMENT"` or `"HISTORY"`) may
appears more than once.

To  append a new record `rec` to the FITS header `A`, call:

    push!(A, rec)

where `rec` may be an instance of [`FITSCard`](@ref) or a pair `key => (val,
com)` associating keyword `key` with a value `val` and/or a comment `com`.

If the intention is to update the record, call:

    A[key] = (val, com)

which modifies the record if the keyword `key` already exists in `A` and
appends the record to `A` otherwise. Note that COMMENT and HISTORY commentary
records are always appended (as if `push!` has been called). To modify any
existing record including commentary ones, use the syntax:

    A[i] = rec

where `i` is a linear (integer) index.

Searching for the index `i` of an existing record in FITS header object `A`
can be done by the usual methods:

    findfirst(what, A)
    findlast(what, A)
    findnext(what, A, start)
    findprev(what, A, start)

which all return a valid integer index if a record matching `what` is found and
`nothing` otherwise. The matching pattern `what` can be a keyword (string), a
FITS card (an instance of [`FITSCard`](@ref) whose name is used as a matching
pattern), or a predicate function which takes a FITS card argument and shall
return whether it matches. The find methods just yield `nothing` for any
unsupported kind of pattern.

"""
struct FITSHeader <: AbstractVector{FITSCard}
    cards::Vector{FITSCard}
end

function FITSHeader(args...)
    V = FITSCard[]
    n = length(args)
    n > 0 && sizehint!(V, n)
    A = FITSHeader(V)
    for rec ∈ args
        push!(A, rec)
    end
    return A
end

contents(A::FITSHeader) = getfield(A, :cards)

Base.convert(::Type{<:FITSHeader}, A::FITSHeader) = A
Base.convert(::Type{<:FITSHeader}, A::AbstractVector{<:FITSCard}) = FITSHeader(A)

# Implement abstract array API.
Base.IndexStyle(::Type{<:FITSHeader}) = IndexLinear()
for func in (:length, :size, :axes)
    @eval Base.$func(A::FITSHeader) = $func(contents(A))
end
Base.keys(A::FITSHeader) = Base.OneTo(length(A))
Base.firstindex(A::FITSHeader) = 1
Base.lastindex(A::FITSHeader) = length(A)
@inline function Base.getindex(A::FITSHeader, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds getindex(contents(A), i)
end
@inline function Base.setindex!(A::FITSHeader, x, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds setindex!(contents(A), x, i)
    return A
end

"""
    push!(A::FITSHeader, rec) -> A

appends a new record `rec` in into FITS header `A`. The record may be specified
as an instance of [`FITSCard`](@ref) or as a pair `key => (val, com)`
associating a keyword `key` with a value `val` and/or a comment `com`. Call:

    A[key] = (val, com)

if the intention is to update the record if the keyword `key` already exists
in `A` and appends the record to `A` otherwise. Note that COMMENT and HISTORY
commentary records are always appended.

"""
function Base.push!(A::FITSHeader, rec)
    push!(contents(A), convert(FITSCard, rec)::FITSCard)
    return A
end

function Base.getindex(A::FITSHeader, key::AbstractString)
    card = get(A, key, nothing)
    card === nothing ? throw(KeyError(key)) : card
end

function Base.setindex!(A::FITSHeader, x, key::AbstractString)
    card = FITSCard(key => x)
    if ! is_comment(card.key)
        # Replace card with same keyword if it exists.
        i = findfirst(card, A)
        if i !== nothing
            @inbounds contents(A)[i] = card
            return A
        end
    end
    # Push a new card.
    push!(contents(A), card)
    return A
end

function Base.get(A::FITSHeader, key::Integer, def)
    i = Int(key)
    if checkbounds(Bool, A, i)
        @inbounds A[i]
    else
        def
    end
end

function Base.get(A::FITSHeader, key::AbstractString, def)
    i = findfirst(key, A)
    i === nothing ? def : @inbounds A[i]
end

"""
    findfirst(what, A) -> i :: Union{Int,Nothing}

find the first occurence of a record in FITS header `A` matching the pattern
`what`.

"""
function Base.findfirst(what, A::FITSHeader)
    # Quick return if possible to avoid additional work in unsafe_find.
    i_first = firstindex(A)
    i_last = lastindex(A)
    i_first ≤ i_last || return nothing
    unsafe_find(what, A, i_first : i_last)
end

"""
    findlast(what, A) -> i :: Union{Int,Nothing}

find the last occurence of a record in FITS header `A` matching the pattern
`what`.

"""
function Base.findlast(what, A::FITSHeader)
    # Quick return if possible to avoid additional work in unsafe_find.
    i_first = firstindex(A)
    i_last = lastindex(A)
    i_first ≤ i_last || return nothing
    unsafe_find(what, A, i_last : -1 : i_first)
end

"""
    findnext(what, A, start) -> i :: Union{Int,Nothing}

find the next occurence of a record in FITS header `A` matching the pattern
`what` at or after index `start`.

"""
function Base.findnext(what, A::FITSHeader, start::Integer)
    # Quick return if possible to avoid additional work in unsafe_find.
    i_first = max(firstindex(A), convert(Int, start)::Int)
    i_last = lastindex(A)
    i_first ≤ i_last || return nothing
    return unsafe_find(what, A, i_first : i_last)
end

"""
    findprev(what, A, start) -> i :: Union{Int,Nothing}

find the previous occurence of a record in FITS header `A` matching the pattern
`what` at or before index `start`.

"""
function Base.findprev(what, A::FITSHeader, start::Integer)
    # Quick return if possible to avoid additional work in unsafe_find.
    i_first = firstindex(A)
    i_last = min(lastindex(A), convert(Int, start)::Int)
    i_first ≤ i_last || return nothing
    return unsafe_find(what, A,  i_last : -1 : i_first)
end

# Searching for any unsupported pattern yields nothing.
unsafe_find(what::Any, A::FITSHeader, I::OrdinalRange{<:Int}) = nothing

unsafe_find(card::FITSCard, A::FITSHeader, I::OrdinalRange{<:Int}) =
    unsafe_find(card.key, card.name, A, I)

function unsafe_find(name::AbstractString, A::FITSHeader, I::OrdinalRange{<:Int})
    try
        quick_key, full_name = check_keyword(name)
        return unsafe_find(quick_key, full_name, A, I)
    catch
    end
    return nothing
end

function unsafe_find(key::FITSKey, name::AbstractString, A::FITSHeader,
                     I::OrdinalRange{<:Int})
    if ncodeunits(name) ≤ FITS_SHORT_KEYWORD_SIZE
        @inbounds for i ∈ I
            card = A[i]
            card.key === key && return i
        end
    else
        @inbounds for i ∈ I
            card = A[i]
            card.key === key && card.name == name && return i
        end
    end
    return nothing
end

function unsafe_find(pred::Base.Callable, A::FITSHeader, I::OrdinalRange{<:Int})
    @inbounds for i ∈ I
        pred(A[i]) && return i
    end
    return nothing
end

end # module
