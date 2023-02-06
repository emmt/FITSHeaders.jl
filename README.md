# BaseFITS [![Build Status](https://github.com/emmt/BaseFITS.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/BaseFITS.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/BaseFITS.jl?svg=true)](https://ci.appveyor.com/project/emmt/BaseFITS-jl) [![Coverage](https://codecov.io/gh/emmt/BaseFITS.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/BaseFITS.jl)

`BaseFITS` is a pure [Julia](https://julialang.org/) package for managing basic
FITS structures such as FITS headers. [FITS (for *Flexible Image Transport
System*)](https://fits.gsfc.nasa.gov/fits_standard.html) is a data file format
widely used in astronomy. A FITS file is a concatenation of *Header Data Units*
(HDUs) that consist in a header part and a data part. The header of a HDU is a
collection of so-called *FITS cards*. Each such card is stored in textual form
and associates a keyword with a value and/or a comment.

The `BaseFITS` package is intended to provide:

- Methods for fast parsing of a FITS header or of a piece of a FITS header that
  is a single FITS header card.

- An expressive API for creating FITS cards and accessing their components
  (keyword, value, and comment), possibly, in a *type-stable* way.

- Methods for easy access the records of a FITS header.


## Building FITS cards

A FITS header card associates a keyword (or a name) with a value and a comment
(both optional). A FITS header card can be efficiently stored as an instance of
`FitsCard` built by:

``` julia
card = FitsCard(key => (val, com))
```

with `key` the card name, `val` its value, and `com` its comment. The value
`val` may be:

- a boolean to yield a card of type `FITS_LOGICAL`;
- an integer to yield a card of type `FITS_INTEGER`;
- a non-integer real to yield a card of type `FITS_FLOAT`;
- a complex to yield a card of type `FITS_COMPLEX`;
- a string to yield a card of type `FITS_STRING`;
- `nothing` to yield a card of type `FITS_COMMENT`;
- `missing` or `undef` to yield a card of type `FITS_UNDEFINED`.

The comment may be omitted for a normal FITS card and the value may be omitted
for a commentary FITS card:

``` julia
card = FitsCard(key => val::Number)
card = FitsCard(key => str::AbstractString)
```

In the 1st case, the comment is assumed to be empty. In the 2nd case, the
string `str` is assumed to be the card comment if `key` is `"COMMENT"` or
`"HISTORY"` and the card value otherwise.

Conversely, `Pair(card)` yields the pair `key => (val, com)`. The `convert` method
is extended by the `BaseFITS` package to perform these conversions.

If the string value of a FITS card is too long, it shall be split across
several consecutive `CONTINUE` cards when writing a FITS file. Likewise, if the
comment of a commentary keyword is too long, it shall be split across several
consecutive cards with the same keyword when writing a FITS file.


## FITS cards properties

FITS cards have properties:

``` julia
card.type     # type of card: FITS_LOGICAL, FITS_INTEGER, etc.
card.key      # quick key of card: Fits"BITPIX", Fits"HIERARCH", etc.
card.name     # name of card
card.value    # callable object representing the card value
card.comment  # comment of card
card.units    # units of card value
card.unitless # comment of card without the units part if any
```

As the values of FITS keywords have different types, `card.value` does not
yield a Julia value but a callable object. Called without any argument, this
object yields the actual card value:

``` julia
card.value() -> val::Union{Bool,Int64,Float64,ComplexF64,String,Nothing,Missing}
```

but such a call is not *type-stable* as indicated by the union `Union{...}` in
the above type assertion. For a type-stable result, the card value can be
converted to a given data type `T`:

``` julia
card.value(T)
convert(T, card.value)
```

both yield the value of `card` converted to type `T`. For readability, `T` may
be an abstract type: `card.value(Integer)` yields the same result as
`card.value(Int64)`, `card.value(Real)` or `card.value(AbstractFloat)` yield
the same result as `card.value(Float64)`, `card.value(Complex)` yields the same
result as `card.value(ComplexF64)`, and `card.value(AbstractString)` yields the
same result as `card.value(String)`.

To make things easier, a few properties are aliases that yield the card value
converted to a specific type:

``` julia
card.logical :: Bool       # alias for card.value(Bool)
card.integer :: Int64      # alias for card.value(Integer)
card.float   :: Float64    # alias for card.value(Real)
card.complex :: ComplexF64 # alias for card.value(Complex)
card.string  :: String     # alias for card.value(String)
```

When the actual card value is of a different type than the one requested, an
error is thrown if the conversion is not possible or inexact.

`valtype(card)` yields the Julia type of the value of `card` while
`isassigned(card)` yields whether `card` has a value (that is whether it is
neither a commentary card nor a card with an undefined value).


## FITS keywords

There are two kinds of FITS keywords:

- Short FITS keywords are words with at most 8 ASCII characters from the
  restricted set of upper case letters (bytes 0x41 to 0x5A), decimal digits
  (hexadecimal codes 0x30 to 0x39), hyphen (hexadecimal code 0x2D), or
  underscore (hexadecimal code 0x5F). In a FITS file, keywords shorter than 8
  characters are padded with ordinary spaces (hexadecimal code 0x20).

- `HIERARCH` FITS keywords start with the string `"HIERARCH "` (with a single
  trailing space) followed by one or more words composed from the same
  restricted set of ASCII characters as short keywords and separated by a
  single space.

Keywords longer than 8 characters or composed of several words can only be
represented as `HIERARCH` FITS keywords. To simplify the representation of FITS
cards as pairs, the `FitsCard` constructor automatically converts long keywords
or multi-word keywords into a `HIERARCH` FITS keyword by prefixing the keyword
with the string `"HIERARCH "` for example:

``` julia
julia> card = FitsCard("VERY-LONG-NAME" => (2, "keyword is longer than 8 characters"))
FitsCard: HIERARCH VERY-LONG-NAME = 2 / keyword is longer than 8 characters

julia> card.name
"HIERARCH VERY-LONG-NAME"

julia> FitsCard("SOME KEY" => (3, "keyword has 8 characters but 2 words"))
FitsCard: HIERARCH SOME KEY = 3 / keyword has 8 characters but 2 words

julia> card.name
"HIERARCH SOME KEY"

```

This rule is only applied to the construction of FITS cards from pairs. When
parsing a FITS header card from a file, the `"HIERARCH "` prefix must be
present.

The non-exported method `BaseFITS.keyword` may be used to apply this rule:

``` julia
julia> BaseFITS.keyword("VERY-LONG-NAME")
"HIERARCH VERY-LONG-NAME"

julia> BaseFITS.keyword("SOME KEY")
"HIERARCH SOME KEY"

julia> BaseFITS.keyword("NAME")
"NAME"

julia> BaseFITS.keyword("HIERARCH NAME")
"HIERARCH NAME"
```


## Quick FITS keys

In `BaseFITS`, a key of type `FitsKey` is a 64-bit value computed from a FITS
keyword. The key of a short FITS keyword is unique and exactly matches the
first 8 bytes of the keyword as it is stored in a FITS file. Thus quick keys
provide fast means to compare and search FITS keywords. The constructor
`FitsKey(name)` yields the quick key of the string `name`. A quick key may be
literally expressed by using the `@Fits_str` macro in Julia code. For example:

``` julia
card.key == Fits"NAXIS"
```

is faster than, say `card.name == "NAXIS"`, to check whether the name of the
FITS header card `card` is `"NAXIS"`. This is because, the comparison is
performed on a single integer (not on several characters) and expression
`Fits"...."` is a constant computed at compile time with no run-time penalty.
Compared to `FitsKey(name)`, `Fits"...."` checks the validity of the characters
composing the literal short keyword (again this is done at compile time so
without run-time penalty) and, for readability, does not allow for trailing
spaces.

For a `HIERARCH` keyword, the quick key is equal to the constant
`Fits"HIERARCH"` whatever the other part of the keyword.


## Parsing of FITS header cards

Each FITS header card is stored in a FITS file as 80 consecutive bytes from the
restricted set of ASCII characters from `' '` to `'~'` (hexadecimal codes 0x20
to 0x7E). Hence Julia strings (whether they are encoded in ASCII or in UTF8)
can be treated as vectors of bytes. The parsing methods provided by the
`BaseFITS` package exploit this to deal with FITS headers and cards stored as
either vectors of bytes (of type `AbstractVector{UInt8}`) or as Julia strings
(of type `String` or `SubString{String}`).

A `FitsCard` object can be built by parsing a FITS header card as it is stored
in a FITS file:

``` julia
card = FitsCard(buf; offset=0)
```

where `buf` is either a string or a vector of bytes. Keyword `offset` can be
used to specify the number of bytes to skip at the beginning of `buf`, so that
it is possible to extract a specific FITS header card, not just the first one.
At most, the 80 first bytes after the offset are scanned to build the
`FitsCard` object. The next FITS card to parse is then at `offset + 80` and so
on.

The considered card may be shorter than 80 bytes, the result being exactly the
same as if the missing bytes were spaces. If there are no bytes left, a
`FitsCard` object equivalent to the final `END` card of a FITS header is
returned.


## FITS headers

The `BaseFITS` package provides objects of type `FitsHeader` to store,
possibly partial, FITS headers.


### Building a FITS header

To build a FITS header initialized with records `args..`, call:

``` julia
hdr = FitsHeader(args...)
```

where `args...` is a variable number of records in any form allowed by the
`FitsCard` constructor, it can also be a vector or a tuple of records. For
example:

``` julia
dims = (384, 288)
hdr = FitsHeader("SIMPLE" => true,
                 "BITPIX" => (-32, "32-bit floats"),
                 "NAXIS" => (length(dims), "number of dimensions"),
                 ntuple(i -> "NAXIS$i" => dims[i], length(dims))...,
                 "COMMENT" => "A comment.",
                 "COMMENT" => "Another comment.",
                 "DATE" => ("2023-02-01", "1st of February, 2023"),
                 "COMMENT" => "Yet another comment.")
```

Method `keys` can be applied to get the list of keywords in a FITS header:

``` julia
julia> keys(hdr)
KeySet for a Dict{String, Int64} with 7 entries. Keys:
  "COMMENT"
  "BITPIX"
  "SIMPLE"
  "NAXIS2"
  "NAXIS1"
  "NAXIS"
  "DATE"
```


### Retrieving records from a FITS header

A FITS header object behaves as a vector of `FitsCard` elements with integer or
keyword (string) indices. When indexed by keywords, a FITS header object is
similar to a dictionary except that the order of records is preserved and that
commentary and continuation records (with keywords `"COMMENT"`, `"HISTORY"`,
`""`, or `"CONTINUE"`) may appear more than once.

An integer (linear) index `i` or a string index `key` can be used to retrieve
a given record:

``` julia
hdr[i]   # i-th record
hdr[key] # first record whose name matches `key`
```

For example (with `hdr` as built above):

``` julia
julia> hdr[2]
FitsCard: BITPIX  = -32 / 32-bit floats

julia> hdr["NAXIS"]
FitsCard: NAXIS   = 2 / number of dimensions

julia> hdr["COMMENT"]
FitsCard: COMMENT A comment.
```

Note that, when indexing by name, the first matching record is returned. This
may be a concern for non-unique keywords as in the last above example. All
matching records can be collected into a vector of `FitsCard` elements by:

``` julia
collect(key, hdr) # all records whose name matches `key`
```

For example:

``` julia
julia> collect("COMMENT", hdr)
3-element Vector{FitsCard}:
 FitsCard: COMMENT A comment.
 FitsCard: COMMENT Another comment.
 FitsCard: COMMENT Yet another comment.

julia> collect(rec -> startswith(rec.name, "NAXIS"), hdr)
3-element Vector{FitsCard}:
 FitsCard: NAXIS   = 2 / number of dimensions
 FitsCard: NAXIS1  = 384
 FitsCard: NAXIS2  = 288

julia> collect(r"^NAXIS[0-9]+$", hdr)
2-element Array{FitsCard,1}:
 FitsCard("NAXIS1" => 384)
 FitsCard("NAXIS2" => 288)
```

This behavior is different from that of `filter` which yields another FITS
header instance:

``` julia
julia> filter(rec -> startswith(rec.name, "NAXIS"), hdr)
3-element FitsHeader:
 FitsCard: NAXIS   = 2 / number of dimensions
 FitsCard: NAXIS1  = 384
 FitsCard: NAXIS2  = 288
```

For more control, searching for the index `i` of an existing record in FITS
header object `hdr` can be done by the usual methods:

``` julia
findfirst(what, hdr)
findlast(what, hdr)
findnext(what, hdr, start)
findprev(what, hdr, start)
```

which all return a valid integer index if a record matching `what` is found and
`nothing` otherwise. The matching pattern `what` can be a keyword (string), a
FITS card (an instance of `FitsCard` whose name is used as a matching pattern),
a regular expression, or a predicate function which takes a FITS card argument
and shall return whether it matches. The find methods just yield `nothing` for
any unsupported kind of pattern.

The `eachmatch` method is a simple mean to iterate over matching records:

``` julia
eachmatch(what, hdr)
```

yields an iterator over the records of `hdr` matching `what`. For example:

``` julia
@inbounds for rec in eachmatch(what, hdr)
    ... # do something
end
```

is a shortcut for:

``` julia
i = findfirst(what, hdr)
@inbounds while i !== nothing
    rec = hdr[i]
    ... # do something
    i = findnext(what, hdr, i+1)
end
```

while:

``` julia
@inbounds for rec in reverse(eachmatch(what, hdr))
    ... # do something
end
```

is equivalent to:

``` julia
i = findlast(what, hdr)
@inbounds while i !== nothing
    rec = hdr[i]
    ... # do something
    i = findprev(what, hdr, i-1)
end
```

If it is not certain that a record exists or to avoid throwing a `KeyError`
exception, use the `get` method. For example:

``` julia
julia> get(hdr, "BITPIX", nothing)
FitsCard: BITPIX  = -32 / 32-bit floats

julia> get(hdr, "GIZMO", missing)
missing
```


### Modifying a FITS header

A record `rec` may be pushed to a FITS header `hdr` to modify the header:

``` julia
push!(hdr, rec)
```

where `rec` may have any form allowed by the `FitsCard` constructor. If the
keyword of `rec` must be unique and a record of the same name exists in `hdr`,
it is replaced by `rec`; otherwise, `rec` is appended to the end of the list of
records stored by `hdr`.

The `setindex!` method may also be used with a linear (integer) or a keyword
(string) index. The above rule for unique / non-unique keywords is always
applied. For example, the two following statements are equivalent:

``` julia
hdr[key] = (val, com)
push!(hdr, key => (val, com))
```

while, assuming `i` is an integer:

``` julia
hdr[i] = rec
```

replaces the `i`-th record in `hdr` by `rec`. The following example illustrates
how this can be used to change the comment of the BITPIX record:

``` julia
julia> if (i = findfirst("BITPIX", hdr)) != nothing
           hdr[i] = ("BITPIX" => (hdr[i].value(), "A better comment."))
       end
"BITPIX" => (-32, "A better comment.")
```


## Timings

`BaseFITS` is ought to be fast. Below are times and memory allocations for
parsing 80-byte FITS cards measured with Julia 1.8.5 on a Linux laptop with an
Intel Core i7-5500U CPU:

- parsing logical FITS card:  114.588 ns (2 allocations:  64 bytes)
- parsing integer FITS card:  118.519 ns (2 allocations:  72 bytes)
- parsing HIERARCH FITS card: 142.462 ns (2 allocations:  88 bytes)
- parsing float FITS card:    274.119 ns (4 allocations: 152 bytes)
- parsing complex FITS card:  424.060 ns (6 allocations: 248 bytes)
- parsing string FITS card:   155.694 ns (4 allocations: 144 bytes)
- parsing string with quotes: 169.223 ns (4 allocations: 168 bytes)
- parsing COMMENT FITS card:   90.615 ns (2 allocations: 112 bytes)
- parsing HISTORY FITS card:  100.591 ns (2 allocations:  72 bytes)
- parsing blank FITS card:     78.261 ns (0 allocations:   0 bytes)
- parsing END FITS card:       82.286 ns (0 allocations:   0 bytes)

The benchmark code is in file [`test/benchmarks.jl`](test/benchmarks.jl). The
HIERARCH card has an integer value. The float and complex valued cards take
more time to parse because parsing a floating-point value is more complex than
parsing, say, an integer and because the string storing the floating-point
value must be copied to replace letters `d` and `D`, allowed in FITS standard
to indicate the exponent, by an `e`.

For comparison, just extracting the keyword, value, and comment parts from a
80-characters FITS card by calling the functions `fits_get_keyname` and
`fits_parse_value` of CFITSIO library takes about 150 ns on the same machine.
This does not includes the allocation of the buffers to store these 3 parts
(about 120 ns for this) and the parsing of the value which are all included in
the timings of the `FISCard` constructor above.
