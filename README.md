# FITSCards [![Build Status](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/FITSCards.jl?svg=true)](https://ci.appveyor.com/project/emmt/FITSCards-jl) [![Coverage](https://codecov.io/gh/emmt/FITSCards.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/FITSCards.jl)

`FITSCards` is a pure [Julia](https://julialang.org/) package for storing and
parsing FITS header cards. [FITS (for *Flexible Image Transport
System*)](https://fits.gsfc.nasa.gov/fits_standard.html) is a data file format
widely used in astronomy. A FITS file is a concatenation of *Header Data Units*
(HDUs) that consist in a header part and a data part. The header of a HDU is a
collection of so-called *FITS cards*. Each such card is stored in textual form
and associates a keyword with a value and/or a comment.

The `FITSCards` package is intended to provide:

- methods for fast parsing of a FITS header or of a piece of a FITS header that
  is a single FITS header card;

- an expressive API for creating FITS cards and accessing their components
  (keyword, value, and comment), possibly, in a *type-stable* way.


## Building FITS cards

A FITS header card associates a keyword (or a name) with a value and a comment
(both optional). A FITS header card can be efficiently stored as an instance of
`FITSCard` built by:

``` julia
card = FITSCard(key => (val, com))
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
card = FITSCard(key => val::Number)
card = FITSCard(key => str::AbstractString)
```

In the 1st case, the comment is assumed to be empty. In the 2nd case, the
string `str` is assumed to be the card comment if `key` is `"COMMENT"` or
`"HISTORY"` and the card value otherwise.

Conversely, `Pair(card)` yields the pair `key => (val, com)`. The `convert` method
is extended by the `FITSCards` package to perform these conversions.

If the string value of a FITS card is too long, it shall be split across
several consecutive `CONTINUE` cards when writing a FITS file. Likewise, if the
comment of a commentary keyword is too long, it shall be split across several
consecutive cards with the same keyword when writing a FITS file.


## FITS cards properties

FITS cards have properties:

``` julia
card.type     # type of card: FITS_LOGICAL, FITS_INTEGER, etc.
card.key      # quick key of card: FITS"BITPIX", FITS"HIERARCH", etc.
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
cards as pairs, the `FITSCard` constructor automatically converts long keywords
or multi-word keywords into a `HIERARCH` FITS keyword by prefixing the keyword
with the string `"HIERARCH "` for example:

``` julia
julia> card = FITSCard("VERY-LONG-NAME" => (2, "keyword is longer than 8 characters"))
FITSCard: HIERARCH VERY-LONG-NAME = 2 / keyword is longer than 8 characters

julia> card.name
"HIERARCH VERY-LONG-NAME"

julia> FITSCard("SOME KEY" => (3, "keyword has 8 characters but 2 words"))
FITSCard: HIERARCH SOME KEY = 3 / keyword has 8 characters but 2 words

julia> card.name
"HIERARCH SOME KEY"

```

This rule is only applied to the construction of FITS cards from pairs. When
parsing a FITS header card from a file, the `"HIERARCH "` prefix must be
present.

The non-exported method `FITSCards.keyword` may be used to apply this rule:

``` julia
julia> FITSCards.keyword("VERY-LONG-NAME")
"HIERARCH VERY-LONG-NAME"

julia> FITSCards.keyword("SOME KEY")
"HIERARCH SOME KEY"

julia> FITSCards.keyword("NAME")
"NAME"

julia> FITSCards.keyword("HIERARCH NAME")
"HIERARCH NAME"
```


## Quick FITS keys

In `FITSCards`, a key of type `FITSKey` is a 64-bit value computed from a FITS
keyword. The key of a short FITS keyword is unique and exactly matches the
first 8 bytes of the keyword as it is stored in a FITS file. Thus quick keys
provide fast means to compare and search FITS keywords. The constructor
`FITSKey(name)` yields the quick key of the string `name`. A quick key may be
literally expressed by using the `@FITS_str` macro in Julia code. For example:

``` julia
card.key == FITS"NAXIS"
```

is faster than, say `card.name == "NAXIS"`, to check whether the name of the
FITS header card `card` is `"NAXIS"`. This is because, the comparison is
performed on a single integer (not on several characters) and expression
`FITS"...."` is a constant computed at compile time with no run-time penalty.
Compared to `FITSKey(name)`, `FITS"...."` checks the validity of the characters
composing the literal short keyword (again this is done at compile time so
without run-time penalty) and, for readability, does not allow for trailing
spaces.

For a `HIERARCH` keyword, the quick key is equal to the constant
`FITS"HIERARCH"` whatever the other part of the keyword.


## Parsing of FITS header cards

Each FITS header card is stored in a FITS file as 80 consecutive bytes from the
restricted set of ASCII characters from `' '` to `'~'` (hexadecimal codes 0x20
to 0x7E). Hence Julia strings (whether they are encoded in ASCII or in UTF8)
can be treated as vectors of bytes. The parsing methods provided by the
`FITSCards` package exploit this to deal with FITS headers and cards stored as
either vectors of bytes (of type `AbstractVector{UInt8}`) or as Julia strings
(of type `String` or `SubString{String}`).

A `FITSCard` object can be built by parsing a FITS header card as it is stored
in a FITS file:

``` julia
card = FITSCard(buf; offset=0)
```

where `buf` is either a string or a vector of bytes. Keyword `offset` can be
used to specify the number of bytes to skip at the beginning of `buf`, so that
it is possible to extract a specific FITS header card, not just the first one.
At most, the 80 first bytes after the offset are scanned to build the
`FITSCard` object. The next FITS card to parse is then at `offset + 80` and so
on.

The considered card may be shorter than 80 bytes, the result being exactly the
same as if the missing bytes were spaces. If there are no bytes left, a
`FITSCard` object equivalent to the final `END` card of a FITS header is
returned.


## Timings

`FITSCards` is ought to be fast. Below are times and memory allocations for
parsing 80-byte FITS cards measured with Julia 1.8.5 on a Linux laptop with an
Intel Core i7-5500U CPU:

- parsing logical FITS card:  122.777 ns (2 allocations:  64 bytes)
- parsing integer FITS card:  133.269 ns (2 allocations:  72 bytes)
- parsing HIERARCH FITS card: 163.408 ns (2 allocations:  88 bytes)
- parsing float FITS card:    308.244 ns (4 allocations: 152 bytes)
- parsing complex FITS card:  422.613 ns (5 allocations: 184 bytes)
- parsing string FITS card:   199.827 ns (4 allocations: 144 bytes)
- parsing string with quotes: 187.920 ns (4 allocations: 168 bytes)
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
