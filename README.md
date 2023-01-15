# FITSCards [![Build Status](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/FITSCards.jl?svg=true)](https://ci.appveyor.com/project/emmt/FITSCards-jl) [![Coverage](https://codecov.io/gh/emmt/FITSCards.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/FITSCards.jl)

`FITSCards` is a Julia package for storing and parsing FITS header cards.


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

If the string value is too long of a FITS card, it shall be split across
several consecutive `CONTINUE` cards when writing a FITS file. Likewise, if the
comment of a commentary keyword is too long, it shall be split across several
consecutive cards with the same keyword when writing a FITS file.


## FITS cards properties

FITS cards have properties:

``` julia
card.type    # type of card: FITS_LOGICAL, FITS_INTEGER, etc.
card.key     # short key of card: FITS"BITPIX", FITS"HIERARCH", etc.
card.name    # name of card
card.value   # value of card
card.comment # comment of card
```

Beware that `card.value` does not yield a *type-stable* result. To retrieve the
card value with a known type, use one of:

``` julia
card.logical   # value of card as a Bool
card.integer   # value of card as an Int64
card.float     # value of card as a Float64
card.complex   # value of card as a Complex{Float64}
card.string    # value of card as a String
```

With these properties, conversion is automatically attempted if the actual card
value is of a different type, throwing an error if the conversion is not
possible or inexact.

`valtype(card)` yields the type of the value of `card`. `isassigned(card)`
yields whether `card` has a value (that is whether it is neither a commentary
card nor a card with an undefined value).


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
card = FITSCard(buf, off=0)
```

where `buf` is either a string or a vector of bytes. Optional argument `off` is
the number of bytes to skip at the beginning of `buf`, so that it is possible
to extract a specific FITS header card, not just the first one. At most, the 80
first bytes after the offset are scanned to build the `FITSCard` object. The
next FITS card to parse is then at offset `off + 80` and so on.

The considered card may be shorter than 80 bytes, the result being exactly the
same as if the missing bytes were spaces. If there are no bytes left, a
`FITSCard` object equivalent to the final `END` card of a FITS header is
returned.
