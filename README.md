# FITSCards [![Build Status](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/emmt/FITSCards.jl/actions/workflows/CI.yml?query=branch%3Amain) [![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/FITSCards.jl?svg=true)](https://ci.appveyor.com/project/emmt/FITSCards-jl) [![Coverage](https://codecov.io/gh/emmt/FITSCards.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/emmt/FITSCards.jl)

`FITSCards` is a Julia package for storing and parsing FITS header cards.


## Building FITS cards

A FITS header card associates a keyword (or a name) with a value and a comment
(both optional). A FITS header card can be efficiently stored as an instance of
`FITSCard` which can be built by:

``` julia
card = FITSCard(key, val, com="")
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

A FITS card can also be built from a pair:

``` julia
card = FITSCard(key => (val, com))
card = FITSCard(key => val::Number)
card = FITSCard(key => str::AbstractString)
```

In the second case, the comment is assumed to be empty. In the third case, the
string `str` is assumed to be the card comment if `key` is `"COMMENT"` or
`"HISTORY"` and is assumed to be the card value otherwise.

Conversely, `Pair(card)` yields the pair `key => (val, com)`. The `convert` method
is extended by the `FITSCards` package to perform these conversions.

If the string value is too long of a FITS card, it shall be split across
several consecutive `CONTINUE` cards when writing a FITS file. Likewise, if the
comment of a commentary keyword is too long, it shall be split across several
consecutive cards with the same keyword when writing a FITS file.


## Cards properties

FITS cards have properties:

``` julia
card.type    # yields the type of the card: FITS_LOGICAL, FITS_INTEGER, etc.
card.name    # yields the name of the card
card.value   # yields the value of the card
card.comment # yields the comment of the card
```

Beware that `card.value` does not yield a *type-stable* result. To retrieve the
card value with a known type, use one of:

``` julia
card.logical   # yields the logical value of the card as a Bool
card.integer   # yields the integer value of the card as an Int
card.float     # yields the floating-point value of the card as a Float64
card.complex   # yields the complex value of the card as a Complex{Float64}
card.string    # yields the string value of the card as a String
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
to 0x7E). Hence Julia strings (encoded in ASCII or in UTF8) can be treated as
vectors of bytes. The parsing methods provided by the `FITSCards` package
exploit this to deal with either vectors of bytes (of type
`AbstractVector{UInt8}`) or with Julia strings (of type `String` or
`SubString{String}`).
