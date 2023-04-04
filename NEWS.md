# User visible changes in `BaseFITS` package

## Version 0.3.4

- *Provide support for dates:* Card values of type `Dates.DateTime` are
  automatically converted into string values and, conversely, calling
  `card.value(DateTime)`, `convert(DataTime,card.value)`, and
  `DateTime(card.value)` attempts to convert the string value of `card` into a
  date.

## Version 0.3.3

- Use `AsType` package.

- Remove `CompatHelper`.

## Version 0.3.2

- Fix silly bug in keyword comparison.

## Version 0.3.1

- Use quick table for character class.

## Version 0.3.0

- Package renamed `BaseFITS.jl`.

- `FitsHeader` can be built from list of header cards, list of pairs, and named
  tuples. Here "list" means any iterable producing items of a given type. The
  same types are allowed for `merge` and `merge!` applied to a FITS header.

- Non-exported constants `CarName`, `CardValue`, `CardComment`, and `Undefined`
  to help converting a pair into a FITS header card.

- `BaseFITS.keyword` and `BaseFITS.check_keyword` can take a symbolic name as
  argument.

## Version 0.2.1

- Can search cards in FITS header by regular expressions.
- Implement merging of headers with `merge` and `merge!`.
- `length(eachmatch(pat,hdr::FitsHeader))` yields number of matches.
- Some bug fixes.

## Version 0.2.0

- Package renamed `FITSBase.jl`.
- To avoid collisions with `FITSIO.jl` types are prefixed by `Fits` instead of
  `FITS`.

## Version 0.1.1

- Package name `FITSCards.jl`.
- Add `FITSHeader` structure.
- New card properties: `card.units` and `card.unitless`.
