# User visible changes in `BaseFITS` package

## Version 0.3.0

- Package renamed `BaseFITS.jl`.

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
