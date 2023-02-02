# User visible changes in `FITSBase` package

## Version 0.2.0

- Package renamed `FITSBase.jl`.
- To avoid collisions with `FITSIO.jl` types are prefixed by `Fits` instead of
  `FITS`.

## Version 0.1.1

- Package name `FITSCards.jl`.
- Add `FITSHeader` structure.
- New card properties: `card.units` and `card.unitless`.
