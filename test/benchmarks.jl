module BenchmarkingFITSCards
using FITSCards
using BenchmarkTools

let s = "SIMPLE  =                    T / this is a FITS file                            "
    print("- parsing logical FITS card: ")
    @btime FITSCard($s)
end
let s = "BITPIX  =                  -32 / number of bits per data pixel                  "
    print("- parsing integer FITS card: ")
    @btime FITSCard($s)
end
let s = "HIERARCH ESO OBS EXECTIME = +2919 / Expected execution time                     "
    print("- parsing HIERARCH FITS card:")
    @btime FITSCard($s)
end
let s = "CRVAL3  =                 0.96 / CRVAL along 3rd axis                           "
    print("- parsing float FITS card:   ")
    @btime FITSCard($s)
end
let s = "COMPLEX = (-2.7,+3.1d5)          / some other complex value                     "
    print("- parsing complex FITS card: ")
    @btime FITSCard($s)
end
let s = "EXTNAME = 'SCIDATA '           / a simple string                                "
    print("- parsing string FITS card:  ")
    @btime FITSCard($s)
end
let s = "REMARK  = 'Joe''s taxi'        / a string with an embedded quote                "
    print("- parsing string with quotes:")
    @btime FITSCard($s)
end
let s = "COMMENT   Some comments (with leading spaces that should not be removed)        "
    print("- parsing COMMENT FITS card: ")
    @btime FITSCard($s)
end
let s = "HISTORY A new history starts here...                                            "
    print("- parsing HISTORY FITS card: ")
    @btime FITSCard($s)
end
let s = "                                                                                "
    print("- parsing blank FITS card:   ")
    @btime FITSCard($s)
end
let s = "END                                                                             "
    print("- parsing END FITS card:     ")
    @btime FITSCard($s)
end

end # module
