module FITSHeadersMappedBuffersExt

using FITSHeaders
using MappedBuffers

FITSHeaders.Parser.PointerCapability(::Type{<:MappedBuffers.MappedBuffer}) =
    FITSHeaders.Parser.PointerFull()

end # module
