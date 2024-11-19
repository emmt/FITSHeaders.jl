import Base: merge!
@deprecate merge!(dest::FitsHeader, other::Union{Pair,FitsCard}) push!(dest, other) false
