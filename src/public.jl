# The code complexity below is to avoid errors with CI and coverage tools.
# See https://discourse.julialang.org/t/is-compat-jl-worth-it-for-the-public-keyword
if VERSION ≥ v"1.11.0-DEV.469"
    macro public(args::Union{Symbol,Expr}...)
        esc(Expr(:public, args...))
    end
else
    macro public(args::Union{Symbol,Expr}...)
        nothing
    end
end

@public is_structural is_comment is_comment is_naxis is_end
