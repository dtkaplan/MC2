pattern <- "Figure \\\\@ref\\(fig:([^\\)]*)\\)"

example <- "In Figure \\@ref(fig:ramp-decomposition) this grid"

gsub(pattern, "\\@\\1", example)

# Figure \\@ref\(fig:([^\)]*)\)
# @fig-\1