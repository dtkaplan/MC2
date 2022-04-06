library(Zcalc) # New, catch-all package
library(Znotes)
library(here) # for file locations
library(thematic)
library(gridExtra) # for arranging plots.
library(kableExtra)

# theme the plots to match the document
thematic_rmd() #(bg="#8F8F8C", fg=NA, accent=NA)
ggplot2::theme_set(theme_bw(base_size = 16))


knitr::opts_chunk$set(out.width="90%", fig.align="center", fig.margin=TRUE,
                      collapse=TRUE # don't split output box from input.
                      )
#for regular pdf but not Tufte add:            fig.pos = "h", out.extra = "")

# Resolve the exercise number assigned to a permanent name like "3KEgLM"
# or "chicken-sit-table".
# See script in <_make_exercise_index.R>
if (file.exists("_exercise_cross_reference.csv")) {
  exercise_cross_reference <- readr::read_csv("_exercise_cross_reference.csv")
}

ref_ex <- function(perm_name) {
  # This is not yet implemented but will be based on a program that searches through all the
  # insertion commands in the chapters and assembles a table with exercise number, perm name, word-name. This
  # function will read that table and replace <perm_name> with the exercise number.
  res <- if (nchar(perm_name) < 10) { # It's a hash
      exercise_cross_reference %>% filter(hash==!!perm_name) %>% .$number
  } else {
    exercise_cross_reference %>% filter(wordname == !!perm_name) %>% .$number
  }

  if (is.null(res) || nchar(res) == 0 ) return("**MISSING EXERCISE NUMBER**")
  else return(res)
}


if (!exists("objective_list"))
  objective_list <- list()

add_objective <- function(ID, text) {
  objective_list[[length(objective_list) + 1]] <<-
                      list(ID = ID, text = text)
}

state_objective <- function(ID, text) {
  add_objective(ID, text)
  format_objective(list(ID=ID, text=text))
}

format_objective <- function(obj) {
  knitr::asis_output(glue::glue("#. **[{obj$ID}]** *{obj$text}*\n\n"))
}

show_objectives <- function() {
  Tmp <- lapply(objective_list, tibble::as_tibble) %>%
    bind_rows()
  readr::write_csv(Tmp, file="objective-list.csv")

  lapply(objective_list, format_objective) %>%
    unlist() %>%
    paste(collapse="\n") %>%
    knitr::asis_output()
}

# A blank image for spacing
BlankImage <- gf_blank(hp ~ wt, data=mtcars) %>% gf_theme(theme_void())

MC_counter <- Znotes::letter_counter()



sandbox_link <- function() {
  "[SANDBOX](https://maa-statprep.shinyapps.io/CalcZ-Sandbox/)"
}

drill_link <- function() {
  "[DRILL QUESTIONS](https://maa-statprep.shinyapps.io/Zdrill/)"
}


Chaps <- list(
  parameters = 8,
  magnitudes = 15,
  dimension = 16.00,
  pattern_book_derivs = 19.00,
  fun_notation = 5.00,
  slope_function = 9.00,
  gradient = 24.00,
  local_approx = 25.00,
  iteration = 32.00,
  taylor = 26.00,
  optimization = 23.00,
  splines = 33.00,
  modeling_cycle = 14.00,
  expectation_value = 35.00,
  foobar = NA
)

Sections <- list(
  exp_curve_fitting = "8.003",
  gradient_vector = "24.003",
  functions_as_tables = "4.002",
  likelihood = "35.004"
)

under_construction <- function() {
  "::: {.underconstruction}
**Under Construction**

Content subject to revision.
:::
"
}

# For gradescope output
# askMC <- Znotes::askGS

