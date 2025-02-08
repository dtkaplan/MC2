library(stringr)
library(tokenizers)
library(stopwords)
library(dplyr)

Chapters <- list(
"Preliminaries/01-quant-fun-space.qmd",
"Preliminaries/02-notation.Rmd",
"Preliminaries/03-computing.Rmd",
"Preliminaries/04-graphs-and-graphics.qmd",
"Preliminaries/05-pattern-book-functions.Rmd",
"Preliminaries/06-describing-functions.Rmd",
"Preliminaries/07-data-functions-graphics.Rmd",
"Modeling/08-parameters.Rmd",
"Modeling/09-assembling-functions.Rmd",
"Modeling/10-functions-with-multiple-inputs.Rmd",
"Modeling/11-fitting-features.Rmd",
"Modeling/12-low-order-polynomials.Rmd",
"Modeling/13-operations.Rmd",
"Modeling/14-magnitudes.Rmd",
"Modeling/15-dimensions.Rmd",
"Modeling/16-modeling-scientific-method.Rmd",
"Differentiation/17-continuous-change.Rmd",
"Differentiation/18-rate-of-change.Rmd",
"Differentiation/19-evanescent-h.Rmd",
"Differentiation/20-computing.Rmd",
"Differentiation/21-concavity.Rmd",
"Differentiation/22-cont-and-smooth.Rmd",
"Differentiation/23-rules.Rmd",
"Differentiation/24-optim.Rmd",
"Differentiation/25-partial.Rmd",
"Differentiation/26-approximation.Rmd",
"Differentiation/27-taylor.Rmd",
"Linear-combinations/28-Vectors.Rmd",
"Linear-combinations/29-linear-combinations.Rmd",
"Linear-combinations/30-projection.Rmd",
"Linear-combinations/31-target-problem.Rmd",
"Linear-combinations/32-stat-modeling.Rmd",
"Accumulation/33-intro.Rmd",
"Accumulation/34-visualizing.Rmd",
"Accumulation/35-integration.Rmd",
"Accumulation/36-functions.Rmd",
"Accumulation/37-euler.Rmd",
"Accumulation/38-symbolic.Rmd",
"Dynamics/39-diff-eq.Rmd",
"Dynamics/40-solution.Rmd",
"Dynamics/41-flow-on-line.Rmd",
"Dynamics/42-flow-on-plane.Rmd",
"Dynamics/43-modeling.Rmd",
"Dynamics/44-equilibria.Rmd",
"Dynamics/45-eigen.Rmd",
"Dynamics/46-second-order.Rmd",
"Manifestations/47-operations.Rmd",
"Manifestations/48-splines.Rmd",
"Manifestations/49-optimization.Rmd",
"Manifestations/50-probability.Rmd",
"Manifestations/51-future-value.Rmd",
"Manifestations/52-mechanics.Rmd",
"Manifestations/53-diffusion.Rmd"
)


get_words <- function(chap) {
  words <- readLines(chap) |>
    tokenize_word_stems(stopwords = stopwords::stopwords("en")) |> unlist()
  tibble::tibble(word = words) |>
    dplyr::summarize(count = n(), .by = word) |>
    dplyr::filter(!is.na(word)) |>
    dplyr::filter(!grepl("[0-9]", word)) |>
    mutate(chapter = gsub(".*/B?([0-9]{1,2})-.*", "\\1", chap)) |>
    arrange(desc(count))
}

get_functions <- function(chap) {
  Lines <- readLines(chap) |>
    tokenize_sentences() |>
    unlist() 

    Candidates <- tibble::tibble(sentence = Lines) |>
      filter(nchar(sentence) > 10) |>
      filter(grepl("\\(\\)", sentence))
    results <- str_extract_all(Candidates$sentence, "[\\$\\.\\` ^]{1}([a-zA-Z_.]+\\()") |>
      unlist(recursive = TRUE) 
    tibble::tibble(fun = lapply(results, FUN=c) |> unlist(recursive = TRUE), 
                   chapter = chap)
}

# Same as get_functions(), but with different regular expressions
get_definitions <- function(chap) {
  Lines <- readLines(chap) |>
    tokenize_sentences() |>
    unlist() 
  
  Candidates <- tibble::tibble(sentence = Lines) |>
    filter(nchar(sentence) > 10) |>
    filter(grepl("\\*{2}", sentence))
  results <- str_extract_all(Candidates$sentence, "\\*\\*([^*]*)\\*\\*") |>
    unlist(recursive = TRUE) 
  tibble::tibble(definition = lapply(results, FUN=c) |> unlist(recursive = TRUE), 
                 chapter = chap)
}

# Count number of function names, e.g sin()
Tmp_fun  <- lapply(Chapters, get_functions) |>
  bind_rows() |>
  mutate(context = substr(fun, 1, 1),
         fun = gsub("^.", "", fun))


Fun_counts <- Tmp_fun |> 
  mutate(chapter = gsub(".*/([0-9]{1,2})-.*", "\\1", chapter)) |>
  summarize(count = n(), .by = c(fun, chapter, context))

Tmp_define <-  lapply(Chapters, get_definitions) |>
  bind_rows() 

Define_counts <- Tmp_define |> 
  mutate(chapter = gsub(".*/([0-9]{1,2})-.*", "\\1", chapter)) |>
  summarize(count = n(), minc = min(chapter), .by = c(definition, chapter))

Define_overall <- Define_counts |>
  mutate(lower = tolower(definition)) |>
  summarize(total = sum(count), minc = min(minc), .by = lower)

# Count number of times each word appears
Foo <- lapply(Chapters, FUN = get_words) |>
  bind_rows()

Overall <- Foo |> summarize(total_count = sum(count), chaps = n_distinct(chapter), .by = word) |>
  arrange(desc(total_count))
