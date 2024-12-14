write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

word_search <- input |>
  stringr::str_split("", simplify = TRUE)

count_xmas_occurence <- function(x) {
  paste(x, collapse = "") |>
    stringr::str_count("XMAS")
}


word_search |>
  apply(1, count_xmas_occurence) |>
  sum()


# Part 2 ---------------------------------------

