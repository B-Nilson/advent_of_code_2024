# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
##  What do you get if you add up all of the
##  results of the multiplications?

input <- "input.txt" |>
  readLines() |>
  paste(collapse = "")

# Get all occurences of "mul(\d*?,\d*?)"
extract_valid_muls <- function(lines) {
  pattern <- "mul\\(\\d*?,\\d*?\\)"
  lines |>
    stringr::str_extract_all(pattern) |>
    unlist()
}
# For each occurence of muls, multiply values
do_mul <- function(muls) {
  muls |>
    stringr::str_remove_all("mul\\(|\\)") |>
    stringr::str_split(",", simplify = TRUE) |>
apply(2, as.numeric) |>
  apply(1, \(vals) vals[1] * vals[2])

}
# Extract valid muls, multiply vals, sum
input |>
  extract_valid_muls() |>
  do_mul() |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------
## What about when only including muls between do() and don't()?

# Search for valid muls between do() and don't()
extract_enabled_muls <- function(lines) {
  pattern <- "do\\(\\).*?don't\\(\\)"
  paste0("do()", lines, "don't()") |>
    stringr::str_extract_all(pattern) |>
    unlist() |>
    extract_valid_muls()
}

# Get enabled muls, multiply vals, sum
input |>
  extract_enabled_muls() |>
  do_mul() |>
  sum() |>
  write_answer(part = 2)
