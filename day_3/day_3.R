write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines() |>
  paste(collapse = "")

extract_valid_muls <- function(lines) {
  pattern <- "mul\\(\\d*?,\\d*?\\)"
  lines |>
    stringr::str_extract_all(pattern) |>
    unlist()
}

do_mul <- function(muls) {
  mul_row <- \(mul) as.numeric(mul[1]) * as.numeric(mul[2])
  muls |>
    stringr::str_remove_all("mul\\(|\\)") |>
    stringr::str_split(",", simplify = TRUE) |>
    apply(1, mul_row)
}

input |>
  extract_valid_muls() |>
  do_mul() |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

extract_enabled_muls <- function(lines) {
  pattern <- "do\\(\\).*?don't\\(\\)"
  paste0("do()", lines, "don't()") |>
    stringr::str_extract_all(pattern) |>
    unlist() |>
    extract_valid_muls()
}

input |>
  extract_enabled_muls() |>
  do_mul() |>
  sum() |>
  write_answer(part = 2)
