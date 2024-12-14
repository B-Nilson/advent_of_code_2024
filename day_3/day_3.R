write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

extract_valid_muls = function(lines) {
  pattern = "mul\\(\\d*?,\\d*?\\)"
  lines |>
    stringr::str_extract_all(pattern, simplify = TRUE) |>
    as.vector()
} 

do_mul = function(muls) {
  muls |>
    stringr::str_remove_all("mul\\(|\\)") |>
    stringr::str_split(",", simplify = TRUE) |>
    apply(2, as.numeric) |>
    apply(1, \(mul) mul[1] * mul[2])
}

input |>
  extract_valid_muls() |>
  do_mul() |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------
