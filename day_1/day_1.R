write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input_list <- "input.txt" |>
  data.table::fread()

list_a <- sort(input_list$V1)
list_b <- sort(input_list$V2)

(list_b - list_a) |>
  abs() |>
  sum() |>
  write_answer(part = 1)

# Part 2 --------------------------------------

occurences <- list_a |>
  data.frame() |>
  dplyr::left_join(
    plyr::count(list_b),
    by = c(list_a = "x"),
  )

with(
  occurences,
  list_a * ifelse(is.na(freq), 0, freq)
) |>
  sum() |>
  write_answer(part = 2)
