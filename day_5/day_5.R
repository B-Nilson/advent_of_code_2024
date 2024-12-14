# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

# write_answer(part = 1)


# Part 2 ---------------------------------------

# write_answer(part = 2)
