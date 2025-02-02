# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

split_strings <- function(strings) {
  strings |> sapply(\(x) {
    middle <- ceiling(nchar(x) / 2)
    stringr::str_sub(x, 1, middle) |>
      c(stringr::str_sub(x, middle + 1))
  })
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines() |>
  stringr::str_split(" ", simplify = TRUE) |>
  as.integer()

blinks <- 25
stones <- input
for (blink in 1:blinks) {
  stones <- stones |>
    lapply(\(stone){
      if (stone == 0) {
        new_stone <- 1
      } else if (nchar(stone) %% 2 == 0) {
        new_stone <- split_string(stone) |> as.numeric()
      } else {
        new_stone <- as.numeric(stone) * 2024
      }
      as.character(new_stone)
    }) |>
    unlist()
}

length(stones) |> write_answer(part = 1)

# Part 2 ---------------------------------------

# write_answer(part = 2)
