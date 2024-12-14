# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
## What is the total distance between your lists?

input_list <- "input.txt" |>
  data.table::fread()

list_a <- sort(input_list$V1)
list_b <- sort(input_list$V2)

(list_b - list_a) |>
  abs() |>
  sum() |>
  write_answer(part = 1)

# Part 2 --------------------------------------
## What is their similarity score? sum(value * occurences)

# Get occurences of list_a values in list_b
occurences <- data.frame(list_a) |>
  dplyr::left_join(
    plyr::count(list_b),
    by <- c(list_a = "x")
  )

# Calulcate similarity score
occurences |>
  with(list_a * ifelse(is.na(freq), 0, freq)) |>
  sum() |>
  write_answer(part = 2)
