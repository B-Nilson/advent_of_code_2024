write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

reports <- input |>
  stringr::str_split(" ")

is_safe <- function(levels) {
  # Get changes in levels
  levels <- as.numeric(levels)
  changes <- (levels - dplyr::lag(levels))[-1]
  # Assess if changes too large or both + and -
  is_0_or_gt_3 <- abs(changes) > 3 | abs(changes) < 1
  is_neg_and_pos <- any(changes < 0) & any(changes > 0)
  # Return safe if passes both tests
  !(any(is_0_or_gt_3) | is_neg_and_pos)
}

reports |>
  sapply(is_safe) |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------

problem_dampener = function(levels){
  1:length(levels) |>
    sapply(\(i) levels[-i] |> is_safe()) |>
    any()
}

reports |>
  sapply(problem_dampener) |>
  sum() |>
  write_answer(part = 2)
