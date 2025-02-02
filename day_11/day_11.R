# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

ndigits <- function(x) {
  ifelse(x == 0, 1, floor(log10(x)) + 1)
}

split_numbers <- function(numbers, digits = NULL) {
  if (is.null(digits)) digits <- ndigits(numbers)
  denom <- 10^(digits - digits / 2)
  start <- as.integer(numbers / denom)
  end <- numbers - start * denom
  c(start, end)
}

combine_counts <- function(counts) {
  is_duplicated <- names(counts) %in%
    names(counts)[duplicated(names(counts))]
  duplicates <- counts[is_duplicated]
  if(length(duplicates) == 0) return(counts)

  unique(names(duplicates)) |>
    sapply(\(x) duplicates[names(duplicates) == x] |>
      sum()) |>
    c(counts[!is_duplicated])
}

blink_stones <- function(counts) {
  labels <- as.numeric(names(counts))
  digits <- ndigits(labels)
  new_counts = counts

  zeroes <- labels == 0
  evens <- digits %% 2 == 0
  others <- !zeroes & !evens

  names(new_counts)[others] <- labels[others] * 2024
  names(new_counts)[zeroes] <- 1
  if (any(evens)) {
    split_labels <- labels[evens] |>
      split_numbers(digits[evens])
    split_counts <- rep(counts[evens], 2) |>
      setNames(split_labels)
    new_counts <- c(new_counts[!names(counts) %in% labels[evens]], split_counts) |>
      combine_counts()
  }
  return(new_counts)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines() |>
  stringr::str_split(" ", simplify = TRUE) |>
  as.integer()

blinks <- 25
stones <- input
counts <- table(stones)
for (blink in 1:blinks) {
  counts <- blink_stones(counts)
}

sum(counts) |> write_answer(part = 1)

# Part 2 ---------------------------------------

blinks <- 75
counts <- table(stones)
for (blink in 1:blinks) {
  counts <- blink_stones(counts)
}

sum(counts) |> write_answer(part = 2)
