# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

# Part 1 ---------------------------------------
## What do you get if you add up the middle page 
##   number from the correctly-ordered updates?

input <- "input.txt" |>
  readLines()

rule_entries <- 1:(which(input == "") - 1)
rules <- input[rule_entries] |>
  stringr::str_split("\\|", simplify = TRUE) |>
  as.data.frame() |>
  setNames(c("X", "Y"))

update_entries <- (which(input == "") + 1):length(input)
updates <- input[update_entries] |>
  stringr::str_split(",") |>
  lapply(as.numeric)

check_rules <- function(update, rules) {
  rules |> apply(1, \(rule) {
    a_loc <- which(update == rule[1])
    b_loc <- which(update == rule[2])
    if (length(c(a_loc, b_loc)) != 2) {
      return(TRUE)
    }
    a_loc < b_loc
  }) |>
    all()
}

valid_updates = updates |>
  sapply(check_rules, rules = rules)

middle = function(x) {
  x[median(1:length(x))]
}

updates[which(valid_updates)] |>
  sapply(middle) |>
  sum()

# write_answer(part = 1)
# Part 2 ---------------------------------------

# write_answer(part = 2)
