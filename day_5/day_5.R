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

# Seperate rule and update entries in input
rule_entries <- 1:(which(input == "") - 1)
rules <- input[rule_entries] |>
  stringr::str_split("\\|", simplify = TRUE) |>
  as.data.frame() |>
  setNames(c("X", "Y")) |>
  dplyr::arrange(X, Y)
update_entries <- (which(input == "") + 1):length(input)
updates <- input[update_entries] |>
  stringr::str_split(",") |>
  lapply(as.numeric)

# Checks if an update matches the page order rules
check_rules <- function(update, rules) {
  rules |>
    apply(1, \(rule) {
      a_loc <- which(update == rule[1])
      b_loc <- which(update == rule[2])
      if (length(c(a_loc, b_loc)) != 2) {
        return(TRUE)
      }
      a_loc < b_loc
    })
}

# Check for valid updates
valid_updates <- updates |>
  sapply(\(update) all(check_rules(update, rules)))
# Get the middle number from those and sum
updates[which(valid_updates)] |>
  sapply(\(x) x[median(1:length(x))]) |>
  sum() |>
  write_answer(part = 1)

# Part 2 ---------------------------------------
## What do you get if you add up the middle page
## numbers after correctly ordering just the unordered updates?

invalid_updates <- updates[-which(valid_updates)]

# Shift value 1 behind value 2 in each invalid rule
#  until no more invalid rules remain (takes a minute or so)
apply_rules <- function(update, rules) {
  passes <- update |> check_rules(rules)
  while (any(!passes)) {
    failed_rules <- rules[!passes, ]
    rule <- failed_rules[1, ] |> unlist()
    locs <- which(update == rule[1]) |>
      c(which(update == rule[2]))
    if (locs[2] == 1) {
      update <- c(update[locs[1]], update[-locs[1]])
    } else {
      update <- c(
        update[1:(locs[2] - 1)],
        update[locs[1]],
        update[locs[2]:length(update)]
      ) |> unique()
    }
    passes <- update |> check_rules(rules)
  }
  return(update)
}

# Fix invalid updates and report sum of middle values
invalid_updates |>
  lapply(apply_rules, rules = rules) |>
  sapply(\(x) x[median(1:length(x))]) |>
  sum() |>
  write_answer(part = 2)
