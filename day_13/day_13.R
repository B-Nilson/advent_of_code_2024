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

buttons <- input[input != ""] |>
  stringr::str_remove("Button [A,B]: |Prize: ") |>
  matrix(ncol = 3, byrow = TRUE) |>
  data.frame() |>
  setNames(c("A", "B", "Prize")) |>
  dplyr::mutate(
    dplyr::across(c("A", "B", "Prize"), \(x) x |>
      stringr::str_remove("X=?") |>
      stringr::str_split(", Y=?", simplify = TRUE) |>
      apply(2, as.numeric) |>
      data.frame() |>
      setNames(c("X", "Y")))
  )

press_costs <- list(A = 3, B = 1)
max_presses <- 100

# rows = A, cols = B, top left corner is 0 presses each
calculate_cost_matrix <- function(max_presses, press_costs) {
  cost_matrix <- matrix(0, nrow = max_presses + 1, ncol = max_presses + 1)
  cost_matrix[1, ] <- press_costs$A * 0:max_presses
  for (i in 1:max_presses) {
    cost_matrix[i + 1, ] <- cost_matrix[i, ] + press_costs$B
  }
  return(cost_matrix)
}

cost_matrix <- calculate_cost_matrix(max_presses, press_costs)

get_best_cost <- function(button) {
  button = as.list(button)
  press_matrix_X <- max_presses |>
    calculate_cost_matrix(list(A = button$A.X, B = button$B.X))
  press_matrix_Y <- max_presses |>
    calculate_cost_matrix(list(A = button$A.Y, B = button$B.Y))

  correct_x <- press_matrix_X == button$Prize.X
  correct_y <- press_matrix_Y == button$Prize.Y
  if (!any(correct_x & correct_y)) {
    return(NULL)
  }
  correct_costs <- cost_matrix[correct_x & correct_y]
  min(correct_costs)
}

best_costs = buttons |>
  apply(1, get_best_cost) |>
  unlist()
sum(best_costs) |> write_answer(part = 1)


# Part 2 ---------------------------------------

# write_answer(part = 2)
