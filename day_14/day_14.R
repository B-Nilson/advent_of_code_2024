# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

move_robots <- function(robots, dims) {
  robots[, 1] <- (robots[, 1] + robots[, 3] - 1) %% dims[1] + 1
  robots[, 2] <- (robots[, 2] + robots[, 4] - 1) %% dims[2] + 1
  return(robots)
}

get_quadrant <- function(x, y, dims) {
  mids <- ceiling(dims / 2)
  quadrants <- dplyr::case_when(
    x < mids[1] & y < mids[2] ~ 1,
    x < mids[1] & y > mids[2] ~ 2,
    x > mids[1] & y < mids[2] ~ 3,
    x > mids[1] & y > mids[2] ~ 4,
    TRUE ~ NA
  )
  if (mids[1] %% 1 != 0) {
    quadrants[mids[1] + 1, ] <- NA
  }
  if (mids[2] %% 1 != 0) {
    quadrants[, mids[2] + 1] <- NA
  }
  return(quadrants)
}

get_safety_score <- function(robots, dims) {
  safety_factors = get_quadrant(robots[, 1], robots[, 2], dims) |>
    table()
  safety_score <- 1
  for (sf in safety_factors) safety_score <- safety_score * sf
  return(safety_score)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

room_dimensions <- c(101, 103)

robots_initial <- input |>
  stringr::str_remove_all("p=|v=") |>
  stringr::str_split(" |,", simplify = TRUE) |>
  apply(2, as.numeric)
robots_initial[, 1:2] <- robots_initial[, 1:2] + 1

robots <- robots_initial
for (i in 1:100) {
  robots <- move_robots(robots, room_dimensions)
}

get_safety_score(robots, room_dimensions) |> 
  write_answer(part = 1)

# Part 2 ---------------------------------------

max_iterations = room_dimensions[1] * room_dimensions[2]

robot_movements = list(robots_initial)
safety_scores = c(get_safety_score(robots_initial, room_dimensions))
for(i in 1:max_iterations) {
  robot_movements[[i + 1]] <- move_robots(robot_movements[[i]], room_dimensions)
  safety_scores[i + 1] <- get_safety_score(robot_movements[[i + 1]], room_dimensions)
}

which.min(safety_scores)

# write_answer(part = 2)
