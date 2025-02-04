# Generic function for outputing answer files
write_answer <- function(x, part) {
  out_file <- paste0("answer_", part, ".txt")
  x |>
    as.character() |>
    writeLines(out_file)
}

move_robots <- function(robots) {
  robots |>
    dplyr::mutate(
      p.x = wrap_grid(p.x + v.x, room_dimensions[1]),
      p.y = wrap_grid(p.y + v.y, room_dimensions[2]),
      quadrant = get_quadrant(p.x, p.y, room_dimensions),
      time = time + 1
    )
}

wrap_grid <- function(position, max_value) {
  dplyr::case_when(
    position < 1 ~ max_value + position,
    position > max_value ~ 0 + position %% max_value,
    TRUE ~ position
  )
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
  if(mids[1] %% 1 != 0) {
    quadrants[mids[1] + 1, ] <- NA
  }
  if(mids[2] %% 1 != 0) {
    quadrants[, mids[2] + 1] <- NA
  }
  return(quadrants)
}

# Part 1 ---------------------------------------

input <- "input.txt" |>
  readLines()

room_dimensions <- c(101, 103)

robots_initial <- input |>
  stringr::str_remove_all("p=|v=") |>
  stringr::str_split(" |,", simplify = TRUE) |>
  apply(2, as.numeric) |>
  data.frame() |>
  setNames(c("p.x", "p.y", "v.x", "v.y")) |>
  dplyr::mutate(
    p.x = p.x + 1, # starts at 0 -> 1
    p.y = p.y + 1, # starts at 0 -> 1
    robot = 1:dplyr::n(),
    time = 0
  )

robot_movements <- list(robots_initial)
for (i in 1:100) {
  robot_movements[[i + 1]] <- move_robots(robot_movements[[i]])
}

safety_factors <- robot_movements |>
  dplyr::last() |>
  dplyr::filter(!is.na(quadrant)) |>
  dplyr::group_by(quadrant) |>
  dplyr::summarise(
    count = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::pull(count)
total_sf <- 1
for (sf in safety_factors) {
  total_sf <- total_sf * sf
}

total_sf |>
  write_answer(part = 1)



# Part 2 ---------------------------------------

# write_answer(part = 2)
