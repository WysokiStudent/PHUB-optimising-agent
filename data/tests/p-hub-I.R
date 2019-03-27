library(testthat)
source("../../problems/p-hub-I.R")

phub.test.file.name <- "../phub_test_10.txt"
phub.test.file.number.of.hubs <- 2
phub.test.file.distances <- matrix(
  c(
    0,1,2,3,4,5,6,7,8,9,
    1,0,12,13,14,15,16,17,18,19,
    2,12,0,23,24,25,26,27,28,29,
    3,13,23,0,34,35,36,37,38,39,
    4,14,24,34,0,45,46,47,48,49,
    5,15,25,35,45,0,56,57,58,59,
    6,16,26,36,46,56,0,67,68,69,
    7,17,27,37,47,57,67,0,78,79,
    8,18,28,38,48,58,68,78,0,89,
    9,19,29,39,49,58,69,79,89,0
  ), 
  nrow = 10, ncol = 10, byrow = TRUE
)

test_that("Does 'intialize.problem()' work correctly", {
  problem <- initialize.problem(phub.test.file.name)
  
  # Are hubs correctly defined
  expect_equal(problem$number.of.hubs, phub.test.file.number.of.hubs)
  expect_length(problem$hubs, problem$number.of.hubs)
  expect_false(anyNA(problem$hubs))
  expect_true(all(problem$hubs == floor(problem$hubs)))
  
  # Are distances feasable
  expect_false(anyNA(problem$distances))
  expect_true(is.numeric(problem$distances))
  expect_true(all(phub.test.file.distances == problem$distances))
  
  # Are possible actions well defined
  kExpectedPossibleActions <- array(
    c(c(-1, 1), c(0, 1), c(1, 1),
    c(-1, 0),            c(1, 0),
    c(-1, -1), c(0, -1), c(1, -1)
    ),
    dim = c(2, 8)
  )
  does.action.exist <- FALSE
  for (expected.column.index in 1:ncol(kExpectedPossibleActions)) {
    for (possible.row.index in 1:nrow(problem$actions.possible)) {
      if (all(kExpectedPossibleActions[, expected.column.index] == as.vector(unlist(problem$actions.possible[possible.row.index, ])))) {
          does.action.exist <- TRUE
          break
      }
    }
    expect_true(does.action.exist)
    does.action.exist <- FALSE
  }
})


test_that("Is initial state defined", {
  problem <- initialize.problem(phub.test.file.name)
  
  expect_false(is.null(problem$state.initial))
  expect_true(all(is.integer(problem$state.initial)))
})