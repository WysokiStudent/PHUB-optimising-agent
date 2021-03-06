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
  
  # Is number of hubs correctly defined
  expect_equal(problem$number.of.hubs, phub.test.file.number.of.hubs)
  
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


test_that("Is initial well state defined", {
  problem <- initialize.problem(phub.test.file.name)
  
  expect_false(is.null(problem$state.initial))
  expect_length(problem$state.initial, problem$number.of.hubs)
  expect_true(all(is.integer(problem$state.initial)))
  expect_true(all(problem$state.initial >= 1))
})


test_that("Is 'get.evaluation' well defined", {
  problem <- initialize.problem(phub.test.file.name)
  
  equal.state <- c(1, 2)
  good.state <- c(1, 2)
  bad.state <- c(9, 10)
  
  expect_true(get.evaluation(equal.state, problem) == get.evaluation(equal.state, problem))
  expect_true(get.evaluation(good.state, problem) < get.evaluation(bad.state, problem))
  expect_false(get.evaluation(good.state, problem) > get.evaluation(bad.state, problem))
})


test_that("Is 'is.applicable' well defined", {
  problem <- initialize.problem(phub.test.file.name)
  
  states.to.test = list(
    list(
      # This state is illegal but good for tests
      state = c(rep.int(1, phub.test.file.number.of.hubs)), 
      illegal.action.value = -1
    ),
    list(
      # This state is illegal but good for tests
      state = c(rep.int(nrow(phub.test.file.distances), phub.test.file.number.of.hubs)),
      illegal.action.value = 1
    ),
    list(
      state = 2:(phub.test.file.number.of.hubs+1),
      illegal.action.value = NaN
    )
  )
  
  lapply(states.to.test, function (test.state) {
    apply(problem$actions.possible, 1,  function (action) {
      if (test.state$illegal.action.value %in% action)
        expect_false(is.applicable(test.state$state, action, problem))
      else if (anyDuplicated(effect(test.state$state, action)))
        # A state where there are 2 hubs at the same place is illegal
        expect_false(is.applicable(test.state$state, action, problem))
      else
        expect_true(is.applicable(test.state$state, action, problem))
    })
  })
})


test_that("Is 'effect' well defined", {
  problem <- initialize.problem(phub.test.file.name)
  
  state <- 2:(problem$number.of.hubs+1)
  apply(problem$actions.possible, 1, function (action) {
    expect_true(identical(state + as.vector(unlist(action)), effect(state, action)))
    expect_true(identical(state, effect(effect(state, action), -action)))
  })
})