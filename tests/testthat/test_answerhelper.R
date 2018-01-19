
context("AnswerHelper")

test_that("AnswerHelper returns 3 strikes when the guess is correct", {
    answer <- AnswerHelper$new()
    answer$setAnswer(c(1, 2, 3))
    expect_equal(answer$checkAnswer(c(1, 2, 3)), c(3, 0, 0))
})

test_that("AnswerHelp returns 1 ball and 2 outs when one answer is contained but located in other slot", {
    answer <- AnswerHelper$new()
    answer$setAnswer(c(4, 5, 1))
    expect_equal(answer$checkAnswer(c(1, 2, 3)), c(0, 1, 2))
})

test_that("AnswerHelp returns 1 strike and 2 balls when one number is correct two numbers are switched", {
    answer <- AnswerHelper$new()
    answer$setAnswer(c(3, 2, 1))
    expect_equal(answer$checkAnswer(c(1, 2, 3)), c(1, 2, 0))
})
