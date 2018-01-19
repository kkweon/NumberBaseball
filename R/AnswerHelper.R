#' Answer Helper Class
#' @docType class
#' @importFrom R6 R6Class
AnswerHelper <-
    R6::R6Class(
        "AnswerHelper",
        public = list(
            generateAnswer = function(n) {
                private$answer <- sample(0:9, size = n)
            },
            checkAnswer =
                function(input) {
                    result <- c(0, 0, 0)
                    for (i in 1:length(private$answer)) {
                        for (j in 1:length(input)) {
                            if (input[[i]] == private$answer[[j]]) {
                                if (i == j)
                                    result[[1]] <-
                                        result[[1]] + 1
                                else
                                    result[[2]] <-
                                        result[[2]] + 1
                                break
                            }
                        }
                    }

                    result[[length(result)]] <-
                        length(input) - result[[1]] - result[[2]]

                    private$printMessage(result)
                    result
                },
            setAnswer = function(answer) {
                private$answer <- answer
            }
        ),
        private = list(
            answer = NULL,
            printMessage = function(result) {
                s <- result[[1]]
                b <- result[[2]]
                o <- result[[3]]
                message(sprintf("Strike: %d, Ball: %d, Out: %d", s, b, o))
            }
        )
    )
