#' Baseball Class
#' @docType class
#' @description Number Baseball R6Class. \code{new()} and \code{start()} to begin the game
#' @importFrom R6 R6Class
#' @include UserInput.R AnswerHelper.R
#' @keywords Game
#' @usage
#' # Initialize a class
#' baseball <- Baseball$new()
#' # Start the game
#' baseball$start()
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Constructor}
#'   \item{\code{start()}}{Start the game}
#'  }
#' @export
Baseball <- R6::R6Class(
    "Baseball",
    public = list(
        start = function() {
            "Start Game"
            private$n <- private$input$askMaxLength()
            private$answer$generateAnswer(private$n)

            clear <- FALSE
            while (!clear) {
                guess <- private$input$askUserGuess(private$n)
                result <- private$answer$checkAnswer(guess)
                private$trial <- private$trial + 1
                clear <- private$checkClear(result)
            }
            private$printResult()
        }
    ),
    private = list(
        n = 0,
        trial = 0,
        clear = FALSE,
        answer = AnswerHelper$new(),
        input = UserInput$new(),
        checkClear = function(xs) {
            xs[[1]] == private$n
        },
        printResult = function() {
            message(sprintf("Game Cleared in %d", private$trial))
        }
    )
)


