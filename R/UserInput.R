#' UserInput Class
#' @importFrom R6 R6Class
UserInput <- R6::R6Class(
    "UserInput",
    public = list(
        askMaxLength = function() {
            private$askPrompt("Max Length [min: 2, max: 9]? ", private$validate2to9)
        },

        askUserGuess = function(n) {
            while (TRUE) {
                xs <- private$promptVectorClean(n)
                if (length(xs) == n && private$allValid(xs))
                    return(xs)
                message("Enter ", n, " unique numbers!")
            }
        }
    ),
    private = list(
        promptVectorClean = function(n) {
            xs <-
                readline(sprintf("Enter %d number between 0 and 9 separated by space? ", n))

            if ("q" %in% xs)
                stop()

            xs <- unlist(strsplit(xs, split = " "))
            as.integer(xs)
        },
        validate2to9 = function(x)
            2 <= x && x <= 9,
        allValid =  function(xs)  {
            valid <- function(x)
                x == length(xs)
            valid(sum(0 <= xs)) &&
                valid(sum(xs <= 9)) && valid(length(unique(xs)))
        },
        askPrompt = function(prompt, isValid) {
            while (!isValid(answer <- as.integer(readline(prompt)))) {
                message("Please enter valid number")
            }
            answer
        }
    )
)
