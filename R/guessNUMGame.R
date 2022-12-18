#'@title guess the Number Game
#'
#'@description Design a number guessing game: the computer generates a random integer between any two integers, and then the several users guesses the random number. According to the user's given number to narrow the range of prompts. When the number is correctly given, then it will show "Congratulation! You win" and quit the game.
#'
#' @param a The start value
#' @param b The end value
#'
#' @return
#' @export
#'
#' @examples
#' guessNum(0,100)
#' 83
#'
guessNum <- function(a = 0, b = 100){
  num <- round(runif(1, a, b))
  print("Game Start")
  cat("Please enter a number beweent ",a," and ",b)
  Tmp <- scan()
  a = a; b = b; k = 1
  repeat{
    if(Tmp > num){
      a = a; b = Tmp
      cat(paste0("Wrong answer, please enter a number beweent ",a," and ",b))
      Tmp <- scan()
      k = k +1;
      next
    }else{
      if(Tmp < num){
        a = Tmp
        b = b
        cat(paste0("Wrong answer, please enter a number beweent ",a," and ",b))
        Tmp <- scan();
        k = k +1
        next
      }else{
        cat(paste0("Congratulation! You win!!! The answaer is：",num,". Total number of opportunities consumed: ",k))
        break
      }
    }
  }
}
