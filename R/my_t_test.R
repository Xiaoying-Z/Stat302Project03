#' Test function
#'
#' This function performs one and two sample t-tests on vectors of data.
#' @param x Numeric vector input of data.
#' @param alternative A character string specifying the alternative hypothesis,
#'   must be one of \code{two.sided}, \code{greater} or \code{less}.
#' @param mu A number indication the null hupothesis value of mean.
#' @keywords inference
#'
#' @return A list contains the numeric test statistic, the degree of freedom,
#'   the value of the parameter \code{alternative}, and the numeric \code{p-value}.
#'
#' @examples
#' my_t.test(my_iris$Sepal.Length, "less", 5)
#'
#' @imprt stats
#'
#' @export
my_t.test <- function(x, alternative, mu){
  if(!is.numeric(x) | !is.numeric(mu)){
    stop("Inpout for x and mean must be numeric.")
  } else if (alternative != "two.sided" &
      alternative != "less" &
      alternative != "greater"){
    stop('The second parameter for alternative hypothesis only accepts
         \"two.sided\", \"less\" or \"greater\".')
  } else {
    my_se <- sd(x)/sqrt(length(x))
    test_stat <- (mean(x) - mu) / my_se
    df = length(x) - 1
    if (alternative == "two.sided"){
      p_val = 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    } else if (alternative == "less"){
      p_val = pt(test_stat, df, lower.tail = TRUE)
    } else {
      p_val = pt(test_stat, df, lower.tail = FALSE)
    }
    my_result <- list("test_stat" = test_stat, "df" = df,
                      "alternative" = alternative,
                      "p-value" = p_val )
    return (my_result)
  }
}
