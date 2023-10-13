icc_report <- function(x, ...) {
  icc.title <- ifelse(x$unit=="single", "Single Score \nIntraclass Correlation", "Average Score \nIntraclass Correlation")
  
  # Use 'paste' to concatenate the text without printing it immediately
  res <- c()
  res <- c(res, paste(" ", icc.title, "\n\n", sep=""))
  res <- c(res, paste("   Model:", x$model, "\n"))
  res <- c(res, paste("   Type :", x$type, "\n\n"))
  res <- c(res, paste("   Subjects =", x$subjects, "\n"))
  res <- c(res, paste("     N Measures =", x$raters, "\n"))
  
  results <- paste(formatC(x$icc.name, width=11, flag="+"), "=", format(x$value, digits=3))
  res <- c(res, results, "\n")
  
  Ftest <- paste(formatC(paste("F(", x$df1, ",", format(x$df2, digits=3), ")", sep=""), width=11, flag="+"), "=",
                 format(x$Fvalue, digits=3), ", p =", format(x$p.value, digits=3), "\n\n")
  res <- c(res, Ftest)
  
  res <- c(res, paste(" ", round(x$conf.level * 100, digits=1), "%-Confidence Interval for \nICC Population Values:\n", sep=""))
  res <- c(res, paste("  ", round(x$lbound, digits=3), " < ICC < ", round(x$ubound, digits=3), "\n", sep=""))
  
  # Return the concatenated character vector
  return(paste(res, collapse = ""))
}

