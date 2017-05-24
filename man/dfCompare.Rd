\name{dfCompare}
\alias{dfCompare}
\title{Generate Delta Records}
\usage{
dfCompare(dfOld,dfNew,key)
}
\arguments{
  \item{dfOld}{Original dataframe}
  \item{dfNew}{New dataframe}
  \item{key}{Key used to join the dataframes}

}
\description{
Return dataframes for added, changed, and deleted records
}
\examples{
  a <- c(2, 3, 5)
  b <- c("aa", "bb", "cc")
  c <- c(TRUE, FALSE, TRUE)
  dfOld <- data.frame(a,b,c)

  a <- c(3,4,5)
  b <- c("aaa","dd","cc")
  c <- c(TRUE, FALSE, TRUE)
  dfNew <- data.frame(a,b,c)

  dfDelta <- dfCompare(dfOld,dfNew,"a")
}
