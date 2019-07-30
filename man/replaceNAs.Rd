\name{replaceNAs}
\alias{replaceNAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Replaces missing variables in a vector as \code{<NA>}.
}
\description{
  Given a vector (numeric, character, double, factor) with missingness coded as not \code{NA}, replace it as \code{NA}. Some examples are - 9 N/A Unknown, Not Available etc.
}
\usage{
replaceNAs(x, vNA)
}

\arguments{
  \item{x}{  A vector of values - numeric, character, double or factor.}
  \item{vNA}{  A character vector of values to be replaced that are coded as missing into \code{NA}}

}
\details{

  Both arguments should be supplied. If \code{typeof} is not matched to one of the following - \code{character, double, integer} or \code{factor}, a \code{character} vector is returned
}
\value{
  \item{x.na} { A vector with misisng values coded as \code{NA}. If \code{typeof} did not match the following - \code{character, double, integer} or \code{factor}, a \code{character} vector is returned }
}

\author{
Arshi Arora
}

\examples{
  set.seed(123)
  #sample 20 numbers from 1 -10, say we want to replace the 9s to NA
  x<-sample(1:10, 20, replace=TRUE)
  #x
  #[1]  3  8  5  9 10  1  6  9  6  5 10  5  7  6  2  9  3  1  4 10

  x.na<-replaceNAs(x,9)
  #[1]  3  8  5 NA 10  1  6 NA  6  5 10  5  7  6  2 NA  3  1  4 10

}
