\name{get.colvector}
\alias{get.colvector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Returns a color vector of unique values of interest.
}

\description{
 Given a vector of class labels, return a color vector correponding to the unique class values. There is an option to set a different color for \code{NA}. A vector with corresponding color values and a key specifying color value of each class label is returned.
}

\usage{
get.colvector(labels,col, NA.flag=FALSE, NA.col="grey")
}

\arguments{
  \item{labels}{ vector, labels for which color vector is desired. Can be \code{numeric, character} or \code{factor}. If it is a \code{factor} colors are coded in the order of \code{levels} of a \code{factor} }
  \item{col}{ vector, a character vector containing colors for each unique class label. Also accepts colors from \code{colorRampPallette}}
  \item{NA.flag}{ logical, a logical flag where \code{NA} in \code{labels} vector are replaced with \code{NA.col}. Default \code{NA.flag} is \code{FALSE}.}
  \item{NA.col}{ character, color value specifying how \code{NA} should be color coded. Default color value is \code{grey}. Will only color code \code{NA} when \code{NA.flag} is \code{TRUE} }
}

\details{
 if the vector \code{labels} has \code{names}, the returned color coded vector will also have \code{names}.
}

\value{
  \item{labels.list}{ A list, with following elements - }

      \code{labels.col} : {character, a color coded character vector of \code{labels}}

      \code{key}        : { matrix, a character matrix showing how values in \code{labels} have mapped to \code{col} }

}

\author{
 Arshi Arora
}

\examples{

  set.seed(123)
  x<-sample(letters[1:4], 20, replace=TRUE)
  #table(x)
  #x
  #a b c d
  #4 5 4 7

  x.col<-get.colvector(x,c("red","blue","green","orange"))
  #x.col$labels.col
  #[1] "blue"   "orange" "blue"   "orange" "orange" "red"    "green"  "orange"
  #[9] "green"  "blue"   "orange" "blue"   "green"  "green"  "red"    "orange"
  #[17] "red"    "red"    "blue"   "orange"

  #x.col$key
  #[,1]  [,2]   [,3]    [,4]
  #ul  "a"   "b"    "c"     "d"
  #col "red" "blue" "green" "orange"

  #lets add some NAs
  x[c(1,4,5)] = NA
  #
  x.col<-get.colvector(x,c("red","blue","green","orange"), NA.flag=TRUE, NA.col="grey")
  # x.col$labels.col
  #[1] "grey"   "orange" "blue"   "grey"   "grey"   "red"    "green"  "orange"
  #[9] "green"  "blue"   "orange" "blue"   "green"  "green"  "red"    "orange"
  #[17] "red"    "red"    "blue"   "orange"
  #x.col$key
  #[,1]  [,2]   [,3]    [,4]     [,5]
  #ul  "a"   "b"    "c"     "d"      "NA"
  #col "red" "blue" "green" "orange" "grey"


}
