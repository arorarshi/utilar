\name{my.table}
\alias{my.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Tabulates differrent lengths of vector by overlapping names
}
\description{
  my.table, cross tabs between two vector but first overlaps their names to make sure the cross tab is legit.
}

\usage{
my.table(x,y)
}

\arguments{
  \item{x}{ a vector with \code{names} to tabulate with another vector}
  \item{y}{ a vector with \code{names} to tabulate with another vector}
}

\details{

  my.table, cross tabs between two vector but first overlaps their names to make sure the cross tab is legit. vector with names is required. \code{useNA} is always set to \code{"if any"}. Both arguments are required.

}

\value{
 Returns a table cross tabulating \code{x} and \code{y} with overlapping \code{names}

}

\author{
Arshi Arora
}

\examples{

set.seed(123)
x<-sample(1:4, 20, replace=TRUE)
names(x)=paste0("sample",1:length(x))

y<-sample(1:4, 25, replace=TRUE)
names(y)=paste0("sample",1:length(y))

my.table(x,y)


}
