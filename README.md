# utilar
utilArshi suite of functions 

# Install 

Packages required - `coin` , `plyr` and `survival`

```{r}
library(devtools)
install_github("arorarshi/utilar")
```

# Functions and Usage 

For more details see the man pages of `utilar` package. 

## replaceNAs
Given a vector (numeric, character, double, factor) with missingness coded as not \code{NA}, replace it as \code{NA}. Some examples are - 9 N/A Unknown, Not Available etc.

Usage - `replaceNAs(x, vNA)`

```{r}
#example 
set.seed(123)
  #sample 20 numbers from 1 -10, say we want to replace the 9s to NA
  x<-sample(1:10, 20, replace=TRUE)
  #x
  #[1]  3  8  5  9 10  1  6  9  6  5 10  5  7  6  2  9  3  1  4 10

  x.na<-replaceNAs(x,9)
  #[1]  3  8  5 NA 10  1  6 NA  6  5 10  5  7  6  2 NA  3  1  4 10
```
## my.table 
my.table, cross tabs between two vector but first overlaps their names to make sure the cross tab is legit.
Usage - `my.table(x,y)`

```{r}
#example
set.seed(123)
x<-sample(1:4, 20, replace=T)
names(x)=paste0("sample",1:length(x))

y<-sample(1:4, 25, replace=T)
names(y)=paste0("sample",1:length(y))

my.table(x,y)
```

## get.colvector 
Given a vector of class labels, return a color vector correponding to the unique class values. There is an option to set a different color for `NA`. A vector with corresponding color values and a key specifying color value of each class label is returned.

Usage - `get.colvector(labels, col, NA.flag=FALSE, NA>col="grey")`

```{r}
#example
set.seed(123)
  x<-sample(letters[1:4], 20, replace=T)
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

```
## get.sumamry
A vector based summary function. Provides a table with percentages for dicrete data. And median, mean, range or standard deviation for continuous data.

usage `get.summary(var, type)`

```{r}
#example
#when type=1, data is discrete
set.seed(123)
x<-sample(1:3,50, replace=T)
get.summary(x,type=1)
#        1         2         3
#"17(34%)" "15(30%)" "18(36%)"

#lets see with NAs
x[c(1,5,30)] = NA
get.summary(x,type=1)
#        1         2         3      <NA>
#"15(30%)" "15(30%)" "17(34%)"   "3(6%)"

#when type=2, data is continuous
set.seed(123)
x<-rnorm(50)
get.summary(x, type=2, type2="range")
#[1] "-0.07(-1.97-2.17); NA=0"

x[c(1,5,30)] = NA
#get.summary(x, type=2, type2="sd")
#[1] "-0.08(0.93); NA=3"


```
## get.summary2


# References 


# Contact 
Arshi Arora
arshiaurora@gmail.com
