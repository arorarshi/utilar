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
## get.summary
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
Sumamrize and provide p-value after association test of 2 or more variables. Percentages are reported for discrete variables. Users can choose the type of test they want. For continuous variable median and range are reported if the test specifid is non parametric, mean and standard deviation are reported for a parametric test.  For discrete categories we perform Fisher's Exact Test, for continous following options are available - parametric - Students' T-Test or one way ANNOVA, for non-paramteric Wilcoxon Rank sum test or Kruskal Wallis test.

usage - `get.summary2(fac, var, type)`

```{r}
#example
#two categorical variables
set.seed(100)
fac<-sample(1:2, 50, replace=T)
var<-sample(1:3, 50, replace=T)
get.summary2(fac,var, type=1,var.n="Example, Type=1" )

#Performing Fisher's Exact test
```

|                |1(n=23;46%) |2(n=27;54%) |RowTotal |pval   |
|:---------------|:-----------|:-----------|:--------|:------|
|Example, Type=1 |            |            |         |0.5971 |
|1               |9(39%)      |8(30%)      |17(34%)  |       |
|2               |7(30%)      |12(44%)     |19(38%)  |       |
|3               |7(30%)      |7(26%)      |14(28%)  |       |

```{r}
#continuous with two categories of fac
var<-rnorm(50)
get.summary2(fac, var,  type=2, test.type="p", var.n="2-Way, test.type=p")

#Performing T-Test
```

|                   |1           |2           |RowTotal |pval  |
|:------------------|:-----------|:-----------|:--------|:-----|
|2-Way, test.type=p |-0.11(1.42) |-0.05(0.99) |         |0.855 |
|NA                 |0           |0           |         |      |

```{r}
get.summary2(fac, var,  type=2, test.type="np", var.n="2-Way, test.type=np")
#Performing Wilcoxon Rank Sum test
```

|                    |1                |2                 |RowTotal |pval  |
|:-------------------|:----------------|:-----------------|:--------|:-----|
|2-Way, test.type=np |-0.1[-2.27-2.58] |-0.07[-1.93-1.73] |         |0.676 |
|NA                  |0                |0                 |         |      |


```{r}
fac<-sample(1:3, 50, replace=T)
get.summary2(fac, var,  type=2, test.type="p",var.n="Example, test.type=p")
#Performing one way ANOVA
```

|                     |1           |2          |3         |RowTotal |pval  |
|:--------------------|:-----------|:----------|:---------|:--------|:-----|
|Example, test.type=p |-0.46(1.31) |-0.16(0.9) |0.38(1.3) |         |0.132 |
|NA                   |0           |0          |0         |         |      |

```{r}
get.summary2(fac, var,  type=2, test.type="np",var.n="Example, test.type=np")

#Performing Kruskal-Wallis test
```
|                      |1                 |2                 |3                |RowTotal |pval  |
|:---------------------|:-----------------|:-----------------|:----------------|:--------|:-----|
|Example, test.type=np |-0.53[-2.27-2.58] |-0.07[-1.93-1.82] |0.42[-1.74-2.45] |         |0.131 |
|NA                    |2.58              |1.82              |2.45             |         |      |


# References 


# Contact 
Arshi Arora
arshiaurora@gmail.com
