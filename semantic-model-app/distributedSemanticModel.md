---
title: "DistributedSemanticModel-Springer"
author: "Mukund"
date: "May 20, 2019"
output: html_document
---

Loading Libraries 


```r
library(data.table) 
library(wordspace)
```

## Importing dataset and Creating DSM 


```r
# loading keywords dataset 

Keyword_dt <- read.csv("C:/Users/ecom2/Desktop/springerNature/Keyword_dataset.csv", stringsAsFactors = FALSE)

#we use dsm() constructor to create vector object -- NOUNS as targets and VERBS as features
Vector.Obj <- dsm(target=Keyword_dt$KW1, feature=Keyword_dt$KW2, score= Keyword_dt$Frequency.of.Co.occurance,
                  raw.freq=TRUE, sort=TRUE)

# to have look at 6 most frequent nouns from the dataset
subset(Vector.Obj$rows, rank(-f) <= 6)
```

```
##                            term nnzero     f
## 89  addiction & substance abuse     72  1292
## 216                angiogenesis     99  1371
## 283                   apoptosis    561 12008
## 484               breast cancer    222  3097
## 523                      cancer    131  1547
## 954                    dopamine     86  1437
```

```r
# to have look at sparse matrix -- only 5 rows are being displayed since the actual sparse matrix is very big 
head(Vector.Obj)
```

```
## 6 x 6 sparse Matrix of class "dgCMatrix"
##                                1 4 diethynylbenzene 12 15 16 18 19
## [11c]raclopride                                   .  .  .  .  .  .
## 1                                                 .  .  .  . 29 16
## 1 4 benzenedithiol                                5  .  .  .  .  .
## 1 butadienyloxytrimethylsilane                    .  .  .  .  .  .
## 10                                                .  7 10  . 23  7
## 11)                                               .  .  .  .  .  .
```

```r
VObj <- Vector.Obj 
```

## The DSM parameters 


**step 01** : Limiting non-zero entries in the sparse matrix  

Rows and columns with few nonzero cells provide unreliable semantic information and can lead to numerical problems (e.g. because a sparse association score deletes the remaining nonzero entries). It is therefore common to apply frequency thresholds both on rows and columns, here in the form of requiring at least 3 nonzero cells. The option recursive=TRUE guarantees that both criteria are satisfied by the final DSM when rows and columns are filtered at the same time


```r
# nnzero stands for non-zero entries in rows and columns 
# we apply frequency threshold on rows and columns, requiring at least nonzero cells 
VObj <- subset(VObj, nnzero >= 3, nnzero >= 3, recursive=TRUE)
dim(VObj)
```

```
## [1] 567 650
```

## Association Measures 

**step 02** : The next step is to weight co-occurrence frequency counts. 
The built-in methods for Association Measures are as follows :- 
        - MI (pointwise) : Mutual Information 
        - simple-ll  : simple log-likelihood 
        - t-score 
        - z-score 
        - Dice coeffieicnt 
        - tf.idf
        - reweight 

Here, we use the simple *log-likelihood association measure* with an additional logarithmic transformation(simple-11), which has shown good results in evaluation studies. We also Normalize the weighted row vectors to unit Euclidean length (normalize=TRUE)


```r
VObj <- dsm.score(VObj, score="simple-ll", transform="log", normalize=TRUE, method="euclidean")

# Printing a DSM object shows information about the dimensions of the co-occurrence matrix and whether it has already been scored

VObj 
```

```
## Distributional Semantic Model with 567 rows x 650 columns
## * raw co-occurrence matrix M available
##   - sparse matrix with 5682 / 368.6k nonzero entries (fill rate = 1.54%)
##   - in canonical format
##   - known to be non-negative
##   - sample size of underlying corpus: 112.7k tokens
## * scored matrix S available
##   - sparse matrix with 5585 / 368.6k nonzero entries (fill rate = 1.52%)
##   - in canonical format
##   - known to be non-negative
```
##Dimensionality Reduction 

Most distributional models apply a dimensionality reduction technique to make data sets more manageable and to refine the semantic representations. A widely-used technique is singular value decomposition (SVD). Since VObj is a sparse matrix, dsm.projection automatically applies an efficient algorithm from the sparsesvd package.


```r
VObj300 <- dsm.projection(VObj, method="svd", n=300)
dim(VObj300)
```

```
## [1] 567 300
```


VObj300 is a dense matrix with 300 columns, giving the coordinates of the target terms in 300 latent dimensions. Its attribute "R2" shows what proportion of information from the original matrix is captured by each latent dimension.


```r
par(mar=c(4,4,1,1))
plot(attr(VObj300, "R2"), type="h", xlab="latent dimension", ylab="R2")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## Cosine Similarity 

The primary goal of a DSM is todetermine "semantic" distances between pairs of words. The arguments to "pair.distances" can also be parallel vectors in order to computwe distances for a large number of words efficiently. 
Higher Cosine value implies higher similarity between terms. 



```r
# we can compare cosine similarity between words "diabates" and "blood pressure"
pair.distances('diabetes', 'blood pressure', VObj50 ,method='cosine', convert = FALSE) 
```

```
## Error in find.canonical.matrix(M): object 'VObj50' not found
```

```r
# 0.86 #?? = 30.68 deg

# we can compare cosine similarity between words "diabates" and "atherosclerosis"
pair.distances('diabetes', 'atherosclerosis', VObj50 ,method='cosine', convert = FALSE)
```

```
## Error in find.canonical.matrix(M): object 'VObj50' not found
```

```r
# 0.79 #?? = 37.68 deg 

# we can se that the angle(??) between first pair is lesser than the second pair. 
# this is also visible in the plot we obtain at the end. 
```

## Finding nearest neighbours 

We are often interested in finding the nearest neighbours of a given term in the DSM space, 
The return value is actually a vector of distances to the nearest neighbours, labelled with the corresponding terms


```r
# lets find the nearest neighbours for "diabetes"
nearest.neighbours(VObj300, "diabetes", n=14, convert=FALSE)   #!reduced space 
```

```
##           diabetes mellitus cardiovascular risk factors 
##                   0.6958462                   0.5679742 
##                hypertension                dyslipidemia 
##                   0.5643415                   0.5582965 
##      cardiovascular disease              blood pressure 
##                   0.5482932                   0.5358622 
##              adipose tissue         cardiovascular risk 
##                   0.5351766                   0.5141838 
##     endothelial dysfunction                     glucose 
##                   0.4586405                   0.4377825 
##                 adiponectin             body mass index 
##                   0.4330155                   0.4253370 
##                        diet                         bmi 
##                   0.4232298                   0.4221338
```


In above code chunk we have generated nearest neighbours in reduced space of vector object with 300 columns .i.e. VObj300 

let's generate nearest neighbours for unreduced space - .i.e. VObj 


```r
nn <- nearest.neighbours(VObj, "diabetes", n=14, convert=FALSE) #! unreduced space 
names(nn)
```

```
##  [1] "diabetes mellitus"           "cardiovascular risk factors"
##  [3] "dyslipidemia"                "hypertension"               
##  [5] "cardiovascular disease"      "blood pressure"             
##  [7] "adipose tissue"              "cardiovascular risk"        
##  [9] "endothelial dysfunction"     "adiponectin"                
## [11] "glucose"                     "bmi"                        
## [13] "diet"                        "body mass index"
```

## neighbourhood plot 

The neighbourhood plot visualizes nearest neighbours as a semantic network based on their mutual distances. This often helps interpretation by grouping related neighbours. The network below shows that book as a text type is similar to novel, essay, poem and article; as a form of document it is similar to paper, letter and document; and as a publication it is similar to leaflet, magazine and newspaper.



```r
nn.mat <- nearest.neighbours(VObj300, "diabetes", n=10, dist.matrix=TRUE)
par(mar=c(1,1,1,1))
plot(nn.mat)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
# nn.mat
# can we transform, this matrix above to disply first column or first row 
```


```r
nearest.neighbours(VObj300, 'diabetes' + 'hypertension', n=10 ) 
```

```
## Error in "diabetes" + "hypertension": non-numeric argument to binary operator
```
