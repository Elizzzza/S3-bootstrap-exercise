# S3-bootstrap-exercise

#### BIOST 561: Computational Skills for Biostatistics
#### Instructor: Eardi Lila

In this problem, you will construct an S3 method `bootstrap`, for both the class `numeric` and `stratified` with the following interface.

`bootstrap.my_class <- function(object, nboot, stat){... your code here ...}`

The function `bootstrap.my_class` will return the **evaluations** of the statistics (i.e., function) encoded in
the function `stat` on each one of the bootstrapped vectors.

Illustrate the use of your `bootstrap` generic function on objects of the class `numeric` and `stratified` using
the mean, the median, and the standard deviation as the statistics of interest (e.g. make a histogram with
the evaluations of the statistics).


Generalize the methods `bootstrap` defined above to the case of an argument `stat` that is a function
that can take additional arguments, e.g. a function that computes the $k$ th moment. Test it.

`moment <- function(x, k)
{
(1/length(x))*sum((x-mean(x))ˆk)
}`
