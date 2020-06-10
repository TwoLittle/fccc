## fccc - functional concordance correlation coefficient

  We developed an R package, fccc, which is an abbreviation of functional concordance correlation coefficient and distributed it via Github. To install the package, please use the following commands:
  
    >devtools::install_github("TwoLittle/fccc")
    >library(fccc)
The package includes an introduction about the functions that can be used to carry out the conventional and proposed methods, as well as a simple example that demonstrates the usage. To check out the introduction and example, please use the following commands:

    >help(package = 'fccc')
There are two functions in the package:

    get.con.cor(X, Y) #calculates the conventional Pearson’s correlation coefficient and the conventional concordance correlation coefficient.
    get.fun.cor(X, Y, W) #calculates the functional Pearson’s correlation coefficient and the functional concordance correlation coefficient.
    
The input data X and Y should be prepared in a matrix form with each row being a subject and each column being a time point. X and Y should be of the same size. Missing values should be coded as NaN. 
        The function, get.fun.cor(X, Y, W), also allows the user to specify the weight function, W, based on the research context. If the user does not specify the weight function W, get.fun.cor uses equal weights for each time point by default. For example, 
        
    >x = matrix(norm(12), 3, 4)
    >y = matrix(norm(12), 3, 4)
    >get.fun.cor(x, y)
  
Without the weight function, get.fun.cor(x, y)assumes equal weights, which produce exactly the same result as

    >w = matrix(1, 3, 4) # equal weight
    >get.fun.cor(x, y, w)
One can also assign different weights to different time points.
