## Assignment 1
# I confirm that the attached is my own work, except where clearly indicated in 
# the text.

# my_rnorm function
# Inputs:
#  n : number of values to return 
#  mean : mean of the values to return (set default as 0)
#  sd : standard deviation of values to retun (set default as 1)
# Outputs:
#  a vector of pseudo-random values from a normal distribution
my_rnorm <- function(n, mean = 0, sd = 1) {
  # Input Checks
  if (class(n) != "numeric" | class(mean) != "numeric" | 
      class(sd) != "numeric") {
    stop("invalid arguments")
  } else if (length(n) > 1 | abs(n - round(n)) > 1e-10 | 
             n <= 0 | length(mean) > 1 | length(sd) > 1 | 
             sd < 0)  {
    stop("invalid arguments")
  } 
  sample <- c()
  for (i in 1:(n / 2 + 1)) {
  A <- runif(1, 0, 1)
  B <- runif(1, 0, 1)
  x1 <- sin(2 * pi * A) * sqrt(-2 * log(B))
  x2 <- cos(2 * pi * A) * sqrt(-2 * log(B))
  s1 <- mean + x1 * sd
  s2 <- mean + x2 * sd
  sample <- c(sample, s1, s2)
  }
  if((n %% 2) == 0) {
    # if n is even, return n-2 deviates 
    sample <- sample[3:length(sample)] 
  } else {
    # if n is odd, return n-1 devaites
    sample <- sample[2:length(sample)]
  }
  return(sample)
}

# my_rchisq function
# Inputs:
#  n : number of values to return
#  df : degrees of freedom of the chisq-distribution (set default as 1)
# Outputs:
#  a vector of pseudo-random chisq-distribution deviates
my_rchisq <- function(n, df = 1) {
  # Input Checks
  if (class(n) != "numeric" | class(df) != "numeric") {
    stop("invalid arguments")
  } else if (length(n) > 1 | abs(n - round(n)) > 1e-10 | 
             n <= 0 | length(df) > 1 | df < 0)  {
    stop("invalid arguments")
  }
  chisq <- rep(0, n)
  for (i in 1:df) {
    z <- my_rnorm(n)
    chisq <- chisq + z ^ 2
  }
  return(chisq)
}

# my_rt function
# Inputs: 
#  n : number of values to return 
#  df : degrees of freedom of the t-distribution (set default as 1)
# Outputs: 
#  a vector of pseudo-random t-distributed deviates
my_rt <- function(n, df = 1) {
  # Input Checks
  if (class(n) != "numeric" | class(df) != "numeric") {
    stop("invalid arguments")
  } else if (length(n) > 1 | abs(n - round(n)) > 1e-10 | 
             n <= 0 | length(df) > 1 | df <= 0)  {
    stop("invalid arguments")
  } 
  z <- my_rnorm(n)
  chisq <- my_rchisq(n, df)
  t <- z / sqrt(chisq / df)
  return(t)
}

# Test functions
# Test my_rnorm function
# Inputs:
#  n : number of values
#  mean : mean of the values (set default as 0)
#  sd : standard deviation of values (set default as 1)
# Outputs:
#  print "PASSED" if outputs matches with expected outputs
#  print "FAILED" if outputs don't match the expected one
test_my_rnorm <- function(n, mean = 0, sd = 1) {
  out <- my_rnorm(n, mean, sd)
  # outputs length correct (handle even and odd)? 
  if (length(out) == n) {
    print("PASSED: output length correct")
  } else {
    print("FAILED: outputs length incorrect")
  }
  # mean correct (need n to be large)? 
  if (mean(out) > mean - 0.1 & mean(out) <= mean + 0.1) {
    print("PASSED: mean correct")
  } else {
    print("FAILED: mean incorrect")
  }
  # sd correct? 
  if (sd(out) > sd - 0.1 & sd(out) <= sd + 0.1) {
    print("PASSED: sd correct")
  } else {
    print("FAILED: sd incorrect")
  }
  # distribution correct?
  real <- rnorm(n, mean, sd)
  if (ks.test(out, real)$p.value > 0.05) {
    print("PASSED: distribution correct")
  } else {
    print("FAILED: distribution incorrect")
  }
}

# Test my_rchisq function
# Inputs:
#  n : number of values
#  df : degrees of freedom of the chisq-distribution (set default as 1)
# Outputs:
#  print "PASSED" if outputs matches with expected outputs
#  print "FAILED" if outputs don't match the expected one
test_my_rchisq <- function(n, df = 1) {
  out <- my_rchisq(n, df)
  # outputs length correct? 
  if (length(out) == n) {
    print("PASSED: output length correct")
  } else {
    print("FAILED: outputs length incorrect")
  }
  # mean correct (need n to be large)? 
  if (mean(out) > df - 0.1 & mean(out) <= df + 0.1) {
    print("PASSED: mean correct")
  } else {
    print("FAILED: mean incorrect")
  }
  # distribution correct?
  real <- rchisq(n, df)
  if (ks.test(out, real)$p.value > 0.05) {
    print("PASSED: distribution correct")
  } else {
    print("FAILED: distribution incorrect")
  }
}

# Test my_rt function
# Inputs:
#  n : number of values
#  df : degrees of freedom of the t-distribution (set default as 1)
# Outputs:
#  print "PASSED" if outputs matches with expected outputs
#  print "FAILED" if outputs don't match the expected one
test_my_rt <- function(n, df = 1) {
  out <- my_rt(n, df)
  # outputs length correct? 
  if (length(out) == n) {
    print("PASSED: output length correct")
  } else {
    print("FAILED: outputs length incorrect")
  }
  # mean correct (need n to be large)? 
  if (mean(out) > -0.1 & mean(out) <= 0.1) {
    print("PASSED: mean correct")
  } else {
    print("FAILED: mean incorrect")
  }
  # distribution correct?
  real <- rt(n, df)
  if (ks.test(out, real)$p.value > 0.05) {
    print("PASSED: distribution correct")
  } else {
    print("FAILED: distribution incorrect")
  }
}

# Test function for checking if they can handling wrong model input 
# Inputs:
#  method : using "my_rnorm", "my_rchisq" or "my_rt"
#  n : number of values 
#  mean : mean of the values (set default as "clever girl")
#  sd : standard derivation of the values (set default as 1)
#  df : degrees of freedom of the distribution (set default as 1)
# Outputs:
#  print "PASSED" if outputs matches with expected outputs
#  print "FAILED" if outputs don't match the expected one
test_errout <- function(method = "my_rnorm", n, mean = "clever girl", 
                        sd = 1, df = 1) {
  options(warn = -1)
  test_output = NULL # create an empty output vector 
  if (method == "my_rnorm") {
    # if some output is returned, it will be assigned to the test_output
    # if no output is returned (stop() function produces an error), 
    # the test_output remains an empty vector 
    try(test_output <- my_rnorm(n, mean, sd), silent = TRUE) 
    # if test_output is empty? (input check has no problem)
    if (is.null(test_output) == TRUE) {
      print("PASSED: well done (my_rnorm)")
    } else {
      print("FAILED: error in input check")
    }
  }
  if (method == "my_rchisq") {
    try(test_output <- my_chisq(n, df), silent = TRUE) 
    if (is.null(test_output) == TRUE) {
      print("PASSED: well done (my_rchisq)")
    } else {
      print("FAILED: error in input check")
    }
  }
  if (method == "my_rt") {
    try(test_output <- my_rt(n, df), silent = TRUE) 
    if (is.null(test_output) == TRUE) {
      print("PASSED: well done (my_rt)")
    } else {
      print("FAILED: error in input check")
    }
  }
}

# Test distributions function (able to test all three)
# Inputs:
#  n : set default as 10000 since we need large sample size to test
#  method : using "my_rnorm", "my_rchisq" or "my_rt"
#  mean : mean of the values (set default as 0)
#  sd : standard derivation of the values (set default as 1)
#  df : degrees of freedom of the distribution (set default as 1)
# Outputs:
#  return "TRUE" if the distribution of our sample matches with expected one
#  (we do not reject H0 that the two samples were drawn from the same distribution  
#  at 5% significant level)
#  return "FALSE" if the distribution of our sample doesn't match the expected one
#  (we reject H0)
test_distri <- function(n = 10000, method = "my_rnorm", mean = 0, sd = 1, df = 1) {
  options(warn = -1)
  if (method == "my_rnorm") {
    x <- my_rnorm(n, mean, sd)
    y <- rnorm(n, mean, sd)
  } else if (method == "my_rchisq") {
    x <- my_rchisq(n, df)
    y <- rchisq(n, df)
  } else if (method == "my_rt") {
    x <- my_rt(n, df)
    y <- rt(n, df)
  } else {
    stop("invalid arguments")
  }
  result <- (ks.test(x, y)$p.value) > 0.05 # compare two models 
  return(result)
}









