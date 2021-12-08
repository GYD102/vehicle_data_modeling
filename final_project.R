setwd("D:/stonybrook_coursework/ams572")

## ---- required-packages ----

require(foreign)
require(ggplot2)
require(Hmisc)
require(reshape2)
require(MASS)
require(ordinal)
require(rms)
require(lmtest)
require(tidyverse)
require(mice)


## ---- short-summary-function ----
# Print summary function for long strings of coefficients
# Courtesy of LyzandeR
# https://stackoverflow.com/questions/32989379/print-the-summary-of-an-lm-or-fastlm-model-without-printing-the-coefficients
print.sum2 <-
  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...){
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if (!is.null(x$weights) && diff(range(x$weights))) 
      "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
      nam <- c("Min", "1Q", "Median", "3Q", "Max")
      rq <- if (length(dim(resid)) == 2L) 
        structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                                 dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(quantile(resid), digits + 1L)
        structure(zz, names = nam)
      }
      print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    }
    else {
      cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
      cat("\n")
    }
    if (length(x$aliased) == 0L) {
      #cat("\nNo Coefficients\n")
    }
    else {
      if (nsingular <- df[3L] - df[1L]) {
        #cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", sep = "")
      }
      else {
        #  cat("\nCoefficients:\n")
      }
      coefs <- x$coefficients
      if (!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                                                                colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      #printCoefmat(coefs, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
    }
    cat("\nResidual standard error:", format(signif(x$sigma, 
                                                    digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if (nzchar(mess <- naprint(x$na.action))) 
      cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
      cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
      cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                             digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                         digits = digits), "on", x$fstatistic[2L], "and", 
          x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                            x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                         digits = digits))
      cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
      p <- NCOL(correl)
      if (p > 1L) {
        cat("\nCorrelation of Coefficients:\n")
        if (is.logical(symbolic.cor) && symbolic.cor) {
          print(symnum(correl, abbr.colnames = NULL))
        }
        else {
          correl <- format(round(correl, 2), nsmall = 2, 
                           digits = digits)
          correl[!lower.tri(correl)] <- ""
          print(correl[-1, -p, drop = FALSE], quote = FALSE)
        }
      }
    }
    cat("\n")
    invisible(x)
  }

## ---- data-formatting ----
# Part 0: Importing and Formatting Data
data <- read.csv("imports-85.data.csv")

data$normalized.losses <- as.integer(replace(data$normalized.losses, data$normalized.losses == "?", NA))
data$bore <- as.numeric(replace(data$bore, data$bore == "?", NA))
data$stroke <- as.numeric(replace(data$stroke, data$stroke == "?", NA))
data$horsepower <- as.integer(replace(data$horsepower, data$horsepower == "?", NA))
data$peak.rpm <- as.integer(replace(data$peak.rpm, data$peak.rpm == "?", NA))
data$price <- as.integer(replace(data$price, data$price == "?", NA))

for(i in names(data)){
  data[i] <- replace(data[i], data[i] == "?", NA)
}

to_be_factors <- c("symboling", "make", "fuel.type", "aspiration",
                   "num.of.doors", "body.style", "drive.wheels",
                   "engine.location", "engine.type", "num.of.cylinders",
                   "fuel.system")

for(i in to_be_factors){
  data[,i] <- as.factor(data[,i])
}

## ---- a0 ----

# Part 1: Investigating Variables

# what does "normalized losses" mean?
table(data$body.style)
hist(data$normalized.losses, breaks = 20)
hist(data$normalized.losses, breaks = 64:256)

# The data description indicates that "Station wagon" is a "size classification"
# about which the data is normalized.
ggplot(data, aes(x = body.style, y = normalized.losses)) + geom_boxplot()
ggplot(data, aes(x = body.style:num.of.doors, y = normalized.losses)) + geom_boxplot()

## ---- a ----
# Multiple differences, same symboling and normalized losses
data[174:178,]

# Only difference in below two is drive.wheels
data[154:155,]

## ---- a1 ----
# Does identifying the model predict ties in symboling and normalized losses?
# See Appendix 5.4 for full printout
print.sum2(summary(lm(normalized.losses ~ make : drive.wheels : wheel.base, data = data)))

## ---- a2 ----
print.sum2(summary(lm(normalized.losses ~ make : drive.wheels : as.factor(wheel.base), data = data)))

# Interestingly, considering wheel.base as categorical improves the model (curious aside, nothing else)
# AIC is lower, F-statistic is higher, R^2 is higher
AIC(lm(normalized.losses ~ make : drive.wheels : wheel.base, data = data))
AIC(lm(normalized.losses ~ make : drive.wheels : as.factor(wheel.base), data = data))

## ---- a3 ----

# Normalized losses (and symboling) are assigned on a per-model basis. Though
# the models aren't recorded in the data, they can be infered by the combined
# statistic make:drive.wheels:wheel.base. To this end, it maybe be more useful
# to set wheel.base as a factor with levels (and therefore consider it ordinal 
# categorical) rather than a numeric value.

## ---- collapse-model-function ----

# Collapsing the ties
# Returns a model stripped of ties (repeat observations)
collapse_model <- function(df, keeper_variable = NULL){
  # Initializing data.frame
  out.df <- df[0,]
  for(i in 1:nrow(data)){
    # Interator df
    temp.df <- df[i,]
    # If current row is not the same model as any existing row in output df
    
    # Same model logical vector
    same_model <- out.df$make == temp.df$make & 
      out.df$symboling %in% temp.df$symboling & 
      out.df$normalized.losses %in% temp.df$normalized.losses
    
    if(sum(same_model) == 0){
      # Add row to output df
      out.df <- rbind(out.df, temp.df)
    }
    else if(!is.null(keeper_variable) & sum(! out.df[same_model,][,keeper_variable] %in% temp.df[,keeper_variable]) > 0){
      out.df <- rbind(out.df, temp.df)
    }
  }
  return(out.df)
}

## ---- c ----

# See Appendix 5.5 for collapse_model function code
deflated_data <- collapse_model(data)
head(data)
head(deflated_data)

## ---- d ----
# Part 1 Analysis -- Comparing population means
diesel <- data[data$fuel.type == "diesel",]
gas <- data[data$fuel.type == "gas",]
c(nrow(gas), nrow(diesel))
# Testing normality for diesel (gas has over 100 observations)
shapiro.test(diesel$normalized.losses)
# Reject normality assumption

## ---- e ----

# Look for alternatives
# Box plot for visualization
ggplot(subset(data, !is.na(num.of.doors)), aes(x = num.of.doors, y = normalized.losses)) + geom_boxplot()

## ---- e1 ----
deflated_data_doors <- collapse_model(data, "num.of.doors")

## ---- h ----

# Investigating randomness of missing values
ggplot(data, aes(x = is.na(normalized.losses), y = price)) + geom_boxplot()

## ---- prop-test-function ----

# Setting up a proportion test to prove that random values in normalized losses are MNAR
prop_test_across <- function(df, f, g = "normalized.losses"){
  d <- data.frame(na = integer(), notna = integer())
  
  for(l in levels(df[,f])){
    tt <- table(is.na(df[df[,f] == l,][,g]))
    d <- rbind(d, data.frame(na = tt['TRUE'], notna = tt['FALSE'], row.names = NULL))
  }
  d[is.na(d)] <- 0
  print(t(d))
  prop.test(as.matrix(d))
}

## ---- h2 ----

prop_test_across(deflated_data, "make")
# prop_test_across(data, "make")
# Commented out, no significant difference

## ---- i ----
prop_test_across(deflated_data_doors, "num.of.doors")
# prop_test_across(data, "num.of.doors")
# Commented out, no significant difference

## ---- t-test-functions ----

t_test_assumption_check <- function(v){
  if(length(v) - sum(is.na(v)) < 30){
    s <- shapiro.test(v)
    print(s)
    if(s$p.value < 0.05){
      cat("Assumptions necessary for t.test fail to hold:
            two.door data insufficient and non-normal.\n")
      return(FALSE)
    }
    else{
      cat("Data satisfies normality assumption: Shapiro-Wilk test.\n")
      return(TRUE)
    }
  }
  else{
    cat("Data satisfies normality assumption: n >= 30; CLT.\n")
    cat(paste(c("n = ", length(v) - sum(is.na(v)),"\n"), collapse = ""))
  }
  return(TRUE)
}

t_test_doors <- function(df){
  # First test length of available data
  two.doors <- filter(df, num.of.doors %in% "two")$normalized.losses
  four.doors <- filter(df, num.of.doors %in% "four")$normalized.losses
  
  # For each length of data that is shorter than 30, conduct a normality test
  if(!t_test_assumption_check(two.doors)){
    return(0)
  }
  
  if(!t_test_assumption_check(four.doors)){
    return(0)
  }
  
  # If either normality test fails, return an error
  v.test <- var.test(two.doors, four.doors)
  cat("Testing equal variance assumption...\n")
  cat(c("Variance ratio confidence interval: (",v.test$conf.int[1],",",v.test$conf.int[2],")\n"), collapse = "")
  
  eq.var.assumption <- (v.test$p.value > 0.05)
  
  t <- t.test(two.doors, four.doors, var.equal = eq.var.assumption)
  print(t)
  cat(c("Standard error of difference of means:", t$stderr),"\n")
  invisible(t)
  return(
    data.frame(variance.equality = eq.var.assumption,
               ci.lower = t$conf.int[1],
               ci.upper = t$conf.int[2],
               std.err = t$stderr)
  )
}

full_t_test_doors <- function(df,
                              deflated = c(TRUE, FALSE),
                              randomness = c("mcar", "mnar"),
                              imputation.method = c("none", "mean", "random")){
  return(cbind(
    data.frame(deflated = deflated,
               randomness = randomness,
               imputation.method = imputation.method),
    t_test_doors(df)
  ))
}

## ---- g ----

t_test_result_comparisons <- full_t_test_doors(data, FALSE, "mcar", "none")

## ---- g2 ----

t_test_result_comparisons <- 
  rbind(t_test_result_comparisons,
        full_t_test_doors(deflated_data_doors, TRUE, "mcar", "none"))



## ---- mean-imputation-function ----
# Mean imputation
mean_impute_normalized_losses <- function(df){
  output.df <- df
  output.df$normalized.losses <-
    replace(output.df$normalized.losses,
            is.na(output.df$normalized.losses),
            mean(output.df$normalized.losses, na.rm = TRUE))
  return(output.df)
}

## ---- j ----

data_mean_imputed <- mean_impute_normalized_losses(data)
deflated_data_doors_mean_imputed <- mean_impute_normalized_losses(deflated_data_doors)

t_test_result_comparisons <- 
  rbind(t_test_result_comparisons,
        full_t_test_doors(data_mean_imputed, FALSE, "mcar", "mean"))

t_test_result_comparisons <- 
  rbind(t_test_result_comparisons,
        full_t_test_doors(deflated_data_doors_mean_imputed, TRUE, "mcar", "mean"))

## ---- mnar-simulation ----
mnar_deflated_data_doors <- deflated_data_doors
set.seed(2838940)
rs <- runif(nrow(mnar_deflated_data_doors))
for(i in 1:nrow(mnar_deflated_data_doors)){
  if(mnar_deflated_data_doors[i,]$num.of.doors %in% "four"){
    if(rs[i] <= 0.5){
      mnar_deflated_data_doors[i,]$normalized.losses <- NA
    }
  }
}

prop_test_across(mnar_deflated_data_doors, "num.of.doors")

t_test_result_comparisons <- 
  rbind(t_test_result_comparisons,
        full_t_test_doors(mnar_deflated_data_doors, TRUE, "mnar", "none"))

## ---- mnar2 ----

mnar_deflated_data_doors_mean_imputed <- 
  mean_impute_normalized_losses(mnar_deflated_data_doors)

t_test_result_comparisons <- 
  rbind(t_test_result_comparisons,
        full_t_test_doors(mnar_deflated_data_doors_mean_imputed, TRUE, "mnar", "mean"))

## ---- comparison ----

t_test_result_comparisons

## ---- k1 ----

# Proportion test to check MCAR or MNAR between the two considered variables
prop_test_across(deflated_data, "symboling")
# For comparison to inflated data
prop_test_across(data, "symboling")
# prop_test_across(deflated_data, "make")
# prop_test_across(data, "make")

## ---- k2.1 ----
# Part 2: GLM Symboling

# Analyzing Symboling
ggplot(deflated_data, aes(x = symboling, y = price)) + geom_boxplot()
ggplot(deflated_data, aes(x = symboling, y = normalized.losses)) + geom_boxplot()

ggplot(deflated_data, aes(x = price, y = normalized.losses)) + geom_point()

## ---- k2.2 ----

m2.1 <- clm(symboling ~ normalized.losses, data = deflated_data)
summary(m2.1)

m2.3 <- clm(symboling ~ normalized.losses + price, data = deflated_data)
summary(m2.3)

lrtest(m2.3, m2.1)
# Select m2.3 as primary model

## ---- k3 ----

predicted_prob <- predict(m2.3, deflated_data[,c("normalized.losses", "price")])$fit
most_likely_symbol <- apply(predicted_prob, 1, function(x) which(x == max(x)))
names(most_likely_symbol) <- NULL
is.na(most_likely_symbol) <- lengths(most_likely_symbol) == 0
compare_symboling_fitting <- data.frame(deflated_data$symboling, names(unlist(most_likely_symbol)))

## ---- k3.2 ----
sum(is.na(predicted_prob[,1]))
as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]
sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2])
## ---- k3.3 ----
cat(c("Accuracy = ", sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]) / sum(!is.na(predicted_prob[,1])), "\n"))

## ---- k4.1 ----
deflated_data_mean_imputed <- mean_impute_normalized_losses(deflated_data)

m2.3_mean_imputed <- clm(symboling ~ price + normalized.losses, data = deflated_data_mean_imputed)
summary(m2.3_mean_imputed)

## ---- k4.2 ----

predicted_prob <- predict(m2.3_mean_imputed, deflated_data_mean_imputed[,c("normalized.losses", "price")])$fit
most_likely_symbol <- apply(predicted_prob, 1, function(x) which(x == max(x)))
names(most_likely_symbol) <- NULL
# most_likely_symbol <- most_likely_symbol - 4
is.na(most_likely_symbol) <- lengths(most_likely_symbol) == 0
compare_symboling_fitting <- data.frame(deflated_data_mean_imputed$symboling, names(unlist(most_likely_symbol)))

## ---- k4.3 ----

sum(is.na(predicted_prob[,1]))
compare_symboling_fitting[,1] == compare_symboling_fitting[,2]
sum(compare_symboling_fitting[,1] == compare_symboling_fitting[,2])
## ---- k4.4 ----
cat(c("Accuracy = ", sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]) / sum(!is.na(predicted_prob[,1])), "\n"))

## ---- k5 ----

mnar_deflated_data_symboling <- deflated_data
set.seed(81234)
rs <- runif(nrow(mnar_deflated_data_symboling))
for(i in 1:nrow(mnar_deflated_data_symboling)){
  if(mnar_deflated_data_symboling[i,]$symboling == 3 & rs[i] <= 0.8){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
  if(mnar_deflated_data_symboling[i,]$symboling == 2 & rs[i] <= 0.4){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
  if(mnar_deflated_data_symboling[i,]$symboling == 1 & rs[i] <= 0.2){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
  if(mnar_deflated_data_symboling[i,]$symboling == 0 & rs[i] <= 0.01){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
  if(mnar_deflated_data_symboling[i,]$symboling == -1 & rs[i] <= 0.05){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
  if(mnar_deflated_data_symboling[i,]$symboling == -2 & rs[i] <= 0.025){
    mnar_deflated_data_symboling[i,]$normalized.losses <- NA
  }
}

prop_test_across(mnar_deflated_data_symboling, "symboling")

## ---- k5.2 ----

m2.3_mnar <- clm(symboling ~ price + normalized.losses, data = mnar_deflated_data_symboling)
summary(m2.3_mnar)

## ---- k5.3 ----

predicted_prob <- predict(m2.3_mnar, mnar_deflated_data_symboling[,c("normalized.losses", "price")])$fit
most_likely_symbol <- apply(predicted_prob, 1, function(x) which(x == max(x)))
names(most_likely_symbol) <- NULL
is.na(most_likely_symbol) <- lengths(most_likely_symbol) == 0
compare_symboling_fitting <- data.frame(mnar_deflated_data_symboling$symboling, names(unlist(most_likely_symbol)))

## ---- k5.4 ----
sum(is.na(predicted_prob[,1]))
as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]
sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2])
## ---- k5.5 ----
cat(c("Accuracy = ", sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]) / sum(!is.na(predicted_prob[,1])), "\n"))
## ---- k6 ----

mnar_deflated_data_symboling_mean_imputed <- mean_impute_normalized_losses(mnar_deflated_data_symboling)
m2.3_mnar_mean_imputed <- clm(symboling ~ price + normalized.losses, data = mnar_deflated_data_symboling_mean_imputed)
summary(m2.3_mnar_mean_imputed)

## ---- k6.2 ----

predicted_prob <- predict(m2.3_mnar_mean_imputed, mnar_deflated_data_symboling_mean_imputed[,c("normalized.losses", "price")])$fit
most_likely_symbol <- apply(predicted_prob, 1, function(x) which(x == max(x)))
names(most_likely_symbol) <- NULL
is.na(most_likely_symbol) <- lengths(most_likely_symbol) == 0
compare_symboling_fitting <- data.frame(mnar_deflated_data_symboling_mean_imputed$symboling, names(unlist(most_likely_symbol)))

## ---- k6.3 ----
sum(is.na(predicted_prob[,1]))
compare_symboling_fitting[,1] == compare_symboling_fitting[,2]
sum(compare_symboling_fitting[,1] == compare_symboling_fitting[,2])

## ---- k6.4 ----
cat(c("Accuracy = ", sum(as.character(compare_symboling_fitting[,1]) == compare_symboling_fitting[,2]) / sum(!is.na(predicted_prob[,1])), "\n"))
## ---- k7 ----

## ---- appendix5.4 ----
summary(lm(normalized.losses ~ make : drive.wheels : wheel.base, data = data))
summary(lm(normalized.losses ~ make : drive.wheels : as.factor(wheel.base), data = data))

## ---- appendix5.6 ----
summary(data)
summary(deflated_data)

## ---- appendix5.10 ----
m2 <- clm(symboling ~ price, data = deflated_data)
summary(m2)

m2.2 <- clm(symboling ~ normalized.losses:price, data = deflated_data)
summary(m2.2)

m2.4 <- clm(symboling ~ normalized.losses + normalized.losses:price, data = deflated_data)
summary(m2.4)

m2.5 <- clm(symboling ~ normalized.losses*price, data = deflated_data)
summary(m2.5)