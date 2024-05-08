#24369 - FM321 Course Project

#Libraries ----
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(timeSeries)
library(tseries)
library(roll)
library(car)
library(MASS)
library(extraDistr)
library(rugarch)
library(rmgarch)
library(BEKKs)
library(QRM)
library(dplyr)
library(rmarkdown)
library(DataExplorer)

#Part I  ----

ENV.YKC <- new.env()
stocks <- c("Microsoft", "Apple", "Pfizer", "Wells Fargo & Co", "Bank of America", "Visa") 
tickers <- c("MSFT", "AAPL", "PFE", "WFC", "BAC", "V") 
tickers_ <- as.vector(sapply(tickers, FUN = function(x) paste(x, '.Adjusted', sep = ''))) 

#An environment is formed and stock price data is imported from Yahoo Finance as the source.

Symbols <- getSymbols(Symbols = tickers, src = 'yahoo', 
                      from = "2010-01-01",
                      to = "2019-12-31",  
                      env = ENV.YKC)

Adjusted_Stock_Prices <- do.call(merge, eapply(env = ENV.YKC, Ad))
Adjusted_Stock_Prices <- Adjusted_Stock_Prices[, tickers_]
names(Adjusted_Stock_Prices) <- stocks
#We extract the adjusted stock prices for the stocks.

log_returns <- diff(log(Adjusted_Stock_Prices)) 
log_returns <- na.omit(log_returns) 

AvgRet <- colMeans(log_returns)
log_returns_demean <- sweep(x = log_returns, MARGIN = 2, STATS = AvgRet)
#We obtain log returns and de-mean them for theoretical and computational purposes. 
View(log_returns)

#To observe our portfolio of stocks within the sampling period,
#Produces Plot 1:
ggplot(data = log_returns_demean, mapping = aes(x = index(log_returns_demean))) +
  ggtitle('Daily Returns of Portfolio of Stocks') +
  xlab('Date') +
  geom_line(aes(y = log_returns_demean[, 'Microsoft'], color = 'Microsoft')) +
  geom_line(aes(y = log_returns_demean[, 'Apple'], color = "Apple")) +
  geom_line(aes(y = log_returns_demean[, 'Pfizer'], color = 'Pfizer')) +
  geom_line(aes(y = log_returns_demean[, 'Wells Fargo & Co'], color = 'Wells Fargo & Co')) +
  geom_line(aes(y = log_returns_demean[, 'Bank of America'], color = 'Bank of America')) +
  geom_line(aes(y = log_returns_demean[, 'Visa'], color = 'Visa')) +
  scale_y_continuous(name = 'Daily returns') + 
  scale_color_manual(name='Daily returns',
                     breaks=c('Microsoft', 'Apple', 'Pfizer', 'Wells Fargo & Co', "Bank of America", "Visa"),
                     values=c('Microsoft'='orange', 'Apple'='grey', 'Pfizer'='green', 'Wells Fargo & Co'='yellow', "Bank of America" = "red", "Visa" ="blue"))

#Forming a weights vector:
portfolio_weight <- c((1/6), (1/6), (1/6), (1/6), (1/6), (1/6))
asset_weights <- tibble(portfolio_weight, tickers) #Produces the Table 1.
print(asset_weights)
sum(asset_weights$portfolio_weight)
#We assign equal weights to each of our securities which constitute the portfolio of stocks.

#Part II ----

#Starting our analysis with descriptive statistics:
AvgRet = colMeans(log_returns)
StdDevRet = colSds(log_returns)
MaxRet = colMaxs(log_returns)
MinRet = colMins(log_returns)
SkewRet = colSkewness(log_returns)
KurtRet = colKurtosis(log_returns)

StatsRet <- as.table(rbind(AvgRet, StdDevRet, MaxRet, MinRet, SkewRet, KurtRet)) #Produces the Table 2.
print(StatsRet)

#To observe the distribution of returns with Gaussian Distribution imposed,
#Produces Plot 2:
par(mfrow=c(2,3)) 
for (i in 1:6) {
  seq_curve <- seq(min(log_returns[, i]), max(log_returns[, i]), length = 100)
  normal_density <- dnorm(x = seq_curve, mean = AvgRet[i], sd = StdDevRet[i])
  hist(x = log_returns[, i], prob = TRUE, breaks = 80, 
       main = stocks[i], col = 'blue', xlab = '')
  lines(seq_curve, normal_density, lwd = 2, col = 'red')
}

#Followed with QQ-plots of returns,
#Produces Plot 3:
par(mfrow=c(1,1))
plot_qq(log_returns) #makes use of the "DataExplorer" package.

#Jarque-Bera Test of Normality, 
crit_val = qchisq(p = 0.95, df = 2)
print(crit_val)

for(j in 1:6) {
  test_stat <- jarque.bera.test(log_returns[, j])
  print(test_stat)
}


#To observe the distribution of returns with Student's t-distribution imposed,
#Produces Plot 4:
par(mfrow=c(2,3))
for (i in 1:6) {
  fit_t <- fitdistr(log_returns[, i], densfun = 't') 
  mu <- fit_t$estimate['m']
  sigma <- fit_t$estimate['s']
  df <- fit_t$estimate['df']
  seq_curve <- seq(min(log_returns[, i]), max(log_returns[, i]), length = 100)
  tscaled_density <- dlst(x = seq_curve, df = df, mu = mu, sigma = sigma)
  hist(x = log_returns[, i], prob = TRUE, breaks = 100, 
       main = stocks[i], col = 'blue', xlab = '')
  lines(x = seq_curve, y = tscaled_density, type = 'l', lwd = 2, col = 'red')
}


#Moving on with the volatility clusters analysis with daily returns,
par(mfrow=(c(2,3)))
for(i in 1:6) {
  acf(x = log_returns[, i], lag.max = 30, 
  main = paste(stocks[i], '- Autocorrelation Function'))
}

#Now observing the squared daily returns,
#Produces Plot 5:
par(mfrow=(c(2,3)))
for(i in 1:6) {
  acf(x = log_returns[, i]^2, lag.max = 30, 
      main = paste(stocks[i], '- ACF of Squared Returns'))
}

par(mfrow=c(1,1)) #to reset our plotting window.


#Ljung-Box Test:
print(paste('Critical Value is:', qchisq(p = 0.95, df = 20, lower.tail=TRUE)))

for (k in 1:6) {
  print(Box.test(x = log_returns[, k]^2, lag = 20, type = "Ljung-Box"))
}

#Correlation Matrix of stocks,
#Produces Plot 6:
plot_correlation(log_returns_demean) #makes use of the "DataExplorer" package.


#Part III ----

#Modelling the factors,
PCA <- prcomp(x = log_returns_demean)
Table_PCA <- rbind(PCA$rotation, summary(PCA)$importance)
print(Table_PCA)

summary(PCA) #Produces Table 3:

#Scree Plot,
#Produces Plot 6:
par(mfrow=c(1,1))
plot(Table_PCA['Proportion of Variance',], type = 'l', lwd = 5, col = 'blue', xlim = c(1,4), main = 'PC proportions of total variance', xlab = 'PC', ylab = 'Proportion of variance', axes = FALSE)
axis(1, 1:6)
axis(2)

#Explainability of Variance provided by Principal Components,
ggplot(data = PCA$rotation, mapping = aes(x = c(1:6))) + 
  geom_line(aes(y = PCA$rotation[, 'PC1']), color = 'red') +
  geom_line(aes(y = PCA$rotation[, 'PC2']), color = 'blue') +
  geom_line(aes(y = PCA$rotation[, 'PC3']), color = 'green') +
  geom_line(aes(y = PCA$rotation[, 'PC4']), color = 'purple') +
  geom_line(aes(y = PCA$rotation[, 'PC5']), color = 'orange') +
  geom_line(aes(y = PCA$rotation[, 'PC6']), color = 'pink') +
  ylab('Weights') + 
  scale_x_continuous(breaks = seq(1, 10),1) +
  annotate(geom="text", x = 3, y = - 0.5, label="PC1", color="red") +
  annotate(geom="text", x = 2, y = 0.3, label="PC2", color="blue") +
  annotate(geom="text", x = 9, y = 0.3, label="PC3", color="green") +
  annotate(geom="text", x = 9, y = 0.3, label="PC4", color="purple") +
  annotate(geom="text", x = 9, y = 0.3, label="PC5", color="orange") +
  annotate(geom="text", x = 9, y = 0.3, label="PC6", color="pink")


f<- PCA$x
num_factors <- 2 #2 principal components are selected, yielding 72% of explanation.

#Fitting multivariate t-GARCH,
tGARCH_1_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), distribution.model = 'std', 
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
tuspec <- multispec(replicate(num_factors, tGARCH_1_1))
tGARCH_multifit <- multifit(multispec = tuspec, data = f[,1:num_factors], solver = 'hybrid')
#To observe our GARCH parameters,
print(tGARCH_multifit) #produces Table 4.

#Now fitting normal multivariate GARCH,
GARCH_1_1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
uspec <- multispec(replicate(num_factors, GARCH_1_1))
GARCH_multifit <- multifit(multispec = uspec, data = f[,1:num_factors], solver = 'hybrid')
print(GARCH_multifit)

#Comparing the fit with GARCH_1_1 and GARCH_1_1t,
#t-GARCH is the unrestricted model. GARCH is nested in t-GARCH,
LR_GARCH = 2 * (likelihood(tGARCH_multifit) - likelihood(GARCH_multifit))
#Multiple likelihood is produced for each factor embedded into the model.
#Having obtained the values as 124.52 and 179.92 assigned to LR_GARCH,
CV <- qchisq(p = 0.95, df = 1, lower.tail=TRUE)
#Obtaining test statistic value that is significantly higher than the critical value, we can clearly state that the H0 is rejected. 

sigma <- sigma(tGARCH_multifit) #the conditional volatility
#I would like to extend the analysis with the Residual Analysis of GARCH(1,1), Diagnostic Test,
eps <- log_returns_demean / as.vector(sigma) #Standardized residuals
eps2 <- eps^2 #Standardized residuals squared

#Residual's QQ-Plot,
#Produces Plot 7:
par(mfrow=c(1,1)) 
qqPlot(as.vector(eps), xlab = 'Standard Normal Quantiles', 
       ylab = 'Quantiles of Input Sample', main = 'QQplot of Residuals vs Standard Normal', 
       envelope = FALSE)

#Jarque-Bera Test of Normality for GARCH Residuals,
JB_eps <- jarque.bera.test(as.vector(eps))
CV <- qchisq(p = 0.95, df = 2, lower.tail=TRUE)

if (JB_eps$statistic > CV) {
  print('H0: LR = 0 is rejected.')
} else {
  print('We cannot reject H0')
}

#Ljung-Box Test for GARCH Residuals,
LB_eps2 <- Box.test(as.vector(eps2), type = "Ljung-Box", lag = 1)
CV <- qchisq(p = 0.95, df = 1, lower.tail=TRUE)

if (LB_eps2$statistic > CV) {
  print('H0: LR = 0 is rejected.')
} else {
  print('We cannot reject H0')
}

#Then forming the Conditional Variance Covariance Matrix,
htMat <- xts(sigma^2, order.by = index(log_returns_demean))
errors <- log_returns_demean - f[, 1:num_factors] %*% t(PCA$rotation[,1:num_factors])
omega <- diag(colMeans(errors^2))
ht <- array(dim = c(length(stocks), length(stocks), dim(log_returns_demean)[1]))

for (i in 1:dim(log_returns_demean)[1]) {
  ht[, , i] <- PCA$rotation[,1:num_factors] %*% diag(as.numeric(htMat[i, ])) %*% t(PCA$rotation[,1:num_factors]) + omega
}
#The conditional covariance matrix, ht, is constructed with O-GARCH. The loop iterates through each observation to form the time-varying conditional covariance matrix.


#Portfolio's conditional volatility is estimated by using the variance-covariance matrix and portfolio weights.
#The last observation is from 2019-12-30. Taking 2019-12-30 as the date t, We should forecast ht for the next 1 day, yielding the date t+1.
last_obs <- as.numeric(as.Date(2019-12-30))
jan_2 <- multiforecast(tGARCH_multifit, n.ahead = 1, data = last_obs)

sigma_jan_2 <- sigma(jan_2) #sigma function extracts conditional volatility

#Given that the weights are equally distributed,
#The variance-covariance matrix for 2 January 2020,
ht_jan_2 <- PCA$rotation[,1:num_factors] %*% diag(as.numeric(sigma_jan_2^2)) %*% t(PCA$rotation[,1:num_factors]) + omega
print(ht_jan_2)

#The multiplication of portfolio weights with the conditional covariance matrix yields the portfolio volatility.
Conditional_Portfolio_Volatility <- sqrt(t(portfolio_weight) %*% diag(ht_jan_2) %*% portfolio_weight)

#conditional portfolio volatility estimate,
print(Conditional_Portfolio_Volatility)

#portfolio variance,
print(Conditional_Portfolio_Volatility^2)



#Part IV ----

#tGARCH_multifit@fit$coef["shape"]
df <- coef(tGARCH_multifit)["shape", ]

#Referring back to summary(PCA) and weighted-average of principal components,
w_PC1 <- 0.56556000/(0.56556000+0.15697000)
w_PC2 <- 0.15697000/(0.56556000+0.15697000)

#Parametric VaR estimate,
VaR_0.05 <- - qt(p = 0.05, df = df) * Conditional_Portfolio_Volatility * sqrt((df - 2) / df)

print(VaR_0.05)

#Combining VaR estimations with respective weights of principal components,
VaR_0.05 <- c(0.0077, 0.00761)
PC_weights <- c(0.7827495, 0.2172505)

#Value-at-Risk estimation for 2 January 2020,
portfolio_VaR <- sum(VaR_0.05 * PC_weights)
print(portfolio_VaR)

#Part V ----

Trading_Days <- length(index(log_returns_demean))

p = 0.05  #Significance level of 0.05 is assigned.
WE <- 20 / p #sample size to compute VaR.

#Obtaining a hit sequence and calculating the number of violations:
#Calculate VaR for each trading day. Rolling window analysis by selecting sample,
portfolio_VaR <- xts(matrix(nrow = Trading_Days, ncol = 1), order.by = index(log_returns_demean))
portfolio_VaR <- rollapply(data = log_returns_demean, width = WE, FUN = function(x) - sort(coredata(x))[20])
na.omit(portfolio_VaR)

WT <- Trading_Days - WE #The estimation window is determined, this window can be expanding or moving. This is the total number of observations we have.
v <- sum(log_returns_demean < -portfolio_VaR, na.rm = TRUE) #number of violations
v0 <- WT - v #number of no violations 
EV <- p * WT # expected number of violations

print(paste('Number of violations is',v)) 
print(paste('Number of no violations is',v0))
print(paste('Expected number of violations is',EV))

VR = v / EV # observed number of violations/expected number of violations 
print(paste('Violation ratio',VR))

#Criteria for VaR forecast evaluation,
if (v > EV) {
  print('You have underforecasted VaR.')
} else {
  print('You have overforecasted VaR.')
}


#Unconditional Coverage Test (Bernoulli Test):

ra <- log_returns_demean[(WE + 1):Trading_Days] #The portion of log_returns_demean that falls outside the rolling window used for calculating VaR.
VaRa <- portfolio_VaR[(WE + 1):Trading_Days]
#starts from the day after the rolling window ends (WE + 1) and extends to the last trading day.


eta <- ra < -VaRa #returns a logical value: as T and F assigned as a dichotomous variable, 1 and 0 respectively.

v1 <- sum(eta) #sums 1s and 0s.
v0 <- length(eta) - v1
picap <- v1 / (v1 + v0) #estimation of probability
#If picap is slightly higher than 0.05, violation ratio is above 1.

a <- ((1 - p)^v0 * p^v1) #likelihood of restricted model imposed.
b <- ((1 - picap)^v0 * picap^v1) #likelihood of unrestricted model that we use to obtain MLE estimates.

#Having obtained the number of violations and no violations, and constructed likelihood, now applying the log-likelihood test, obtaining the test statistic:
LR <- 2 * is.na(log(b / a))
if (LR > qchisq(p = 1 - p, df = 1)) {
  print('null hypothesis H0 is rejected')
} else {
  print('We cannot reject the null')
}
#The script checks if the observed violations exceed the expected violations.


#Conditional Coverage Test (Independence Test):

T <- length(eta)
logical <- matrix(data = 0, nrow = T, ncol = 4)
#as we will have one column for each possible outcome.
  # -i=1, j=1
  # -i=1, j=0
  # -i=0, j=1
  # -i=0, j=0

for (i in 2:T) {
  logical[i,1] <- coredata(eta)[i-1] == 0 & coredata(eta)[i] == 0
  logical[i,2] <- coredata(eta)[i-1] == 0 & coredata(eta)[i] == 1
  logical[i,3] <- coredata(eta)[i-1] == 1 & coredata(eta)[i] == 0
  logical[i,4] <- coredata(eta)[i-1] == 1 & coredata(eta)[i] == 1
}

#To obtain the sum of each 4 possible outcomes,
eta_00 = sum(logical[, 1], na.rm = TRUE)
eta_01 = sum(logical[, 2], na.rm = TRUE)
eta_10 = sum(logical[, 3], na.rm = TRUE)
eta_11 = sum(logical[, 4], na.rm = TRUE)

#Calculating the probabilities,
P_00 = eta_00 / (eta_00 + eta_01)
P_01 = eta_01 / (eta_00 + eta_01)
P_10 = eta_10 / (eta_10 + eta_11)
P_11 = eta_11 / (eta_10 + eta_11)

hat_p = (eta_01 + eta_11) / (eta_00 + eta_01 + eta_10 + eta_11)

b1 = P_00^(eta_00) * P_01^(eta_01) * P_10^(eta_10) * P_11^(eta_11) #unrestricted model
a1 = (1 - hat_p)^(eta_00 + eta_10) * hat_p^(eta_01 + eta_11) #restricted model

LR_ind = 2 * is.na(log(b1 / a1))   

if (LR_ind > qchisq(p = 1 - p, df = 1)) {
  print('Null hypothesis H0 is rejected')
} else {
  print('We cannot reject the null')
}
#Rejection of H0 indicates that the VaR measures are not independent.
 