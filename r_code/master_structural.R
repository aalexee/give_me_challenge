# packages ----------------------------------------------------------------

library(plyr)
library(tidyverse)
library(reshape2)

library(doParallel)
library(foreach)

library(numDeriv)
library(pracma)

library(minpack.lm)

library(car)
library(MASS)
library(boot)

library(stargazer)
library(extrafont)

library(tikzDevice)

# functions ---------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# square
# sqr <- function(x) x^2

# comparative statics graphs
comp.stat.graph <- function(U, Bet){

  require(magrittr)
  require(ggplot2)

  A <- seq(0,1,0.0002)
  Theta <- seq(0,1, length.out = 50)
  Z <- seq(2,4, length.out = 50)
  W <- seq(1,2, length.out = 50)
  K <- seq(1,2, length.out = 50)

  data.frame(x = Theta,
             y = A[tapply(U(a = rep(A,length(Theta)), delt = data.frame(theta=rep(Theta, each = length(A)),w=1,z=2,k=1), bet = Bet), rep(1:length(Theta), each = length(A)), FUN = which.max)],
             var = 'theta') %>%
    rbind(.,
          data.frame(x = Z,
                     y = A[tapply(U(a = rep(A,length(Z)), delt = data.frame(theta=0.5,w=1,z=rep(Z, each = length(A)),k=1), bet = Bet), rep(1:length(Z), each = length(A)), FUN = which.max)],
                     var = 'z')) %>%
    rbind(.,
          data.frame(x = W,
                     y = A[tapply(U(a = rep(A,length(W)), delt = data.frame(theta=0.5,w=rep(W, each = length(A)),z=2,k=1), bet = Bet), rep(1:length(W), each = length(A)), FUN = which.max)],
                     var = 'w')) %>%
    rbind(.,
          data.frame(x = K,
                     y = A[tapply(U(a = rep(A,length(K)), delt = data.frame(theta=0.5,w=2,z=2,k=rep(K, each = length(A))), bet = Bet), rep(1:length(K), each = length(A)), FUN = which.max)],
                     var = 'k')) %>%
    ggplot(aes(x, y)) +
    geom_line() +
    facet_wrap(~var, scales = "free")
}

# plots actual and predicted comparative statics
comp.stat.compare <- function(df, fitted){
  df.sim <- df
  df.sim$e.pred <- fitted
  df.sim$e <- df$e
  df.sim$theta <- factor(df.sim$theta)
  df.sim$w <- factor(df.sim$w)
  df.sim$z <- factor(df.sim$z)
  df.sim$k <- factor(df.sim$k)

  ddply(df.sim, .(theta), summarise, actual = mean(e), predicted = mean(e.pred)) %>%
    melt(., measure.vars = c('actual', 'predicted'), variable.name = 'effort', value.name = 'val') %>%
    melt(., measure.vars = c('theta')) %>%
    rbind(.,
          ddply(subset(df.sim, k == 1), .(w), summarise, actual = mean(e), predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('actual', 'predicted'), variable.name = 'effort', value.name = 'val') %>%
            melt(., measure.vars = c('w'))
    ) %>%
    rbind(.,
          ddply(df.sim, .(z), summarise, actual = mean(e), predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('actual', 'predicted'), variable.name = 'effort', value.name = 'val') %>%
            melt(., measure.vars = c('z'))
    ) %>%
    rbind(.,
          ddply(subset(df.sim, w == 2), .(k), summarise, actual = mean(e), predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('actual', 'predicted'), variable.name = 'effort', value.name = 'val') %>%
            melt(., measure.vars = c('k'))
    ) %>%
    ggplot(aes(x = value, y = val, fill = effort)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = palette3) +
    facet_wrap(~variable, scales = "free_x")

}

# numerical optimal effort
e.star <- function(par, data) {

  X <- data[,3:6]
  e.pred <- vector(length = length(data$e))
  e.pred <- foreach (i = 1:length(e.pred), .combine = c) %dopar% {
    optimize(U, c(0,1), delt = X[i,], bet = par, maximum = T)$maximum
  }

  return(e.pred)

}

# numerical optimal effort for the use in nls
e.star.nls <- function(U, ..., theta, w, z, k) {

  X <- data.frame(theta, w, z, k)
  par <- c(...)

  e.pred <- vector(length = length(theta))
  e.pred <- foreach(i = 1:length(e.pred), .combine = c) %dopar% {
    optimize(U, c(0,1), delt = X[i,], bet = par, maximum = T)$maximum
  }

  return(e.pred)

}


# data --------------------------------------------------------------------

df0 <- read_csv("../data/data_effort.csv")
df0$X1 <- NULL
names(df0)[names(df0) == 'effort'] <- 'e'
df <- subset(df0, select = c("id", "e", "theta", "w", "z", "k"))
df1 <- subset(df0, select = c("id", "round", "e", "theta", "w", "z", "k"))

# import demog data
df.demog <- read_csv("../data/data_demog.csv")
df.demog$X1 <- NULL

# merged data set
df.merge <- merge(df1, df.demog, by = "id")


# globals -----------------------------------------------------------------

# palettes
palette <- c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0', '#666666', '#31a354')
palette3 <- c('#41b6c4','#e31a1c') # blue-red
palette4 <- c("#252525", '#41b6c4','#e31a1c') # black-blue-red
palette5 <- c('#e31a1c', '#41b6c4','#c2e699') # red-blue-green
palette6 <- c("#252525", '#e31a1c', '#41b6c4','#c2e699') # black-red-blue-green

# font sizes
fs1 <- 20
fs2 <- 8

# lines
linesizz <- 0.75

# choice set
A <- seq(0,1,0.01)

# significance level
conf.level <- 0.95
crit <- qnorm((1 + conf.level)/2)

# significant digits in presentation
signif1 <- 3

# number of subjects
N <- max(df$id)

# number of observations
nobs <- dim(df)[1]

# set plot theme
theme_set(theme_classic(base_size = 9))
theme_update(
  legend.position = "right",
  plot.title = element_text(hjust = 0.5),
  text = element_text(family = "Verdana")
)


# RDU model ---------------------------------------------------------------

U.RDU.1 <- function(a, delt, bet){

  prob <- function(a, theta) (a + 1 - theta)/2
  cost <- function(a, k) k*a^2
  omega <- function(p, alpha,psi) exp(-psi*(-log(p))^alpha)
  v <- function(y, gam) {
    if (gam != 1) (y^(1 - gam) - 1)/(1 - gam)
    else log(y)
  }
  u <- function(y,a, gam,k) v(y - cost(a, k), gam)

  s = 10 # scaling

  delt = as.matrix(delt)
  theta = delt[,1]
  w = s*delt[,2]
  z = s*delt[,3]
  k = s*delt[,4]

  gam = bet[1]
  alpha = bet[2]
  psi = bet[3]
  # psi = 1

  res = omega(prob(a, theta), alpha, psi) * u(w + z, a, gam, k) + (1 - omega(prob(a, theta), alpha, psi)) *
    u(w, a, gam, k)
  return(as.vector(res))
}


# EU model ----------------------------------------------------------------

U.EU.1 <- function(a, delt, bet){

  prob <- function(a, theta) (a + 1 - theta)/2
  cost <- function(a, k) k*a^2
  v <- function(y, gam) {
    if (gam != 1) (y^(1 - gam) - 1)/(1 - gam)
    else log(y)
  }
  u <- function(y,a, gam,k) v(y - cost(a, k), gam)

  s = 10 # scaling

  delt = as.matrix(delt)
  theta = delt[,1]
  w = s*delt[,2]
  z = s*delt[,3]
  k = s*delt[,4]

  gam = bet[1]

  res = prob(a, theta) * u(w + z, a, gam, k)
  + (1 - prob(a, theta)) * u(w, a, gam, k)
  return(as.vector(res))
}


# NLS estimation ----------------------------------------------------------


# > RDU ---------------------------------------------------------------------


# define model
U <- U.RDU.1

# estimate the model
cl <- makeCluster(detectCores())
clusterExport(cl, list('U'))
registerDoParallel(cl)
time.start <- Sys.time()
reg.nls.rdu.1 <-
  nlsLM(
    e ~ e.star.nls(U = U, gamma, alpha, psi, theta = theta, w = w, z = z, k = k),
    data = df,
    start = list(gamma = 0, alpha = 1, psi = 1),
    trace = T
  )
Sys.time() - time.start
stopCluster(cl)


# > EU ---------------------------------------------------------------------


# define model
U <- U.EU.1

cl <- makeCluster(detectCores())
clusterExport(cl, list('U'))
registerDoParallel(cl)
reg.nls.eu.1 <-
  nlsLM(
    e ~ e.star.nls(U = U, gamma, theta = theta, w = w, z = z, k = k),
    data = df,
    start = list(gamma = 0),
    trace = T
  )
stopCluster(cl)


# Table 4 ----------------------------------------------------------


# show result
res.nls.1 <- round(cbind(
  'Estimate' = summary(reg.nls.rdu.1)$coefficients[,1],
  'SE' = summary(reg.nls.rdu.1)$coefficients[,2],
  '2.5%' = summary(reg.nls.rdu.1)$coefficients[,1] - crit * summary(reg.nls.rdu.1)$coefficients[,2],
  '97.5%' = summary(reg.nls.rdu.1)$coefficients[,1] + crit * summary(reg.nls.rdu.1)$coefficients[,2]
),
4)

row.names(res.nls.1) <- c("gamma", "alpha", "psi")

res.nls.1


# Figure 6 -------------------------------------------------------------------

comp.stat.compare <- function(df, fitted){

  df.sim <- df
  df.sim$e.pred <- fitted
  df.sim$e <- df$e
  df.sim$theta <- factor(df.sim$theta)
  df.sim$w <- factor(df.sim$w)
  df.sim$z <- factor(df.sim$z)
  df.sim$k <- factor(df.sim$k)

  df.temp <- ddply(df.sim, .(theta), summarise, Actual = mean(e), Predicted = mean(e.pred)) %>%
    melt(., measure.vars = c('Actual', 'Predicted'), variable.name = 'Effort', value.name = 'val') %>%
    melt(., measure.vars = c('theta')) %>%
    rbind(.,
          ddply(subset(df.sim, k == 1), .(w), summarise, Actual = mean(e), Predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('Actual', 'Predicted'), variable.name = 'Effort', value.name = 'val') %>%
            melt(., measure.vars = c('w'))
    ) %>%
    rbind(.,
          ddply(df.sim, .(z), summarise, Actual = mean(e), Predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('Actual', 'Predicted'), variable.name = 'Effort', value.name = 'val') %>%
            melt(., measure.vars = c('z'))
    ) %>%
    rbind(.,
          ddply(subset(df.sim, w == 2), .(k), summarise, Actual = mean(e), Predicted = mean(e.pred)) %>%
            melt(., measure.vars = c('Actual', 'Predicted'), variable.name = 'Effort', value.name = 'val') %>%
            melt(., measure.vars = c('k'))
    )
  levels(df.temp$variable) <- c("Difficulty", "Fixed revenue", "Excess revenue", "Cost")

  ggplot(df.temp, aes(x = value, y = val, fill = Effort)) +
    geom_bar(stat="identity", position = position_dodge(), width=.66, color = 'black', alpha=0.9) +
    facet_wrap(~variable, scales = "free_x", ncol = 2) +
    scale_fill_manual(values = palette3) +
    xlab("Value of Treatment Variable") +
    ylab("Mean Effort") +
    theme(legend.position = "top")
}

plot.good.fit <- comp.stat.compare(df, fitted = fitted(reg.nls.rdu.1))
plot.good.fit


# Figure D.4 -------------------------------------------------------------------


plot.good.fit.eu <- comp.stat.compare(df, fitted = fitted(reg.nls.eu.1))
plot.good.fit.eu


# Figure 5 ---------------------------------------------------

omega <- function(p, alpha,psi) {
  as.numeric(exp(-psi*(-log(p))^alpha))
}

omega(
  0.01, coef(reg.nls.rdu.1)["alpha"], coef(reg.nls.rdu.1)["psi"]
)*100

plot1 <- ggplot(
  data = NULL,
  aes(x = seq(0,1,0.01), y = omega(seq(0,1,0.01), coef(reg.nls.rdu.1)["alpha"], coef(reg.nls.rdu.1)["psi"]))
) +
  geom_line(size = linesizz) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(
    x = "$p$",
    y = "$\\omega(p)$",
    title = "Probability Weighting Function")
plot1


# create decision weights for equi-probably lottery
dec.weights.f <- function(numprizes, alpha, psi){

  # probs of each prize
  probs <- rep(1/numprizes, numprizes)

  # cum probs
  cum.probs <- cumsum(probs)

  # weighted cum probs
  cum.probs.w <- omega(cum.probs, alpha, psi)

  # decision weights
  dec.weights <- c(cum.probs.w[1],diff(cum.probs.w))

  # return decision weights from worst to best
  return(rev(dec.weights))
}

# function to be used in graphing, creates data frames
dec.weights.f1 <- function(numprizes, alpha, psi){

  prizes <- seq(1,numprizes,1)
  probs <- rep(1/numprizes, numprizes)
  dec.weights <- dec.weights.f(numprizes, alpha, psi)

  return(data.frame(Prizes = prizes, Probs = probs, Weights = dec.weights, Nprizes = numprizes))

}

# create data frames with varying number of prizes
list1 <- list()
for (i in 2:4){
  # list1[[i-1]] <- dec.weights.f1(numprizes = i, alpha = 0.87, psi = 1.02)
  list1[[i-1]] <- dec.weights.f1(numprizes = i,
                                 alpha = coef(reg.nls.rdu.1)["alpha"],
                                 psi = coef(reg.nls.rdu.1)["psi"]
  )
}
df.weights <- do.call(rbind, list1)
df.weights <- melt(df.weights, measure.vars = c("Weights", "Probs"))
df.weights$Nprizes <- as.factor(df.weights$Nprizes)

plot1 <- ggplot(data = df.weights, aes(x = Prizes, y = value, color = Nprizes)) +
  geom_line(size = linesizz, aes(linetype = variable)) +
  geom_point(size = 2*linesizz) +
  labs(
    x = "Prizes (Worst to Best)",
    y = NULL,
    title = "Decision Weights") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = palette5,
                     # labels = c("2", "3"),
                     name = "N Prizes") +
  scale_linetype_manual(
    values = c(1,3),
    labels = c("RDU","EUT"),
    name = "Model"
  )
plot1
