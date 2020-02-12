## Week 2 
data('mtcars', package='datasets')
head(mtcars[,c('mpg','disp')])

mtFit = lm(mpg~disp, data=mtcars)
summary(mtFit)$coef


## dnorm: distribution function, parameters: vector of quantiles, mean, sd, log
myLik = function(param) {
  - sum(dnorm(mtcars$mpg,
              mean=param[1] + param[2] * mtcars$disp,
              sd=param[3], log=TRUE))}

myLik(c(1,1,1))

optim(c(10,0.1,1), myLik,
      control = list( parscale = c(1,0.01,1)))$par

mtFit$coef

data('shuttle', package='SMPracticals')
rownames(shuttle) = as.character(rownames(shuttle))
shuttle[1:4,]

shuttle %>%
  ggplot(aes(x = temperature, y = r/m)) +
  geom_point(size = 4)

shuttle$notDamaged = shuttle$m - shuttle$r
shuttle$y = as.matrix(shuttle[,c('r','notDamaged')])
shuttleFit = glm(y ~ temperature + pressure,
                 family=binomial(link='logit'), data=shuttle)
shuttleFit$coef

logLikShuttle = function(param) {
  muLogit = param[1] + param[2] * shuttle$temperature +
    param[3] * shuttle$pressure
  mu = exp(muLogit)/(1+exp(muLogit))
  - sum(dbinom(shuttle$r, size=shuttle$m, prob=mu, log=TRUE))}
optim(c(2,0,0), logLikShuttle,
      control = list( parscale = c(1,0.01,0.01)))$par

quantile(shuttle$temperature)

quantile(shuttle$pressure)

shuttle$temperatureC = shuttle$temperature - 70
shuttle$pressureC = shuttle$pressure - 200
shuttleFit2 = glm(y ~ temperatureC + pressureC, family='binomial', data=shuttle)

install.packages("Pmisc", repos='http://r-forge.r-project.org')
