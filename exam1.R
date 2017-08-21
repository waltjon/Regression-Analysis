setwd("C:/Users/Jonathan/Desktop/STAT/Regression/Exam 1")

season2013 = read.table("season2013.txt", header = T)
season2013.lm = lm(formula = WinPct ~ ERA + BB + RBI + Slug + WSMger, data = season2013)
summary(season2013.lm)
summary(lm(formula = WinPct ~ ERA + BB + RBI + Slug + WSMger, data = season2013))


DBH = rnorm(n= 25, mean = 5.37, sd = 3.16)
anova(DBH)

factory = read.table("factory.txt", header = T)
factory.lm = lm(formula = Performance ~ poly(Temperature, 2), data = factory)
plot(factory$Temperature, factory$Performance)
summary(factory.lm)
t.test(factory$Temperature, factory$Performance)


faux = as.matrix(read.delim("faux.txt", header = T))
hat = faux%*%solve(t(faux)%*%faux)%*%t(faux)
sum(diag(hat))
max(diag(hat))