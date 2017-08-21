#creates the random data with the specifications required
random1 = rnorm(n= 10, mean = 15, sd = 1)
random2 = rnorm(n= 10, mean = 30, sd = 1)
random3 = rnorm(n= 10, mean = 28, sd = 1)
random4 = rnorm(n= 10, mean = 22, sd = 1)

#creates a 40 row x 2 column table.
# x column is 10 iterations of 0, 1, 2, 3
# y column is the random data
x = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

y = c(random1[1], random1[2], random1[3], random1[4], random1[5],
      random1[6], random1[7], random1[8], random1[9], random1[10],
      random2[1], random2[2], random2[3], random2[4], random2[5],
      random2[6], random2[7], random2[8], random2[9], random2[10],
      random3[1], random3[2], random3[3], random3[4], random3[5],
      random3[6], random3[7], random3[8], random3[9], random3[10],
      random4[1], random4[2], random4[3], random4[4], random4[5],
      random4[6], random4[7], random4[8], random4[9], random4[10])

df = data.frame(x, y)

#linear regression
df.lm = lm(formula = y~x, data = df)
#summary(df.lm)
#anova(df.lm)

#creates an array from 0-3 by 0.1
X = seq(0, 3, 0.1)

#quadratic regression
df.poly2 = lm(formula = y~poly(x, 2), data = df)
predicted2 = predict(df.poly2, list(x = X))
#summary(df.poly2)
anova(df.poly2)

#cubic regression
df.poly3 = lm(formula = y~poly(x, 3), data = df)
predicted3 = predict(df.poly3, list(x = X))
#summary(df.poly3)
#anova(df.poly3)

plot(df$x, df$y); abline(df.lm); lines(X, predicted2, col = "blue"); lines(X, predicted3, col = "red");

owm = lm(y~as.factor(x), data = df)
#summary(owm)
#anova(owm)
