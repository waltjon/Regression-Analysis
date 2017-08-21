library(car)
pneum = `data.ex.13.1_(Pneumoconiosis)`

pneum.lm = lm(Proportion.of.Severe.Cases..y~Number.of.Years.of.Exposure+Number.of.Severe.Cases+Total.Number.of.Miners, data = pneum)
summary(pneum.lm)

pneum.glm = glm(Proportion.of.Severe.Cases..y~Number.of.Years.of.Exposure, family = binomial(), data = pneum)
summary(pneum.glm)
#the regression line next to the data shows that the binomial glm of years to severe cases does not fit the data well
plot(pneum$Number.of.Years.of.Exposure, pneum$Proportion.of.Severe.Cases..y); abline(pneum.glm)

anova(pneum.glm)
Anova(pneum.glm)

odds = exp(coef(pneum.glm))
odds

pneum.quad = glm(Proportion.of.Severe.Cases..y~Number.of.Years.of.Exposure+I(Number.of.Years.of.Exposure^2), family = binomial(), data = pneum)
summary(pneum.quad)
#Null deviance: 1.348836  on 7  degrees of freedom
#Residual deviance: 0.067487  on 5  degrees of freedom

#pneum.quad2 = glm(Proportion.of.Severe.Cases..y~poly(Number.of.Years.of.Exposure, 2), family = binomial(), data = pneum)
#summary(pneum.quad2)