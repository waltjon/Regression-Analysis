library(car)

q1 = read.delim("http://funnel.vtti.vt.edu/~cgaylord/windmill.txt")
q1.lm = lm(DC.Output..y~Wind.Velocity..mpg...x, data = q1)
summary(q1.lm)
vif(q1.lm)
q1.quad = lm(DC.Output..y~Wind.Velocity..mpg...x+I(Wind.Velocity..mpg...x^2), data = q1)
summary(q1.quad)
q1$newx = 1/q1$Wind.Velocity..mpg...x
q1.newlm = lm(DC.Output..y~newx, data = q1)
summary(q1.newlm)


q2 = read.delim("http://funnel.vtti.vt.edu/~cgaylord/coal_injuries.txt")
q2.glm = glm(Fractures.y~INB.x1+EXTRP.x2+SEAMH.x3+TIME.x4, family = "poisson", data = q2)
summary(q2.glm)
q2.log = glm(Fractures.y~INB.x1+EXTRP.x2+SEAMH.x3+TIME.x4, family = poisson(link = "log"), data = q2)
summary(q2.log)
q2.test = glm(Fractures.y~EXTRP.x2, family = "poisson", data = q2)
summary(q2.test)


q3 = read.delim("http://funnel.vtti.vt.edu/~cgaylord/coal_injuries.txt")
q3.lm = lm(Fractures.y~INB.x1+EXTRP.x2+SEAMH.x3+TIME.x4, data = q3)
summary(q3.lm)
sum(residuals(q3.lm)^2)
q3.x1 = lm(Fractures.y~INB.x1, data = q3)
summary(q3.x1)
q3.x2 = lm(Fractures.y~EXTRP.x2, data = q3)
summary(q3.x2)
sum(residuals(q3.x2)^2)
q3.x3 = lm(Fractures.y~SEAMH.x3, data = q3)
summary(q3.x3)
q3.x4 = lm(Fractures.y~TIME.x4, data = q3)
summary(q3.x4)


q4 = read.delim("http://funnel.vtti.vt.edu/~cgaylord/liquefication.txt")
q4.lm = lm(y~x1+x2+x3+x4+x5+x6+x7, data = q4)
vif(q4.lm)
q4.new = lm(y~x6+x7, data = q4)
summary(q4.new)
step(q4.lm, direction = "backward")
vif(q4.new)


q5 = read.delim("http://funnel.vtti.vt.edu/~cgaylord/hald_cement.txt")
q5.lm = lm(y~x_1+x_2+x_3+x_4, data = q5)
summary(q5.lm)
vif(q5.lm)
kappa(q5.test)
q5.test = lm(y~x_1+x_2+x_4, data = q5)
vif(q5.test)
summary(q5.test)
step(q5.lm, direction = "forward")
step(q5.lm, direction = "backward")
step(q5.lm, direction = "both")