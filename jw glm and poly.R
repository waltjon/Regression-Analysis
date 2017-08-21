setwd("C:/Users/Jonathan/Desktop/STAT/Regression/Project")
counties = read.csv("Counties.csv")
#removes NA's
counties = subset(counties, !is.na(counties$democrat))
counties = subset(counties, !is.na(counties$republican))


#full linear democrat
counties.dem.glm = glm(democrat~pop.density+pop+pop.change+
                     age6574+age75+
                     crime+college+income+farm+
                   white+black+turnout, data = counties)
summary(counties.dem.glm)
#removes age75 and college
step(counties.dem.glm, direction = c("backward"))


#full linear republican
counties.rep.glm = glm(republican~pop.density+pop+pop.change+
                         age6574+age75+
                         crime+college+income+farm+
                         white+black+turnout, data = counties)
summary(counties.rep.glm)
#removes age6574
step(counties.rep.glm, direction = c("backward"))





#full quadratic democratic
counties.dem.quad = glm(democrat~pop.density+I(pop.density^2)+
                      pop+I(pop^2)+
                      pop.change+I(pop.change^2)+
                     age6574+I(age6574^2)+
                      age75+I(age75^2)+
                     crime+I(crime^2)+
                      college+I(college^2)+
                      income+I(income^2)+
                      farm+I(farm^2)+
                     white+I(white^2)+
                      black+I(black^2)+
                      turnout+I(turnout^2), data = counties)
summary(counties.dem.quad)
#removes age6574, age6574^2, crime, crime^2, black^2, turnout, turnour^2
step(counties.dem.quad, direction = c("backward"));


#full quadratic republican
counties.rep.quad = glm(republican~pop.density+I(pop.density^2)+
                          pop+I(pop^2)+
                          pop.change+I(pop.change^2)+
                          age6574+I(age6574^2)+
                          age75+I(age75^2)+
                          crime+I(crime^2)+
                          college+I(college^2)+
                          income+I(income^2)+
                          farm+I(farm^2)+
                          white+I(white^2)+
                          black+I(black^2)+
                          turnout+I(turnout^2), data = counties)
summary(counties.rep.quad)
#removes pop.density^2, age6574, age6574^2, farm^2, turnout^2
step(counties.rep.quad, direction = c("backward"))





#full cubic democrat
counties.dem.cubic = glm(democrat~pop.density+I(pop.density^2)+I(pop.density^3)+
                       pop+I(pop^2)+I(pop^3)+
                       pop.change+I(pop.change^2)+I(pop.change^3)+
                       age6574+I(age6574^2)+I(age6574^3)+
                       age75+I(age75^2)+I(age75^3)+
                       crime+I(crime^2)+I(crime^3)+
                       college+I(college^2)+I(college^3)+
                       income+I(income^2)+I(income^3)+
                       farm+I(farm^2)+I(farm^3)+
                       white+I(white^2)+I(white^3)+
                       black+I(black^2)+I(black^3)+
                       turnout+I(turnout^2)+I(turnout^3), data = counties)
summary(counties.dem.cubic)
#removes pop.density^3, pop.change^2, age6574^3, age75, crime, crime^2, crime^3
step(counties.dem.cubic, direction = c("backward"))


#full cubic republican
counties.rep.cubic = glm(republican~pop.density+I(pop.density^2)+I(pop.density^3)+
                           pop+I(pop^2)+I(pop^3)+
                           pop.change+I(pop.change^2)+I(pop.change^3)+
                           age6574+I(age6574^2)+I(age6574^3)+
                           age75+I(age75^2)+I(age75^3)+
                           crime+I(crime^2)+I(crime^3)+
                           college+I(college^2)+I(college^3)+
                           income+I(income^2)+I(income^3)+
                           farm+I(farm^2)+I(farm^3)+
                           white+I(white^2)+I(white^3)+
                           black+I(black^2)+I(black^3)+
                           turnout+I(turnout^2)+I(turnout^3), data = counties)
summary(counties.rep.cubic)
#removes pop.density, pop.change^3, age6564^3, white^3, black^3
step(counties.rep.cubic, direction = c("backward"))