setwd("C:/Users/Jonathan/Desktop/STAT/Regression/Data")

maleptsd = read.table("maleptsd.txt", header = T)

ptsd.lm = lm(PTSD~OVER2+OVER3+OVER5+BOND+POSIT+NEG+CONTR+SUP+CONS+AFF, data = maleptsd)
summary(ptsd.lm)


step(ptsd.lm, direction = c("forward"))
#same as full
#ptsd.forward.lm = lm(PTSD~OVER2+OVER3+OVER5+BOND+POSIT+NEG+CONTR+SUP+CONS+AFF)


step(ptsd.lm, direction = c("backward"))
ptsd.backward.lm = lm(PTSD~OVER2+OVER3+NEG+AFF, data = maleptsd)
summary(ptsd.backward.lm)


step(ptsd.lm, direction = c("both"))
#same as backward

removed = maleptsd[c(39, 31, 21, 22, 59, 24, 44, 10, 8, 63),]
#removed.lm = lm(PTSD~OVER2+OVER3+NEG+AFF, data = removed)

index = maleptsd[-c(39, 31, 21, 22, 59, 24, 44, 10, 8, 63),]
index.lm = lm(PTSD~OVER2+OVER3+NEG+AFF, data = index)
summary(index.lm)


predict(index.lm, removed)
mean(removed$PTSD-predict(index.lm, removed))^2
