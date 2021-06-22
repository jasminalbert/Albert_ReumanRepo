head(cores1)
cores1$mnsymLTdif <- cores1$co.meansym - cores1$co.meanELT

model1 <- lm(formula = mnsymLTdif ~ sigma + mudif, data = cores1)
summary(model1)
AIC(model1)

model2 <- lm(formula = mnsymLTdif ~ sigma + mudif + delta, data = cores1)
summary(model2)
AIC(model2)

model3 <- lm(formula = mnsymLTdif ~ sigma + mu2 + delta + mu1, data = cores1)
summary(model3)
AIC(model3)

model4 <- lm(formula = mnsymLTdif ~ sigma + delta + mu1, data = cores1)
summary(model4)
AIC(model4)

model5 <- lm(formula = mnsymLTdif ~ sigma + delta + mu2, data = cores1)
summary(model5)
AIC(model5)

plot(cores1$mu1,cores1$mnsymLTdif)
plot(cores1$mu2,cores1$mnsymLTdif)
plot(cores1$mudif,cores1$mnsymLTdif)


model6 <- lm(formula = mnsymLTdif ~ sigma * mudif, data = cores1)
summary(model6)
AIC(model6)

model7 <- lm(formula = mnsymLTdif ~ sigma * mudif * delta *mu1 *mu2, data = cores1)
summary(model7)
AIC(model7)

model8 <- lm(formula = msLdif ~ sigma + mudif, data = res2m)
summary(model8)
AIC(model8)

model9 <- lm(formula = msLdif ~ sigma + mudif + delta, data = res2m)
summary(model9)
AIC(model9)

model10 <- lm(formula = msLdif ~ sigma * mudif * delta, data = res2m)
summary(model10)
AIC(model10)




































