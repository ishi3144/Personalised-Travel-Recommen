#install.packages("mlogit")
library("mlogit")
data("ModeCanada", package = "mlogit")
MC <- dfidx(ModeCanada, subset = noalt == 4)
ml.MC1 <- mlogit(choice ~ cost + freq + ovt | income | ivt, MC)
ml.MC1b <- mlogit(choice ~ cost + freq + ovt | income | ivt, ModeCanada,
                  subset = noalt == 4, idx = c("case", "alt"))
MC$time <- with(MC, ivt + ovt)
ml.MC1 <- mlogit(choice ~ cost + freq | income | time, MC, 
                 alt.subset = c("car", "train", "air"), reflevel = "car")
summary(ml.MC1)
head(fitted(ml.MC1, type = "outcome"))
head(fitted(ml.MC1, type = "probabilities"), 4)
sum(log(fitted(ml.MC1, type = "outcome")))
logLik(ml.MC1)
apply(fitted(ml.MC1, type = "probabilities"), 2, mean)
predict(ml.MC1)
NMC <- MC
# YC2020/05/03 should replace everywhere index() by idx()
NMC[idx(NMC)$alt == "train", "time"] <- 0.8 *
  NMC[idx(NMC)$alt == "train", "time"]
Oprob <- fitted(ml.MC1, type = "probabilities")
Nprob <- predict(ml.MC1, newdata = NMC)
rbind(old = apply(Oprob, 2, mean), new = apply(Nprob, 2, mean))
head(Nprob[, "air"] / Nprob[, "car"])
head(Oprob[, "air"] / Oprob[, "car"])
ivbefore <- logsum(ml.MC1)
ivafter <- logsum(ml.MC1, data = NMC)
surplus <- - (ivafter - ivbefore) / coef(ml.MC1)["cost"]
summary(surplus)
effects(ml.MC1, covariate = "income", type = "ar")
effects(ml.MC1, covariate = "cost", type = "rr")
coef(ml.MC1)[grep("time", names(coef(ml.MC1)))] /
  coef(ml.MC1)["cost"] * 60 