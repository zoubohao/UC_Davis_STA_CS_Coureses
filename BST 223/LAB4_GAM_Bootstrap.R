
# Bootstrap ----------------

library(boot)

# * confidence regions ----------------

B <- 999
data <- read.table('../datasets/melanoma.txt', header=TRUE)
beta.est <- function(data, ind){
  res <- glm(totalincidence ~ ., data=data[ind,], family=poisson())
  coef(res)
}
boot.res <- boot(data, beta.est, R=B)

# ** confidence intervals for individual coefficients ----------------

alpha = 0.05
# 95% bootstrap confidence interval for beta_1
sort(boot.res$t[,2])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))]
# 95% bootstrap confidence interval for beta_2
sort(boot.res$t[,3])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))]

# ** confidence ellipse ----------------

#pdf('BootEllipse.pdf', width=4.5, height=3)
#par(mgp=c(1.8, 0.5, 0.0), mar=c(3,3,1,.5))
library(car)
# 95% and 99% bootstrap confidence regions for (beta_1, beta_2)
dataEllipse(boot.res$t[,2], boot.res$t[,3],
            xlab= 'year coefficient', ylab='sunspot coefficient',
            cex=.3, levels=c(.95, .99))
#dev.off()



# GAM ----------------

# consumes a lot more degrees of freedom;
# a good exploratory tool to see if any nonlinear relationship exists between responses and predictors;
# After detecting the relationship/pattern (e.g. linear, quadratic, etc.), fit a GLM accordingly.

# fit a GLM as a baseline model
glm <- glm(Volume ~ Height + Girth, family = Gamma(link=log), data = trees)

# * gam::gam ----------------

# g(E(Y|X)) = beta0 + beta1 * X1 + beta2 * X2 + f1(X1) + f2(X2)

# coefficients: beta0, beta1, beta2
# smooth: f1, f2
# model: design matrix without intercept & response
# additive.predictors: beta0 + beta1 * X1 + beta2 * X2 + f1(X1) + f2(X2)
# fitted.values: g^(-1)(additive.predictors)

library(gam)
data(trees) # n=31, 2 predictors
gamgam1 <- gam::gam(Volume ~ s(Height,4) + s(Girth,5),
                    family=Gamma(link="log"), data=trees)
# smoothing spline with df=4 for Height and df=5 for Girth.
summary(gamgam1)
# additive effect of Height is not significant

as.numeric(
  as.matrix(cbind(rep(1,nrow(trees)), gamgam1$model[,2:3])) %*%
    gamgam1$coef + rowSums(gamgam1$smooth)
)
gamgam1$additive.predictors
log(gamgam1$fitted.values)

par(mfrow=c(1,2), mgp = c(1.8,0.5,0), mar = c(3,3,.5,0.5), oma = c(0,0,2,0))
plot(gamgam1) # plot the fitted smoothing function. See Figure 3.
title(main = "gamgam1", outer = TRUE)
# For Girth: almost a linear pattern
# -> Do not need to waste df to fit a smoothing splines component -> GAPLM (gamgam3)

# comparing gamgam1 with glm
tmp <- cbind(trees$Height, (trees$Height - mean(trees$Height)) * gamgam1$coef[2] + gamgam1$smooth[,1])
tmp <- tmp[order(trees$Height),]
plot(tmp, type = 'l', ylab = expression("f"[1]*"(Height)"), xlab = "Height")
lines(sort(trees$Height), (sort(trees$Height) - mean(trees$Height)) * glm$coef[2], lty = 2, col = 2)
tmp <- cbind(trees$Girth, (trees$Girth - mean(trees$Girth)) * gamgam1$coef[3] + gamgam1$smooth[,2])
tmp <- tmp[order(trees$Girth),]
plot(tmp, type = 'l', ylab = expression("f"[2]*"(Girth)"), xlab = "Girth")
lines(sort(trees$Girth), (sort(trees$Girth) - mean(trees$Girth)) * glm$coef[3], lty = 2, col = 2)
legend("bottomright",c("gamgam1","glm"), lty = 1:2, col = 1:2)
title(main = paste0("gamgam1"," vs glm"), outer = TRUE)

gamgam2 <- gam::gam(Volume~lo(Height,degree=1)+s(Girth,5),
                    family=Gamma(link=log),data=trees)
# local linear regression for Height,
# and smoothing spline with df=5 for Girth
summary(gamgam2)
plot(gamgam2) # See Figure 4.
title(main = "gamgam2", outer = TRUE)

# comparing gamgam2 with glm
tmp <- cbind(trees$Height, (trees$Height - mean(trees$Height)) * gamgam2$coef[2] + gamgam2$smooth[,2])
tmp <- tmp[order(trees$Height),]
plot(tmp, type = 'l', ylab = expression("f"[1]*"(Height)"), xlab = "Height")
lines(sort(trees$Height), (sort(trees$Height) - mean(trees$Height)) * glm$coef[2], lty = 2, col = 2)
tmp <- cbind(trees$Girth, (trees$Girth - mean(trees$Girth)) * gamgam2$coef[3] + gamgam2$smooth[,1])
tmp <- tmp[order(trees$Girth),]
plot(tmp, type = 'l', ylab = expression("f"[2]*"(Girth)"), xlab = "Girth")
lines(sort(trees$Girth), (sort(trees$Girth) - mean(trees$Girth)) * glm$coef[3], lty = 2, col = 2)
legend("bottomright",c("gamgam2","glm"), lty = 1:2, col = 1:2)
title(main = paste0("gamgam2"," vs glm"), outer = TRUE)

gamgam3 <- gam::gam(Volume~s(Height,4)+Girth,family=Gamma(link=log),data=trees)
# Fitting a GAPLM with linear effect on Girth
summary(gamgam3)
plot(gamgam3) # See Figure 5.
title(main = "gamgam3", outer = TRUE)
gamgam4 = gam::gam(Volume~Height+s(Girth,5), family = Gamma(link=log), data = trees)
summary(gamgam4)

# * mgcv::gam ----------------

# g(E(Y|X)) = beta0 + f(X1) + f(X2)

library(mgcv)
mgcvgam1 <- mgcv::gam(Volume~s(Height)+s(Girth),
                      family=Gamma(link=log),data=trees)
summary(mgcvgam1)
plot(mgcvgam1, residuals=TRUE, shade = TRUE)
# confidence bands with observed scatter plot. See Figure 6.
# use the trees data for illustration as well
# both quite linear

mgcvgam2 <- mgcv::gam(Volume ~ s(Height,bs="cr",k=15) + s(Girth, bs="cr", k=15),
                      family=Gamma(link=log),data=trees)
# k specifies number of basis functions
# bs specifies the basis functions used here: cubic splines here; default is thin plate splines.
summary(mgcvgam2)
plot(mgcvgam2, residuals=TRUE, shade=TRUE) # See Figure 7.
# Is this a good fit? Why or why not?
# No. Using too many knots, the results are undersmoothing.

mgcvgam3 <- mgcv::gam(Volume ~ s(Height)+Girth,
                      family=Gamma(link=log),data=trees) # Fit a GAPLM
summary(mgcvgam3)
plot(mgcvgam3, residuals=TRUE, shade=TRUE, all.terms = TRUE) # See Figure 8.
# all.terms=TRUE: plot the parametric component also.

