library(hBayesDM)

output <- gng_m1(data = "/home/aswin/R/Projects/hbayesDM/hbayes_sample/gng_exampleData.txt", niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

output1 = gng_m1("example", niter=2000, nwarmup=1000, nchain=4, ncore=4)
output2 = gng_m2("example", niter=2000, nwarmup=1000, nchain=4, ncore=4)
output3 = gng_m3("example", niter=2000, nwarmup=1000, nchain=4, ncore=4)
output4 = gng_m4("example", niter=2000, nwarmup=1000, nchain=4, ncore=4)

plot(output1, type="trace", fontSize=11) # traceplot of hyper parameters. Set font size 11.
plot(output1, type="trace", inc_warmup=T) # traceplot of hyper parameters w/ warmup samples
plot(output1)
plotInd(output1, "ep")

printFit(output1, output2, output3, output4)

# Variational inference for approximate posterior sampling
output_3 = gng_m3(data="example", vb = TRUE)

### fit example data with the gng_m3 model and run posterior predictive checks
x = gng_m3(data="example", niter=2000, nwarmup=1000, nchain=4, ncore=4, inc_postpred = TRUE)

## dimension of x$parVals$y_pred
dim(x$parVals$y_pred)   # y_pred --> 4000 (MCMC samples) x 10 (subjects) x 240 (trials)
[1] 4000  10  240

y_pred_mean = apply(x$parVals$y_pred, c(2,3), mean)  # average of 4000 MCMC samples

dim(y_pred_mean)  # y_pred_mean --> 10 (subjects) x 240 (trials)
[1]  10 240

numSubjs = dim(x$allIndPars)[1]  # number of subjects

subjList = unique(x$rawdata$subjID)  # list of subject IDs
maxT = max(table(x$rawdata$subjID))  # maximum number of trials
true_y = array(NA, c(numSubjs, maxT)) # true data (`true_y`)

## true data for each subject
for (i in 1:numSubjs) {
  tmpID = subjList[i]
  tmpData = subset(x$rawdata, subjID == tmpID)
  true_y[i, ] = tmpData$keyPressed  # only for data with a 'choice' column
}

## Subject #1
plot(true_y[1, ], type="l", xlab="Trial", ylab="Choice (0 or 1)", yaxt="n")
lines(y_pred_mean[1,], col="red", lty=2)
axis(side=2, at = c(0,1) )
# legend("bottomleft", legend=c("True", "PPC"), col=c("black", "red"), lty=1:2)