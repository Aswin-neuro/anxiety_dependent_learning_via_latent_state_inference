library(hBayesDM)

## b1_hta 
output1 <- gng_m1(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

output2 <- gng_m2(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

output3<- gng_m3(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

output4<- gng_m4(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

## b2_hta

b2hta <- <- gng_m1(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

b2hta <- <- gng_m2(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

b2hta <- <- gng_m3(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)

b2hta <- <- gng_m34(
  data = "b1_hta_gngm1.txt",
  niter = 4000, 
  nwarmup = 1000,
  nchain = 3,
  ncore = 1,
  vb = FALSE,
  modelRegressor = FALSE
)


## plots ##
plot(output)
plotInd(output1, "ep")
## visualize SV (Subject #1)
plot(sv_all[1, ], type="l", xlab="Trial", ylab="Stimulus Value (subject #1)")

# output <- gng_m1("example", niter=20, nwarmup=10, nchain=1, ncore=1, vb=TRUE)
plot(output, type="trace", fontSize=11)


# SAVING

saveRDS(b2hta_m4, "b2_hta_gngm4.rds")
saveRDS(b3v1hta_m4, "b3v1_hta_gngm4.rds")
saveRDS(output2, "b1_hta_gngm2.rds")
saveRDS(output3, "b1_hta_gngm3.rds")
saveRDS(output4, "b1_hta_gngm4.rds")

output1
fit <- readRDS("b1_hta_gngm1.rds")
