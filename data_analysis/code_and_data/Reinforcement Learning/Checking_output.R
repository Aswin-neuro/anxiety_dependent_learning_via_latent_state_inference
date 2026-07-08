library(hBayesDM)
library(rstan)
b2hta_m4
main <- b2hta_m4$fit 
print(main)
View(main)
rhat_vals <- summary(main)$summary[, "Rhat"]
main[, "Rhat"]
library
## Plotting
plot(reload, type="trace", fontSize=11)   # traceplot of hyper parameters. Set font size 11.
plot(reload, type="trace", inc_warmup=T)  
plot(reload)
plotInd(reload, "ep")

printFit(output1, output2, output3, output4)

# print
png("reload_b2hta_m4_trace.png", width = 800, height = 600)
plot(reload)
dev.off()

## Save and Reload
saveRDS(hs1, "s1_hta_m4.rds")

reload <- readRDS("b2_hta_gngm4.rds")
reload$fit


b4lta_m4$fit
