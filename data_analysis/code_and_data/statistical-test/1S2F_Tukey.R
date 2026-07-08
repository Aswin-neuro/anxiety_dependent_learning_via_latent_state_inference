# If there is significant changes
TukeyHSD(aov_res)


### If Assumptions fail - non- param tests
#a. 2 way comparison - Wilcoxon test - 
#repeat blockwise
pairwise.wilcox.test(df$accuracy, df$HL, p.adjust.method = "BH", subset = df$block == 1)

#b. Kruskal- wallis - if multiple groups
kruskal.test(accuracy ~ interaction(block, HL), data = df)

