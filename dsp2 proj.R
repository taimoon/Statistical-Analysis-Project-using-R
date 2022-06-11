rel_pth = "C:\\Users\\Leong Teng Man\\Documents\\GitHub\\DataScienceProg2-project"
setwd(rel_pth)
cereals_df = read.csv("cereals-production.csv")

names(maize_df)
dev.off()
par(mfrow=c(4,1))

plot(cereals_df$Year, cereals_df$Yield, type="l")
plot(cereals_df$Year, cereals_df$Production, type="l")
plot(cereals_df$Year, cereals_df$Fertilizer.consumption, type="l")
plot(cereals_df$Year, cereals_df$Annual.CO2.emissions, type="l")


cereals_df = cereals_df[,4:ncol(cereals_df)]
cereals_df



library(lmtest)
test_graph = function(data, formula){
  par(mfrow=c(1,2))
  lm_obj = lm(formula, data)
  plot(fitted(lm_obj), resid(lm_obj)) # resid plt for const variance
  abline(h=0)
  
  qqnorm(resid(lm_obj)) # normality plt of resid
  qqline(resid(lm_obj))
  
  
  print(bptest(formula, data = data)) # const variance test
  print(shapiro.test(resid(lm_obj))) # normality test
  lm_obj
}

# correlation matrix
cor(cereals_df)
pairs(Yield ~., data = cereals_df)


dat = subset(cereals_df, select = -c(Max.Temperature,Min.Temperature))
crop_lm = test_graph(Yield ~., data = dat)

library(MASS)
crop_lm = lm(Yield ~., dat)
summary(crop_lm)

stepAIC(crop_lm, direction = "both")

crop_blm = test_graph(dat, Yield ~ Mean.Temperature + Humidity + Fertilizer.consumption + 
                        Annual.CO2.emissions)
summary(crop_blm)
