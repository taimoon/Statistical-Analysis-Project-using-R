rel_pth = "C:\\Users\\Leong Teng Man\\Desktop\\dsp2"
setwd(rel_pth)
maize_df = read.csv("maize-production.csv")

names(maize_df)
dev.off()
par(mfrow=c(4,1))

plot(maize_df$Year, maize_df$Yield, type="l")
plot(maize_df$Year, maize_df$Production, type="l")
plot(maize_df$Year, maize_df$Fertilizer.consumption, type="l")
plot(maize_df$Year, maize_df$Annual.CO2.emissions, type="l")



maize_df = maize_df[,5:ncol(maize_df)]
maize_df



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
cor(maize_df)
pairs(Yield ~., data = maize_df)


dat = subset(maize_df, select = -c(Max.Temperature,Min.Temperature))
crop_lm = test_graph(Yield ~., data = dat)

library(MASS)
crop_lm = lm(Yield ~., dat)
summary(crop_lm)

stepAIC(crop_lm, direction = "both")

crop_blm = test_graph(dat, Yield ~ Mean.Temperature + Humidity + Fertilizer.consumption + 
                        Annual.CO2.emissions)
summary(crop_blm)
