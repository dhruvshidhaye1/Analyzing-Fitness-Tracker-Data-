
step <- read.csv("01_Steps.csv")
sleep <- read.csv("02_Sleep.csv")

mean(step$calories)
#232.6744
mean(step$runDistance)
#491.6225 meters
mean(step$distance)
#5917.784 meters
mean(step$steps)
#8218.443

mean(sleep$deepSleepTime)
#109.2282 minutes
mean(sleep$shallowSleepTime)
#233.4974 minutes
mean(sleep$wakeTime)
#4.363436 minutes awake between sleeps

plot(step$steps,step$calories)
regStep <- lm(calories~steps, data=step)
abline(regStep, col="red")


regr.error <- function(predicted,actual){
mae <- mean(abs(actual-predicted))
mse <- mean((actual-predicted)^2)
rmse <- sqrt(mean((actual-predicted)^2))
mape <- mean(abs((actual-predicted)/actual))
errors <- c(mae,mse,rmse,mape)
names(errors) <- c("mae","mse","rmse","mape")
return(errors)
}

n <- nrow(step)
split <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.70,0.30))
train.step <- step[split, ]
test.step <- step[!split, ]


regStep2 <- lm(calories~steps, data=train.step)
abline(regStep2, col="blue")


lm.predictions <- predict(regStep2, test.step)
regr.error(lm.predictions, test.step$calories)


plot(step$distance,step$calories)
regDist <- lm(calories~distance, data=step)
abline(regDist, col="red")
regDist2 <- lm(calories~distance, data=train.step)
abline(regStep2, col="blue")



lm.predictions2 <- predict(regDist2, test.step)
regr.error(lm.predictions2, test.step$calories)


disturbed <-subset(sleep,sleep$wakeTime==4)
undisturbed <-subset(sleep,sleep$wakeTime==0)


explore <- function(x){
data <- c("Mean"=mean(x, na.rm=TRUE),
"Median"=median(x, na.rm =T),
"Standard Deviation" = sd(x, na.rm =T),
"Length" = length(x))
return(data)
}

disturbed_explore <- explore(disturbed$deepSleepTime)
undisturbed_explore <- explore(undisturbed$deepSleepTime)
sd_dist_undist <- sqrt(disturbed_explore[3]^2/disturbed_explore[4] + undisturbed_explore[3]^2/undisturbed_explore[4])
z_score <- (disturbed_explore[1]-undisturbed_explore[1])/sd_dist_undist
z_score
1-pnorm(z_score)
