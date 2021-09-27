# Monday 27.09.21 - Statistics for Business Management. 

### loading data ###
bike <- read.csv("gitRepos/stat4BusMan/Practice_week3/BICI_for_students/train.csv")
bike.new=bike
month <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%u"))
hour <- as.integer(format(as.POSIXlt(bike.new$datetime), format = "%H"))
bike <- data.frame(bike$season, month, weekday, hour, as.factor(bike$workingday), as.factor(bike$holiday), 
                   as.factor(bike$weather), bike$temp, bike$hum, bike$windspeed, bike$count)
names(bike) <- c("season", "month", "weekday", "hour", "isweekday", 
                 "isholiday", "weathertype", "temperature", "humidity", "windspeed", "count")

bike <- bike[which(bike$windspeed != 0.0000),]
### building models ###
# partitioning data
set.seed(1)
nrow(bike)
sample.index <- sample(nrow(bike), 9573*0.75, replace = FALSE)
bike.train <- bike[sample.index,]
bike.test <- bike[-sample.index,]

# linear regression

Model <- lm(data = bike.train, count ~  temperature + humidity)
summary(Model)

library(pmml)

bike_pmml=pmml(Model)
save_pmml(bike_pmml, "gitRepos/stat4BusMan/Practice_week3/BICI_for_students/bike.xml")

library(foreign)
write.foreign(df=bike.test, datafile="gitRepos/stat4BusMan/Practice_week3/BICI_for_students/bike_test.csv", 
              codefile="gitRepos/stat4BusMan/Practice_week3/BICI_for_students/bike_test.sas", package="SAS")
