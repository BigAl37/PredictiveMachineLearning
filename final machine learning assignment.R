library(caret); library(rattle); library(ggplot2)
set.seed(1)

df <- read.csv2("machinelearning.csv", header = T, sep = ",")
set.seed(1)

df2 <- read.csv2("pml-testing.csv", header = T, sep = ",")

df.clean <- df[,c('num_window',	'roll_belt',	'pitch_belt',	'yaw_belt',	'total_accel_belt','gyros_belt_x',	'gyros_belt_y',	'gyros_belt_z',	'accel_belt_x',	'accel_belt_y',	'accel_belt_z',	'magnet_belt_x',	'magnet_belt_y',	'magnet_belt_z',	'roll_arm',	'pitch_arm',	'yaw_arm',	'total_accel_arm','gyros_arm_x',	'gyros_arm_y',	'gyros_arm_z',	'accel_arm_x',	'accel_arm_y',	'accel_arm_z',	'magnet_arm_x',	'magnet_arm_y',	'magnet_arm_z','roll_dumbbell',	'pitch_dumbbell',	'yaw_dumbbell','total_accel_dumbbell','gyros_dumbbell_x',	'gyros_dumbbell_y',	'gyros_dumbbell_z',	'accel_dumbbell_x',	'accel_dumbbell_y',	'accel_dumbbell_z',	'magnet_dumbbell_x',	'magnet_dumbbell_y',	'magnet_dumbbell_z',	'roll_forearm',	'pitch_forearm',	'yaw_forearm','total_accel_forearm','gyros_forearm_x',	'gyros_forearm_y',	'gyros_forearm_z',	'accel_forearm_x',	'accel_forearm_y',	'accel_forearm_z',	'magnet_forearm_x',	'magnet_forearm_y',	'magnet_forearm_z',	'classe')]


df.clean2 <- df2[,c('num_window',	'roll_belt',	'pitch_belt',	'yaw_belt',	'total_accel_belt','gyros_belt_x',	'gyros_belt_y',	'gyros_belt_z',	'accel_belt_x',	'accel_belt_y',	'accel_belt_z',	'magnet_belt_x',	'magnet_belt_y',	'magnet_belt_z',	'roll_arm',	'pitch_arm',	'yaw_arm',	'total_accel_arm','gyros_arm_x',	'gyros_arm_y',	'gyros_arm_z',	'accel_arm_x',	'accel_arm_y',	'accel_arm_z',	'magnet_arm_x',	'magnet_arm_y',	'magnet_arm_z','roll_dumbbell',	'pitch_dumbbell',	'yaw_dumbbell','total_accel_dumbbell','gyros_dumbbell_x',	'gyros_dumbbell_y',	'gyros_dumbbell_z',	'accel_dumbbell_x',	'accel_dumbbell_y',	'accel_dumbbell_z',	'magnet_dumbbell_x',	'magnet_dumbbell_y',	'magnet_dumbbell_z',	'roll_forearm',	'pitch_forearm',	'yaw_forearm','total_accel_forearm','gyros_forearm_x',	'gyros_forearm_y',	'gyros_forearm_z',	'accel_forearm_x',	'accel_forearm_y',	'accel_forearm_z',	'magnet_forearm_x',	'magnet_forearm_y',	'magnet_forearm_z')]


n <- length(df.clean)
n2 <- length(df.clean2)



n <- n - 1
##Convert to Integer all strings that are numeric
for (i in 1:n) {
    if(is.numeric(df.clean[,i]) == TRUE) {
        df.clean[,i] <- as.numeric(df.clean[,i])
    } else if(is.factor(df.clean[,i]) == T) {
        df.clean[, i] <- as.numeric(levels(df.clean[,i])[df.clean[,i]])
    }
}


##Convert to Integer all strings that are numeric
for (i in 1:n2) {
    if(is.numeric(df.clean2[,i]) == TRUE) {
        df.clean2[,i] <- as.numeric(df.clean2[,i])
    } else if(is.factor(df.clean2[,i]) == T) {
        df.clean2[, i] <- as.numeric(levels(df.clean2[,i])[df.clean2[,i]])
    }
}



inTrain <- createDataPartition(y=df.clean$classe, p=0.75, list = FALSE)

training <- df.clean[inTrain,]
testing <- df.clean[-inTrain,]



###Set the Machine Learning Parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
mtry <- floor(sqrt(ncol(training)))
tunegrid <- expand.grid(.mtry = mtry)


predict <- train(classe ~ ., data = training, method = "rf",  metric=metric, tuneGrid=tunegrid, trControl=control)



predicted <- predict(predict, testing)
confusionMatrix(predicted, testing$classe)


predicted2 <- predict(predict, df.clean2)
confusionPredicted2 <- confusionMatrix(predicted, testing$classe)
confusionPredicted2$table
