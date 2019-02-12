# Install required packages

  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(caret)) install.packages("caret")
  if(!require(gtools)) install.packages("gtools")
  if(!require(randomForest)) install.packages("randomForest")

# Load the data set from the desktop

  ds <- read_csv("C:/Users/Desktop/Data_Cortex_Nuclear.csv")

# Data partition
  
  set.seed(1)
  partition <- createDataPartition(y = ds$Genotype, times = 1, p = 0.1, list = FALSE)
  train <- ds[-partition,]
  test <- ds[partition,]

# Create a vector with all the proteins names
  
  proteins <- ds %>% select(-MouseID,-Genotype,-Behavior,-Treatment,-class) %>% names(.)

# A model is fit with the train set and checked with the test set for each mice parameter. Accuracy of each prediction is displayed
  
# Genotype
  
  t <- train %>% select(-MouseID,-Behavior,-Treatment,-class) %>% mutate(Genotype=as.factor(Genotype))
  genoFit <- randomForest(Genotype ~ ., data = t, na.action=na.roughfix)
  
  genoPred <- predict(genoFit,  na.roughfix(test[,2:78]))
  mean(genoPred==test$Genotype)
  
# Behavior
  
  t <- train %>% select(-MouseID,-Genotype,-Treatment,-class) %>% mutate(Behavior=as.factor(Behavior))
  behaFit <- randomForest(Behavior ~ ., data = t, na.action=na.roughfix)
  
  behaPred <- predict(behaFit,  na.roughfix(test[,2:78]))
  mean(behaPred==test$Behavior)
  
# Treatment
  
  t <- train %>% select(-MouseID,-Genotype,-Behavior,-class) %>% mutate(Treatment=as.factor(Treatment))
  treatFit <- randomForest(Treatment ~ ., data = t, na.action=na.roughfix)
  
  treatPred <- predict(treatFit,  na.roughfix(test[,2:78]))
  mean(treatPred==test$Treatment)

# Monte Carlo simulation (It may take 2-3 hours)
# Mean accuracy of random forest with diferent number of random proteins is calculated
  
  results <- vector("numeric")
  pnumber <- 1:length(proteins)
  
  for (ps in pnumber){ # The Monte Carlo simulation is repeated with each number of proteins from 1 to 77
  
    r <- replicate(100,{
      t <- train %>% select(-MouseID,-Behavior,-Treatment,-class) %>% mutate(Genotype=as.factor(Genotype)) %>%
        select(sample(1:77, ps),78)
      fit <- randomForest(Genotype ~ ., data = t, na.action=na.roughfix)
    
      pred <- predict(fit,  na.roughfix(test[,2:78]))
      mean(pred==test$Genotype)
    })
    
    message(" || MC Simulation ", ps,"/", length(proteins)) # This line is to know how is it going
    results[ps] <- mean(r)
  }
  
# Results display
  
  data.frame(proteins=pnumber, accuracy=results) %>% ggplot(aes(proteins,accuracy)) + 
    geom_point() + geom_vline(xintercept=min(pnumber[results > 0.99]), colour="red") +
    geom_vline(xintercept=min(pnumber[results > 0.95]), colour="orange") +
    xlab("Number of proteins") + ylab("Accuracy")
  
  min(pnumber[results > 0.95])
  min(pnumber[results > 0.99])
  