library(DescTools)
library(tidyverse)

data <- pretestBIN[,1:3] %>% as.matrix()

CochranQTest(data)
