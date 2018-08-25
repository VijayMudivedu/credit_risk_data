# Task: The task is pretty much simple. 
# Choose the right model to build given data. 
# Additional points will be awarded, if you do research on credit approval and adopt models accordingly.


library(tidyverse)

credit_data_df <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                           sep = ",",header = FALSE,check.names = TRUE)

head(credit_data_df)
str(credit_data_df)

# check NAs Missing numbers, factors
# check special characters
apply(credit_data_df, MARGIN = 2,function(x) sum(x %in% "?"))
sapply(credit_data_df,summary)
# V1  V2  V3  V4  V5  V6  V7  V8  V9 V10 V11 V12 V13 V14 V15 V16 
# 12  12   0   6   6   9   9   0   0   0   0   0   0  13   0   0 

#checking the correlation between the unknowns

# V1
credit_data_df %>% filter(V1 == "?") 
credit_data_df %>% filter(V1 == "?") %>% summary()
  # V1  V2  V3     V4  V5  V6  V7  V8  V9 V10 V11 V12 V13 V14 V15 V16 
  # 6   ? 26.50  2.710  y  p  ?  ? 0.085  f   f   0   f   s 00080    0   -
  # 7   ? 45.33  1.000  u  g  q  v 0.125  f   f   0   t   g 00263    0   -
  # 8   ? 20.42  7.500  u  g  k  v 1.500  t   t   1   f   g 00160  234   +
  # 9   ? 20.08  0.125  u  g  q  v 1.000  f   t   1   f   g 00240  768   +
  # 10  ? 42.25  1.750  y  p  ?  ? 0.000  f   f   0   t   g 00150    1   -
# Comments: No correlation between the V1 and V2. V6, V7 are slightly correlated.

# V2
credit_data_df %>% filter(V2 == "?") 
credit_data_df %>% filter(V2 == "?") %>% summary()
credit_data_df %>% summary()
V2 <- as.numeric(credit_data_df[!credit_data_df$V2 =="?",]["V2"]) 
attributes(V2)
V2$V2 <- as.numeric(V2$V2)
summary(V2$V2)
summary(V2$V2[-which(V2$V2 %in% boxplot.stats(x = V2$V2,coef = 1.58)$stats)])
boxplot(as.numeric(V2$V2))
summary(as.numeric(as.character(V2)))


# Comments: Missing at Random

# V4 -- # V4, V4 V6 V7 are highly correlated
credit_data_df %>% filter(V4 == "?")
credit_data_df %>% filter(V4 == "?") %>% summary()

# Correlation between V6, V7 and V13
credit_data_df %>% filter(V6 == "?")

# correlation between V13 and other variables
credit_data_df %>% filter(V14 == "?")
credit_data_df %>% filter(V14 == "?") %>% summary()

View(credit_data_df)


# check duplicates
#  V1    V2    V3 V4 V5 V6 V7 V8  V9  V10 V11 V12 V13   V14     V15 V16
#   1  b 30.83 0.000  u  g  w  v 1.25  t   t   1   f   g 00202   0   +
#   2  a 58.67 4.460  u  g  q  h 3.04  t   t   6   f   g 00043 560   +
#   3  a 24.50 0.500  u  g  q  h 1.50  t   f   0   f   g 00280 824   +
#   4  b 27.83 1.540  u  g  w  v 3.75  t   t   5   t   g 00100   3   +
#   5  b 20.17 5.625  u  g  w  v 1.71  t   f   0   f   s 00120   0   +
#   6  b 32.08 4.000  u  g  m  v 2.50  t   f   0   t   g 00360   0   +

distinct(credit_data_df,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15) %>% nrow()
# No duplicates found in the dataset



# Check outliers
# Check invalid values
# check columns

