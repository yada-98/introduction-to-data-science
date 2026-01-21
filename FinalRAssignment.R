library(tidyverse)
library(readODS)
library(plotly)

install.packages("plotly")
#Load data into R

filePath <- "4113-complaint-rate-by-operator.ods"
df_complaints <- read_ods(
  path  = filePath,
  sheet = 3,   
  skip  = 5    
)
#Data Cleaning for complaint rate dataset

# To remove quartely data, use only annual data only

df_annual <- df_complaints [(0:18),]

# To remove covid period Apr 2020-Mar 2021, Apr 2021-Mar2022

df_uncleaned_pre_post <- df_annual |>
  filter(`Time period` %in% c(
    "Apr 2017 to Mar 2018 [b]",
    "Apr 2018 to Mar 2019",
    "Apr 2019 to Mar 2020",
    "Apr 2022 to Mar 2023 [p] [r]",
    "Apr 2023 to Mar 2024 [b] [p] [r]", 
    "Apr 2024 to Mar 2025 [p]"
  ))

# To focus on recent patterns, only use 2023-2024

Complaints <- df_annual %>% 
  filter(`Time period` == "Apr 2023 to Mar 2024 [b] [p] [r]" )

# To remove z value (Data not applicable)

Complaints <- Complaints %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )
  
# To remove annotation in Time Period column

Complaints<-Complaints %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

#To change the table format, from wide to long format, new column become operator

RateOfComplaint <- Complaints |>
  pivot_longer(
    -`Time period`,
    names_to  = "Operator",
    values_to = "TotalComplaintRate"
  )

RateOfComplaint <- select(RateOfComplaint, Operator, TotalComplaintRate)

#To remove annotation in operator column
RateOfComplaint <- RateOfComplaint %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

# To change the numeric for total complaint rate column which is character

RateOfComplaint$TotalComplaintRate <- as.numeric(RateOfComplaint$TotalComplaintRate)

# To identify highest and lowest complaint for the operators 

MaxComplaintPlot <- RateOfComplaint %>%
  slice_max(TotalComplaintRate, n = 5)

MinComplaintPlot <- RateOfComplaint %>%
  slice_min(TotalComplaintRate, n = 5)

#To visualise the pattern, it can easy to identify the pattern
# Complaint rate is continuous, So, col chart is more suitable than bar chart

ggplot(MaxComplaintPlot, aes(x = reorder(Operator, TotalComplaintRate),
                     y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Highest Complaint Rates (Apr 2023–Mar 2024)",
       caption = "ORR Dataset") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )

ggplot(MinComplaintPlot, aes(x = reorder(Operator, TotalComplaintRate),
                     y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Lowest Complaint Rates (Apr 2023–Mar 2024)",
       caption = "ORR Dataset") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )


# For the another data (Annual Total Delay)

#To load data into R
x <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200d : Delay minutes, annual data

i <- which(x[[1]] == "Table 2200d: Delay minutes, annual data")

# Extract table (header + data)
tbl <- x[(i + 1):(i + 20), ]  

# Cleaning 
tbl <- tbl[, colSums(!is.na(tbl)) > 0]
colnames(tbl) <- tbl[1, ]
DelayMinutesAnnual <- tbl[-1, ]

View(DelayMinutesAnnual)

#To remove the NA rows 

Delay <- DelayMinutesAnnual[(1:15),]

#Use only 2023-2024 data, identiy recent patterns

Delay <- filter(Delay, `Time period` == "Apr 2023 to Mar 2024 [r]")

# For delay minutes, have three measurements, aggregate these measurement

SumedDelay <- Delay %>%
  mutate(
    across(
      -c(`Time period`, Measure),
      ~ as.numeric(as.character(.))
    )
  )

SumedDelay <- SumedDelay %>%
  select(-`Time period`, -Measure) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


CleanedDelay <- SumedDelay  %>%
  pivot_longer(
    everything(),
    names_to = "Operator",
    values_to = "TotalDelayMinutes"
  )

TotalDelay2023 <- CleanedDelay %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#To plot the data
TestDelayData <- TotalDelay2023 %>%
  slice_max(TotalDelayMinutes, n = 5)

TestDelayData2 <- TotalDelay2023 %>%
  slice_min(TotalDelayMinutes, n = 5)


#Combine two dataset
#Used inner_join to match both two dataframe

CombinedData <- inner_join(RateOfComplaint,TotalDelay2023, by = "Operator")


#Visulise combined dataset to identify the pattern


ggplot(CombinedData,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           size = TotalComplaintRate,
           color = TotalComplaintRate)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(4, 20), guide = "none") +
  labs(
    x = "Total Delay Minutes",
    y = "Complaint Rate per 100,000 journeys",
    color = "Complaint Rate",
    title = "Delays and Complaint Rates by Train Operator",
    caption = "ORR Dataset"
  ) +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 13),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )

# I found the groupping pattern, So, I tested clustering only two varaibles

#Use Clustering Method

#Data Preparation

ClusterData <- CombinedData %>%
  select(TotalDelayMinutes, TotalComplaintRate)

#Scaling (Delay and Complaints are different scale)
# Using z-score normalisation

ScaledData <- scale(ClusterData)

#Used k-means clustering algorithm

set.seed(123)
#Based on the above pattern, I tested with 4 centroid.

KmeansData <- kmeans(ScaledData, centers = 4, nstart = 25)

#Convert the cluster numbers into categories 

CombinedData$Cluster <- factor(KmeansData$cluster)


ggplot(CombinedData,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           color = Cluster)) +
  geom_point(size = 4) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    title = "Groupping the Operators Based on Delay and Complaint Rate",
    x = "Total Delay Minutes",
    y = "Total Complaint Rate"
  ) +
  theme_minimal()


# Load sheet 3 

x_cancel <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200c: Punctuality and reliability, annual data

j <- which(x_cancel[[1]] ==
             "Table 2200c: Punctuality and reliability, annual data")

# Extract table (header + data)

tbl_cancel <- x_cancel[(j + 1):(j + 20), ]

# Clean
tbl_cancel <- tbl_cancel[, colSums(!is.na(tbl_cancel)) > 0]
colnames(tbl_cancel) <- tbl_cancel[1, ]

CancellationAnnual <- tbl_cancel[-1, ]

CancellationAnnual <- CancellationAnnual[(1:5),]

CancellationAnnual <- filter(CancellationAnnual, `Time period` == "Apr 2023 to Mar 2024")

#To convert the format wider to longer

CancellationPercent <- CancellationAnnual %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

# To combined the data, I want only Operator and TotaCancel Percent)

CancellationPercent23 <- select(CancellationPercent, Operator, TotalCancelPercent)

CancellationPercent23 <- CancellationPercent23 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

VisualCancel <- CancellationPercent23 %>%
  slice_max(TotalCancelPercent, n=5)

VisualCancel2 <- CancellationPercent23 %>%
  slice_min(TotalCancelPercent, n=5)

#Combining three Dataset, Delay + Complaint + Cancellation

CombinedDataframe <- RateOfComplaint %>%
  inner_join(TotalDelay2023, by = "Operator") %>%
  inner_join(CancellationPercent23, by = "Operator")

ggplot(CombinedDataframe,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           fill = TotalCancelPercent)) +
  geom_point(
    shape = 21,           
    size = 3.5,
    color = "black",    
    alpha = 0.8
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Cancellation Percent"
  ) +
  labs(
    x = "Total Delay Minutes",
    y = "Complaint Rate per 100,000 journeys",
    title = "Complaint Rate, Delay Minutes and Cancellations Percent By Operator",
    caption = "ORR Dataset"
  ) +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 13),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )



#Use Clustering Method,
#Data Preparation

ClusterData2023 <- CombinedDataframe %>%
  select(TotalCancelPercent,TotalDelayMinutes, TotalComplaintRate)

#Scaling the variables, measurement are different

ScaledVariables <- scale(ClusterData2023)

#To get the same clusters every time, set.seed (123) and set,seed

set.seed(123)

ClusteredResult <- kmeans(ScaledVariables, centers = 4, nstart = 25)

CombinedDataframe$Cluster <- factor(
  ClusteredResult$cluster,
  levels = c(1, 2, 3, 4),
  labels = c("Group 1", "Group 2", "Group 3", "Group 4")
)

#To Visualise the cluster dataset
#For three dimensional visualisation, I used plot_ly, can move easily 
#and can check easily the cluster and I want to use hover, so, decided to use plot_ly instead of ggplot

plot_ly(
  CombinedDataframe,
  x = ~TotalDelayMinutes,
  y = ~TotalComplaintRate,
  z = ~TotalCancelPercent,
  color = ~Cluster,
  colors = c("#0072B2", "#E69F00", "#009E73", "red"),
  type = "scatter3d",
  mode = "markers",
  text = ~paste(
    "Operator:", Operator,
    "<br>Delay Minutes:", sprintf("%.2f", TotalDelayMinutes),
    "<br>Complaint Rate:", sprintf("%.2f", TotalComplaintRate),
    "<br>Cancellation Percent:", sprintf("%.2f", TotalCancelPercent)
  ),
  hoverinfo = "text",
  marker = list(size = 6)
) %>%
  layout(
    title = "<b> Grouping the Operators  in Apr 2023 to Mar 2024</b>",
    legend = list(
      title = list(text = "Cluster Group")
    ),
    scene = list(
      xaxis = list(title = "Total Delay Minutes"),
      yaxis = list(title = "Total Complaint Rate"),
      zaxis = list(title = "Cancellation Percentage")
    )
  )

#To compare the the consistency of operators' characteristics
#For 2024 to 2025 

filepath_2024 <- "4113-complaint-rate-by-operator.ods"
Complaint2024 <- read_ods(
  path  = filepath_2024,
  sheet = 3,   
  skip  = 5 
)

Complaint2024 <- Complaint2024 [(0:18),]

# To remove the annotation from Time Period column

Complaint2024 <- Complaint2024 %>%
  mutate(`Time period` = str_trim(str_remove_all(`Time period`, "\\[.*?\\]")))

#only use 2024-2025
Complaint2024 <- Complaint2024%>%
  filter(`Time period` == "Apr 2024 to Mar 2025")

#Remove z (data not applicable)

Complaint2024 <- Complaint2024 %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

#convert the format, wider to longer

Complaint2024 <- Complaint2024 %>%
  pivot_longer(
    cols = -c (`Time period`),
    names_to = "Operator",
    values_to = "TotalComplaintRate"
  ) 

# want to remove Time period column 
Complaint2024 <- select(Complaint2024, "Operator", "TotalComplaintRate") %>%
  mutate(TotalComplaintRate = as.numeric(TotalComplaintRate))

# To remove annotation from Opeartor column
Complaint2024 <- Complaint2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#Summary the data for 2024-25, which are highest or lowest, same with last year?

HighComplaintPlot2024 <- Complaint2024 %>%
  slice_max(TotalComplaintRate, n = 5)

LowComplaintPlot2024 <- Complaint2024 %>%
  slice_min (TotalComplaintRate, n = 5)


#To load data into R (TotalDelayAnualData)

x2024 <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200d : Delay minutes, annual data

z <- which(x2024[[1]] == "Table 2200d: Delay minutes, annual data")

# Extract table (header + data)
tbl2024 <- x2024[(z + 1):(z + 20), ]

# Clean
tbl2024 <- tbl2024[, colSums(!is.na(tbl2024)) > 0]
colnames(tbl2024) <- tbl2024[1, ]

Delay2024 <- tbl2024[-1, ]

Delay2024<- Delay2024[(1:15),]

Delay2024 <- filter(Delay2024, `Time period` == "Apr 2024 to Mar 2025")

SumedDelay2024 <- Delay2024 %>%
  mutate(
    across(
      -c(`Time period`, Measure),
      ~ as.numeric(as.character(.))
    )
  )

SumedDelay2024 <- SumedDelay2024 %>%
  select(-`Time period`, -Measure) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

SumedDelay2024 <- SumedDelay2024  %>%
  pivot_longer(
    everything(),
    names_to = "Operator",
    values_to = "TotalDelayMinutes"
  )

SumedDelay2024 <- SumedDelay2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

PlotDelay24 <- SumedDelay2024 %>%
  slice_max(TotalDelayMinutes, n =5)

# Load sheet 3 

x_canceldata <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# Find start of Table 2200c: Punctuality and reliability, annual data
c <- which(x_canceldata[[1]] ==
             "Table 2200c: Punctuality and reliability, annual data")

# Extract table (header + data)
tbl_cancel2024 <- x_canceldata[(c + 1):(c + 20), ]   # increase 20 if needed

# Clean
tbl_cancel2024 <- tbl_cancel2024[, colSums(!is.na(tbl_cancel2024)) > 0]
colnames(tbl_cancel2024) <- tbl_cancel2024[1, ]

Cancel2024 <- tbl_cancel2024[-1, ]


Cancel2024 <- Cancel2024[(1:5),]

Cancel2024 <- filter(Cancel2024, `Time period` == "Apr 2024 to Mar 2025" )

CancelPercent2024 <- Cancel2024 %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

CancelPercent2024 <- select(CancelPercent2024, "Operator", "TotalCancelPercent")

CancelPercent2024 <- CancelPercent2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

MinDataCancelPlot2024 <- CancelPercent2024 %>%
  slice_min(TotalCancelPercent, n = 5)

MaxDataCancelPlot2024 <- CancelPercent2024 %>%
  slice_max(TotalCancelPercent, n = 5)


CombinedDataframe2024 <- Complaint2024 %>%
  inner_join(SumedDelay2024, by = "Operator") %>%
  inner_join(CancelPercent2024, by = "Operator")

#Use Clustering Method,
#Data Preparation

ClusterData2024 <- CombinedDataframe2024 %>%
  select(TotalComplaintRate,TotalDelayMinutes, TotalCancelPercent)

#Scaling the variables, measurement are different

ScaledVariables2024 <- scale(ClusterData2024)

set.seed(123)

# For this, I selected similarly centroid 4, but it is not enough seperation and
#homogeneity

ClusteredResult2024 <- kmeans(ScaledVariables2024, centers = 3, nstart = 25)

CombinedDataframe2024$Cluster <- factor(
  ClusteredResult2024$cluster,
  levels = c(1, 2, 3),
  labels = c("Group 1", "Group 2", "Group 3")
)

#Similarly I used 3D plot

plot_ly(
  CombinedDataframe2024,
  x = ~TotalDelayMinutes,
  y = ~TotalComplaintRate,
  z = ~TotalCancelPercent,
  color = ~Cluster,
  colors = c("red", "blue", "purple"),
  type = "scatter3d",
  mode = "markers",
  text = ~paste(
    "Operator:", Operator,
    "<br>Delay Minutes:", sprintf("%.2f", TotalDelayMinutes),
    "<br>Complaint Rate:", sprintf("%.2f", TotalComplaintRate),
    "<br>Cancellation Percent:", sprintf("%.2f", TotalCancelPercent)
  ),
  hoverinfo = "text",
  marker = list(size = 6)
) %>%
  layout(
    title = "<b> Grouping the Operators  in Apr 2024 to Mar 2025 </b>",
    legend = list(
      title = list(text = "Cluster Group")
    ),
    scene = list(
      xaxis = list(title = "Total Delay Minutes"),
      yaxis = list(title = "Total Complaint Rate"),
      zaxis = list(title = "Cancellation Percentage")
    )
  )

#To find the correlation
# For the accuracy of correlation I used more observations
#So, I prepared the data again

#For Camplaint Dataframe

Newfilepath <- "4113-complaint-rate-by-operator.ods"
NewComplaint <- read_ods(
  path  = Newfilepath,
  sheet = 3,  # only need third sheet
  skip  = 5    
)
# Used annual data, removing unused row

NewComplaint <- NewComplaint [(0:18),]

# Use three years, 2022-23,23-24,24-25, remove other rows

NewComplaint <- NewComplaint [(16:18),]

NewComplaint <- NewComplaint %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

NewComplaint <- NewComplaint %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

NewComplaint <- NewComplaint |>
  pivot_longer(
    -`Time period`,
    names_to  = "Operator",
    values_to = "TotalComplaintRate"
  )

NewComplaint <- NewComplaint %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )%>%
  mutate(TotalComplaintRate = as.numeric(TotalComplaintRate))


#For Delay Dataframe

NewDelay <- tbl [-1,]

NewDelay <- NewDelay[(7:15),]

NewDelay <- NewDelay %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

#Aggregate three measurement

SumedNewDelay <- NewDelay %>%
  group_by(`Time period`) %>%
  summarise(
    across(
      -Measure,
      ~ sum(as.numeric(.), na.rm = TRUE)
    )
  )

DelayDataframe <- SumedNewDelay %>%
  pivot_longer(
    cols = -`Time period`,
    names_to = "Operator",
    values_to = "TotalDelayMinutes"
  )

DelayDataframe <- DelayDataframe %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

# For Cancel percent Dataframe

NewCancel <- tbl_cancel2024[-1,]

NewCancel <- NewCancel[(1:5),]

NewCancel <- NewCancel[(3:5),]

#Similar way with the above code, convert table format, wider to longer

NewCancel<- NewCancel %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

NewCancel <- select(NewCancel, `Time period`,"Operator", "TotalCancelPercent")

NewCancel <- NewCancel %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#Combining Dataframe

NewDataFrame <- NewComplaint %>%
  inner_join(DelayDataframe, by = c("Time period","Operator")) %>%
  inner_join(NewCancel, by = c("Time period","Operator"))

#Check Relationship delay and cancel, linerar or not, to check which correlation method need to use

ggplot(NewDataFrame,
       aes(x = TotalDelayMinutes,
           y = TotalCancelPercent)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Total Delay Minutes",
    y = "Total Cancel Percent",
    title = "The Correlation of Delay Minutes and Cancel Percent",
    caption = "ORR Dataset"
  )+
  theme_minimal()

ggplot(NewDataFrame,
       aes(x = TotalCancelPercent,
           y = TotalComplaintRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Total Cancel Percent",
    y = "Complaint Rate per 100,000 journeys",
    title = "The Correlation of Complaint Rate and Cancel Percent",
    caption = "ORR Dataset"
  )+
  theme_minimal()

ggplot(NewDataFrame,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Total Delay Minutes",
    y = "Complaint Rate per 100,000 journeys",
    title = "The Correlation of Complaint Rate and Delay Minutes",
    caption = "ORR Dataset"
  )+
  theme_minimal()


#No linear, have outliers data

#Finding Correlation (Use Spearman)

#For the Complaint and Cancel

RelationResult <- cor.test(NewDataFrame$TotalComplaintRate,
                           NewDataFrame$TotalCancelPercent,
                           method = "spearman")

RelationResult

# p-value = 0.026, rho = 0.26(positive monotonic relationship)

# Finding Correlation Between Complaint and Delay

RelationDelay <- cor.test(NewDataFrame$TotalComplaintRate,
                          NewDataFrame$TotalDelayMinutes,
                          method = "spearman")
RelationDelay

#p = 0.1, rho = -0.18

#Finding Correlation between Delay and Cancellation rate

DelayAndCancel <- cor.test (NewDataFrame$TotalDelayMinutes,
                            NewDataFrame$TotalCancelPercent,
                            method = "spearman")
DelayAndCancel
#p-value = 0.0001611, rho = 0.43



# The relation of variables are not linear.
#used Mulitiple linear regression as an exploratory tool,
#To indentify how much the variance of delay and cancel in complaint

#Finding linear regression

# Used Total Complaint as an outcome, delay and cancel are features

LinearResults <- lm(TotalComplaintRate ~ TotalDelayMinutes + TotalCancelPercent,
                  data = NewDataFrame)

summary(LinearResults)




