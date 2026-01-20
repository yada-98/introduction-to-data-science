library(tidyverse)
library(readODS)
library(janitor)

#Load data into R

filePath <- "4113-complaint-rate-by-operator.ods"
df_complaints <- read_ods(
  path  = filePath,
  sheet = 3,   
  skip  = 5    
)

df_annual <- df_complaints [(0:18),]

df_uncleaned_pre_post <- df_annual |>
  filter(`Time period` %in% c(
    "Apr 2017 to Mar 2018 [b]",
    "Apr 2018 to Mar 2019",
    "Apr 2019 to Mar 2020",
    "Apr 2022 to Mar 2023 [p] [r]",
    "Apr 2023 to Mar 2024 [b] [p] [r]", 
    "Apr 2024 to Mar 2025 [p]"
  ))


Complaints <- df_annual %>% 
  filter(`Time period` == "Apr 2023 to Mar 2024 [b] [p] [r]" )

Complaints <- Complaints %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )
  
Complaints<-Complaints %>%
  mutate(
    `Time period` = str_remove_all(`Time period`, "\\s*\\[.*?\\]")
  )

RateOfComplaint <- Complaints |>
  pivot_longer(
    -`Time period`,
    names_to  = "Operator",
    values_to = "TotalComplaintRate"
  )

RateOfComplaint <- select(RateOfComplaint, Operator, TotalComplaintRate)

RateOfComplaint <- RateOfComplaint %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )
RateOfComplaint$TotalComplaintRate <- as.numeric(RateOfComplaint$TotalComplaintRate)

DataPlot <- RateOfComplaint %>%
  slice_max(TotalComplaintRate, n = 5)

DataPlot2 <- RateOfComplaint %>%
  slice_min(TotalComplaintRate, n = 5)


ggplot(DataPlot, aes(x = reorder(Operator, TotalComplaintRate),
                     y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Highest Complaint Rates (Apr 2023–Mar 2024)") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )

ggplot(DataPlot2, aes(x = reorder(Operator, TotalComplaintRate),
                     y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Lowest Complaint Rates (Apr 2023–Mar 2024)") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )



# For the another data

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

# Clean
tbl <- tbl[, colSums(!is.na(tbl)) > 0]
colnames(tbl) <- tbl[1, ]
DelayMinutesAnnual <- tbl[-1, ]

View(DelayMinutesAnnual)

Delay <- DelayMinutesAnnual[(1:15),]

Delay <- filter(Delay, `Time period` == "Apr 2023 to Mar 2024 [r]")

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
CleanedDelay <- CleanedDelay %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

#To plot the data
TestPlotData <- CleanedDelay %>%
  slice_max(TotalDelayMinutes, n = 5)

TestPlotData2 <- CleanedDelay %>%
  slice_min(TotalDelayMinutes, n = 5)

ggplot(TestPlotData, aes(x = reorder(Operator, TotalDelayMinutes),
             y = TotalDelayMinutes)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","))+
  labs(x = "Operator", y = "Total delay minutes", title = "Top operators by total delay minutes") +
  theme_minimal()


#Combine two dataset

CombinedData <- inner_join(CleanedDelay, RateOfComplaint, by = "Operator")

Combine <- inner_join(RateOfComplaintC, CancellationPercent, by = "Operator")

DelayTop5 <- CombinedData %>%
  slice_max(TotalDelayMinutes, n = 5)

ggplot(DelayTop5,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           fill = Operator)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    x = "Total Delay Minutes",
    y = "Total Complaint Rate",
    title = "Top 5 Operators: Delay vs Complaint Rate",
    fill = "Operator"
  ) +
  theme_minimal()

#Visulise for all data

ggplot(CombinedData,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           fill = Operator)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    x = "Total Delay Minutes",
    y = "Total Complaint Rate",
    title = "Delay vs Complaint Rate by Operator"
  ) +
  theme_minimal() +
  guides(fill = "none")


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
    title = "Delays and Complaint Rates by Train Operator"
  ) +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 13),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )



#Use Clustering Method

#Data Preparation

ClusterData <- CombinedData %>%
  select(TotalDelayMinutes, TotalComplaintRate)

#Scaling (Delay and Complaints are different measurement)

ScaledData <- scale(ClusterData)

set.seed(123)

KmeansData <- kmeans(ScaledData, centers = 4, nstart = 25)

CombinedData$Cluster <- factor(KmeansData$cluster)

aggregate(
  CombinedData[, c("TotalDelayMinutes", "TotalComplaintRate")],
  by = list(Cluster = CombinedData$Cluster),
  mean
)

ggplot(CombinedData,
       aes(x = TotalDelayMinutes,
           y = TotalComplaintRate,
           color = Cluster)) +
  geom_point(size = 4) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    title = "Operator Clusters Based on Delay and Complaint Rate",
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

# Find start of Table 2200c: Punctuality and reliability, annual data
j <- which(x_cancel[[1]] ==
             "Table 2200c: Punctuality and reliability, annual data")

# Extract table (header + data)
tbl_cancel <- x_cancel[(j + 1):(j + 20), ]   # increase 20 if needed

# Clean
tbl_cancel <- tbl_cancel[, colSums(!is.na(tbl_cancel)) > 0]
colnames(tbl_cancel) <- tbl_cancel[1, ]

CancellationAnnual <- tbl_cancel[-1, ]

# View result
View(CancellationAnnual)

CancellationAnnual <- CancellationAnnual[(1:5),]

CancellationAnnual <- filter(CancellationAnnual, `Time period` == "Apr 2023 to Mar 2024")


CancellationPercent <- CancellationAnnual %>%
  pivot_longer(
    cols = -c (`Time period`, Measure),
    names_to = "Operator",
    values_to = "TotalCancelPercent"
  ) %>%
  mutate(TotalCancelPercent = as.numeric(TotalCancelPercent))

CancellationPercent <- select(CancellationPercent, Operator, TotalCancelPercent)

CancellationPercent <- CancellationPercent %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

VisualCancel <- CancellationPercent %>%
  slice_max(TotalCancelPercent, n=5)

VisualCancel2 <- CancellationPercent %>%
  slice_min(TotalCancelPercent, n=5)

ggplot(VisualCancel, aes(x = reorder(Operator, TotalCancelPercent),
                         y = TotalCancelPercent, fill = Operator)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","))+
  labs(x = "Operator", y = "Cancel Percentage", title = "Top operators by Cancellation Percentage") +
  theme_minimal()



#Combining three Dataset, Delay + Complaint + Cancellation

CombinedDataframe <- RateOfComplaint %>%
  inner_join(CleanedDelay, by = "Operator") %>%
  inner_join(CancellationPercent, by = "Operator")

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
    title = "Complaint Rate, Delay Minutes and Cancellations Percent By Operator"
  ) +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 13),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )



#Use Clustering Method,
#Data Preparation

ClusterData2 <- CombinedDataframe %>%
  select(TotalCancelPercent,TotalDelayMinutes, TotalComplaintRate)

#Scaling the variables, measurement are different

ScaledVariables <- scale(ClusterData2)

set.seed(123)

ClusteredResult <- kmeans(ScaledVariables, centers = 4, nstart = 25)

CombinedDataframe$Cluster <- factor(
  ClusteredResult$cluster,
  levels = c(1, 2, 3, 4),
  labels = c("Group 1", "Group 2", "Group 3", "Group 4")
)


install.packages("plotly")   
library(plotly)

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

#For 2024 to 2025 

filepath_2024 <- "4113-complaint-rate-by-operator.ods"
Complaint2024 <- read_ods(
  path  = filepath_2024,
  sheet = 3,   # third sheet (1-based index)
  skip  = 5    # skip first 5 rows
)

Complaint2024 <- Complaint2024 [(0:18),]

Complaint2024 <- Complaint2024 %>%
  mutate(`Time period` = str_trim(str_remove_all(`Time period`, "\\[.*?\\]")))

Complaint2024 <- Complaint2024%>%
  filter(`Time period` == "Apr 2024 to Mar 2025")

Complaint2024 <- Complaint2024 %>%
  select(
    where(~ !any(. == "[z]", na.rm = TRUE))
  )

Complaint2024 <- Complaint2024 %>%
  pivot_longer(
    cols = -c (`Time period`),
    names_to = "Operator",
    values_to = "TotalComplaintRate"
  ) 

Complaint2024 <- select(Complaint2024, "Operator", "TotalComplaintRate") %>%
  mutate(TotalComplaintRate = as.numeric(TotalComplaintRate))

Complaint2024 <- Complaint2024 %>%
  mutate(
    Operator = str_remove(Operator, "\\s*\\(.*\\)") %>%  
      str_remove("\\s*\\[.*\\]") %>%          
      str_trim()
  )

MaxPlot2024 <- Complaint2024 %>%
  slice_max(TotalComplaintRate, n = 5)

MinPlot2024 <- Complaint2024 %>%
  slice_min (TotalComplaintRate, n = 5)

ggplot(MaxPlot2024, aes(x = reorder(Operator, TotalComplaintRate),
                     y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Highest Complaint Rates (Apr 2024–Mar 2025)") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )

ggplot(MinPlot2024, aes(x = reorder(Operator, TotalComplaintRate),
                        y = TotalComplaintRate, fill = Operator)) +
  geom_col(width = 0.5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  labs(x = "Operator", y = "Complaint Rate per 100,000 journeys", 
       title = "Five Operators with the Lowest Complaint Rates (Apr 2024–Mar 2025)") +
  theme(
    plot.title   = element_text(size = 15, face = "bold"),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 10),
    plot.caption = element_text(size = 15)
  )


#To load data into R
x2024 <- read_ods(
  "table-2200_key_statistics_by_operator.ods",
  sheet = 3,
  col_names = FALSE
)

# To Find start of Table 2200d : Delay minutes, annual data

z <- which(x2024[[1]] == "Table 2200d: Delay minutes, annual data")

# Extract table (header + data)
tbl2024 <- x2024[(z + 1):(z + 20), ]   # increase 20 if needed

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
DataCancelPlot <- CancelPercent2024 %>%
  slice_min(TotalCancelPercent, n = 5)


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

ClusteredResult2024 <- kmeans(ScaledVariables2024, centers = 3, nstart = 25)

CombinedDataframe2024$Cluster <- factor(
  ClusteredResult2024$cluster,
  levels = c(1, 2, 3),
  labels = c("Group 1", "Group 2", "Group 3")
)


install.packages("plotly")   
library(plotly)

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


# Spearman correlation

FindCorrelation <- cor.test(CombinedDataframe2024$TotalComplaintRate,
                       CombinedDataframe2024$TotalCancelPercent,
                       method = "spearman")

#To find the correlation

#For Camplaint Dataframe

Newfilepath <- "4113-complaint-rate-by-operator.ods"
NewComplaint <- read_ods(
  path  = Newfilepath,
  sheet = 3,   # third sheet (1-based index)
  skip  = 5    # skip first 5 rows
)

NewComplaint <- NewComplaint [(0:18),]
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

#Check Relationship y and x
ggplot(NewDataFrame,
       aes(x = TotalDelayMinutes,
           y = TotalCancelPercent)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()


#No linear, have outliers data

#Finding Correlation (Use Spearman)

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



#Finding linear regression

LinearCheck <- lm(TotalComplaintRate ~ TotalDelayMinutes + TotalCancelPercent,
                  data = NewDataFrame)

summary(LinearCheck)

par (mfrow = c(2,2))
plot(LinearCheck)
par(mfrow = c(1,1))




