install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(ggplot2)

library(polycor)
raw_data <- read.csv("E:\\PFDA Assignment\\student_prediction.csv")

raw_data$ATTEND <- factor(raw_data$ATTEND, levels = 1:3, labels = c("always", "sometimes", "never"))
raw_data$GENDER <- factor(raw_data$GENDER,levels = c(1,2), labels =c("female","male"))
raw_data$GRADE <- factor(raw_data$GRADE,levels = c(0,1,2,3,4,5,6,7), labels =c("Fail","DD", "DC","CC","CB", "BB","BA", "AA"), ordered = TRUE)
raw_data$FATHER_EDU <- factor(raw_data$FATHER_EDU, levels = 1:6, labels = c("primary school", "secondary school", "high school", "university", "MSc.", "Ph.D."))
raw_data$CUML_GPA <- factor(raw_data$CUML_GPA, levels = 1:5, labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49"))
str(raw_data)

#polycor
polycor_matrix <- polychor(raw_data$CUML_GPA, raw_data$ATTEND)
print(polycor_matrix)

#chisquare
rawr <- table(raw_data$ATTEND, raw_data$CUML_GPA)
chisq <- chisq.test(rawr)
print(chisq)

#univariate attend
ggplot(raw_data, aes(x = ATTEND, fill = ATTEND)) +
  geom_bar(fill="red") +
  labs(title="Barchart for ATTEND", x="ATTEND", y="Count")

data_sum <- raw_data %>%
  group_by(ATTEND) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
data_sum
ggplot(data = data_sum,
       aes(x = "",y = percent, fill = ATTEND)) +
  geom_bar(stat = "identity", width = 1, color = NA) + # set color to NA to remove piechart outline
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percent),"%")),
            position = position_stack(vjust = 0.5),
            size = 6) +
  labs(title = "Attendance Pie Chart",
       fill = "Attendance Frequency") +
  scale_fill_manual(values = c("#DD4423","#A32323")) +
  theme_void() + #biar ga keluar 0/100 diluar lingkaran 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = 0.5, 
                                  margin = margin(10, 10, 10, 10)),
        legend.title = element_text(size = 12))


#biavariate

library(ggplot2)
library(dplyr)

# Assuming raw_data contains the necessary columns

# Calculate counts for each combination of ATTEND and CUML_GPA
count_data <- raw_data %>%
  count(ATTEND, CUML_GPA) %>%
  group_by(ATTEND) %>%
  mutate(percentage = prop.table(n) * 100)

ggplot(data = count_data, aes(x = ATTEND, y = n, fill = CUML_GPA)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) + # Add count labels
  labs(title = "Bivariate Barchart for Student Attendance and Cumulative GPA", x = "ATTEND", y = "Count") +
  facet_grid(~ CUML_GPA)



#multivariate

library(ggplot2)

library(ggplot2)

ggplot(raw_data, aes(x = CUML_GPA, fill = GENDER)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5)) + # Add count labels in the middle
  labs(title = "Attendance and Gender by Cumulative GPA", x = "Cumulative GPA", y = "Count") +
  facet_wrap(~ ATTEND, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(tidymodels)

library(ggplot2)

# Assuming 'result_df' contains the Actual and Predicted values

# Installing the package

# For Logistic regression
install.packages("caTools") 

# For ROC curve to evaluate model
install.packages("ROCR")	 

# Loading package
library(caTools)
library(ROCR)

# Load necessary libraries
library(ggplot2)

# Assuming you have your data loaded (replace 'your_data' with your actual data frame)
# Example: your_data <- read.csv("your_data.csv")

# Fit the logistic regression model
logistic_model <- glm(ATTEND ~ NOTES + PARTNER, data = raw_data, family = "binomial")
print(logistic_model)

# Create a new data frame for plotting
newdata <- data.frame(NOTES = seq(min(raw_data$NOTES), max(raw_data$NOTES), len = 500),
                      PARTNER = mean(raw_data$PARTNER))  # Use the mean value of PARTNER

# Predict the response probabilities
newdata$ATTEND <- predict(logistic_model, newdata, type = "response")

# Plot the logistic regression curve
ggplot(raw_data, aes(x = NOTES, y = ATTEND)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))



