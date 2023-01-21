library(ggplot2)
library(qqplotr)
library(tidyverse)
library(repr)
library(tidymodels)
library(stringr)
library(leaps)

data <- read.csv(url("https://drive.google.com/uc?export=download&id=1_MECmUXZuuILYeEOfonSGqodW6qVdhsS"))

data <- mutate(data, str_replace(data$Age, " \\s*\\([^\\)]+\\)", ""))
data <- mutate(data, Age = str_replace(data$Age, " \\s*\\([^\\)]+\\)", ""))
data <- mutate(data, Current.Rank = str_replace(data$Current.Rank, "\\s*\\([^\\)]+\\)", ""))
data <- mutate(data, Best.Rank = str_replace(data$Best.Rank, "\\s*\\([^\\)]+\\)", ""))
money <- c(data$Prize.Money)
money <- money %>%
lapply(gsub, pattern="$", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="US", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="all-time leader in earnings", fixed=TRUE,replacement="") %>%
lapply(gsub, pattern="All-time leader in earnings", fixed=TRUE,replacement="") %>%
lapply(gsub, pattern="all-time in earnings", fixed=TRUE,replacement="") %>%
lapply(gsub, pattern="11th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="24th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="10th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="14th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="2nd", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="27th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="15th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="30th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="4th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="28th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="6th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="33rd", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="26th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="24th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="48th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="41st", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="24th", fixed=TRUE, replacement="") %>%
lapply(gsub, pattern="15th", fixed=TRUE, replacement="")
data_selected <- data %>%mutate(data, Prize.Money = money) %>%
select(Age, Name,Country, Current.Rank, Best.Rank, Prize.Money,Seasons) %>%
mutate(Prize.Money = gsub(",","", Prize.Money))
tidy_data <- data_selected %>% filter(Prize.Money != "") %>%
mutate(Prize.Money = as.numeric(Prize.Money)) %>%
mutate(Age = as.numeric(Age)) %>%
mutate(Current.Rank = as.numeric(Current.Rank)) %>%
mutate(Best.Rank = as.numeric(Best.Rank)) %>%
mutate(Seasons = as.numeric(Seasons))
tidy_data <- drop_na(tidy_data)

# Find average prize money for each country's players
table1 <- tidy_data %>%group_by(Country) %>%
summarize(avg_award_in_USD = mean(Prize.Money))
avg_award_in_USD <- table1$avg_award_in_USD
# count each country's number of players and then bind the data with the average prize money column from above
final_table <- tidy_data %>% group_by(Country) %>% summarize(n = n()) %>%
bind_cols(avg_award_in_USD) %>% mutate(avg_award_in_USD = ...3) %>%
select(-...3)
# Find top 10 country with the most players
top_10 <- final_table %>% arrange(n) %>% tail(10)
# Plot the number of players for each top 10 country
top_10_graph <- ggplot(top_10,aes(x = Country, y = n)) +
geom_bar(stat = "identity") + labs(x = "Country", y = "Number of People in Top 500 Tennis Players") +
ggtitle("Top 10 Countries with most People in Top 500 Tennis Players") + coord_flip()
top_10_names <- pull(top_10, Country)

# Start working on Top 10
top_10_data <- tidy_data %>%filter(Country == "United States"| Country == "United Kingdom"
| Country == "Spain" |Country == "Russian Federation" | Country == "Japan" |Country == "Italy" |
Country == "Germany" | Country == "France" | Country =="Australia"| Country == "Argentina") %>%
select(-Name)
# averaged Best Rank
top_10_mean_money_over_rank <- top_10_data %>%
group_by(Best.Rank) %>%
summarize(mean_Prize_Money = mean(Prize.Money))
top_10_data <- filter(top_10_data, Prize.Money != 61544007 & Prize.Money != 119601561)

# Start working on top 100 ranked players
top_10_data_top_100 <- filter(top_10_data, Best.Rank <= 100)
options(repr.plot.width = 30, repr.plot.height = 15)
bestRank_over_money_plot <- ggplot(top_10_data_top_100, aes(x = Best.Rank, y=Prize.Money)) +
geom_point(size = 4) + labs(x = "Best Rank of Tennis Player", y = "Prize Earnings (in USD)") +
scale_y_continuous(labels = dollar_format()) + ggtitle("Best Rank Influencing Prize Earnings") +
theme(text = element_text(size = 20))
bestRank_over_money_plot

#After removing those two outliers
# Prize Money = Age + Seasons + Best Rank + Best Rank^2 + Age * Best Rank
simple_model <- lm(Prize.Money~Age+Best.Rank+Seasons, data = top_10_data_top_100)
simple_model_summary <- summary(simple_model)
simple_residual_plot <- ggplot(simple_model, aes(x = fitted.
values(simple_model), y = residuals(simple_model))) +
geom_point(size = 4) +
labs(x = "Fitted Earnings (in USD)", y = "Residuals") +
scale_y_continuous(labels = dollar_format()) +
ggtitle("Residual Plot for Simple Model")
theme(text = element_text(size = 20))
simple_normal_plot <- ggplot(simple_model, mapping = aes(sample =residuals(simple_model))) +
stat_qq_point(size = 2) +
ggtitle("Normal Plot for Simple Model") +
theme(text = element_text(size = 20))
AIC_simple <- AIC(simple_model)
BIC_simple <- BIC(simple_model)
simple_model_summary
simple_residual_plot
simple_normal_plot
AIC_simple
BIC_simple
