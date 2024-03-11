library(tidyverse)


chat_df <- readxl::read_xlsx(here::here("week 10/ChatGPT_Practice_Dataset.xlsx")) 

chat_df <- separate(chat_df, "UniqueLabel,Name,Position,State,Salary,Years_Worked,Performance_Last_Year,Performance_2_Years_Ago", 
                    into = c("UniqueLabel", "Name", "Position", "State", "Salary","Years_Worked","Performance_Last_Year","Performance_2_Years_Ago"), 
                    sep = ",") %>% 
  mutate(across(5:8, as.numeric))

# WHos performance and compensation are not comensorrate 

evaluate <- chat_df %>% 
  mutate(average_performance = (Performance_2_Years_Ago + Performance_Last_Year)/2)


ggplot(chat_df, aes(x = Position, y = Salary)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Salary Distribution by Position", x = "Position", y = "Salary")

anova_result <- aov(Salary ~ Position, data = chat_df)

# Print ANOVA summary
summary(anova_result)

# If the p-value is less than a chosen significance level (e.g., 0.05), we reject the null hypothesis
# and conclude that there are significant differences in salaries among positions.

# Print p-value
p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
if (p_value < 0.05) {
  print("There are significant differences in salaries among positions.")
} else {
  print("There is no significant difference in salaries among positions.")
}

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Print Tukey's HSD results
print(tukey_result)

# Plot Tukey's HSD results
plot(tukey_result)
