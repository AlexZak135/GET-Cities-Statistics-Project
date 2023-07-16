# Title: GET Cities Data Analysis
# Author: Alexander Zakrzeski
# Date: July 16, 2023

# Load the necessary packages

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

library(ggplot2)
library(scales)
library(cowplot)

library(tidytext)
library(ggwordcloud)
library(vader)

# Load the data and filter appropriately

guide_data <- read_excel("Question-Guide.xlsx") |> 
  filter(ID %in% c("SQ2", "SQ3", "SQ4", "SQ5", "SQ7", "SQ9", "SQ10", 
                   "SQ11", "SQ13", "SQ16", "SQ23", "SQ24", "SQ25",
                   "SQ29", "SQ33", "SQ35", "SQ37", "SQ40"))

# Load the data and perform the data preprocessing steps:
  # These steps include renaming columns, filtering, selecting columns, etc.

cwalk_data <- read_excel("ZIP-FIPS-Crosswalk.xlsx") |> 
  rename_all(tolower) |>
  filter(state %in% c("IL", "IN", "WI")) |> 
  select(zip, state) |> 
  mutate(zip = as.character(zip), 
         state = case_when( 
           state == "IL" ~ "Illinois", 
           state == "IN" ~ "Indiana",  
           TRUE ~ "Wisconsin"  
           )) |> 
  distinct(zip, state)

# Load the data and perform the data preprocessing steps: 
  # These steps include sorting rows, merging, modifying values, etc.

quant_data <- read_excel("GETCities-Quantitative-Data.xlsx") |> 
  rename(date_taken = SQ2, 
         identity = SQ3, 
         race = SQ4, 
         residence = SQ5, 
         zip = SQ7, 
         employment = SQ9, 
         parent_caretaker = SQ10, 
         education = SQ11,  
         job_title = SQ13) |> 
  select(date_taken, identity, race, residence, zip, employment, 
         parent_caretaker, education, job_title, SQ16, SQ23, 
         SQ24, SQ25, SQ29, SQ33, SQ35, SQ37, SQ40) |> 
  arrange(date_taken) |>  
  left_join(cwalk_data, by = "zip") |> 
  relocate(state, .before = residence) |> 
  replace_na(list(state = "Illinois")) |> 
  mutate(date_taken = ymd(str_sub(as.character(date_taken), 1, 10)), 
         race = case_when(  
           race %in% c("White, but I am an immigrant",  
                       "White or Caucasian", 
                       "Slavic (Polish)",
                       "Middle Eastern", 
                       "north african") ~ "White",  
           race == "Black or African American" ~ "Black",  
           race == "Asian or Pacific Islander" ~ "Asian",  
           race == "Hispanic or Latino" ~ "Hispanic",   
           race == "Prefer not to reply" ~ "No Response", 
           TRUE ~ "Other" 
           ), 
         residence = str_extract(residence, "\\(.*?\\)") |> 
                     str_remove_all("\\(|\\)"), 
         parent_caretaker = case_when(  
           str_detect(parent_caretaker, "parent|caretaker of adults") ~ "Yes",
           is.na(parent_caretaker) ~ "No Response",  
           TRUE ~ "No" 
           ), 
         education = case_when(
           !str_detect(education, "Master’s degree|Ph.D.") & 
           str_detect(education, "Bachelor’s degree") ~ "Undergraduate Degree", 
           str_detect(education, "Master’s degree|Ph.D.") ~ "Graduate Degree", 
           TRUE ~ "No Degree"
           ), 
         job_title = if_else(
           job_title %in% c("Software Developer",
                            "IT Project Manager",
                            "Graphic Designer", 
                            "Product Manager", 
                            "Data Analyst"), 
           job_title, 
           "Other" 
           ), 
         SQ16 = case_when( 
           SQ16 == "A year from now, I plan to stay in this role" 
           ~ "Plan To Stay In Role",   
           SQ16 == "I plan to exit this role in 12 months or less"  
           ~ "Plan To Leave Role",  
           SQ16 == "I plan to exit technology in 12 months or less" 
           ~ "Plan To Leave Tech",   
           TRUE 
           ~ "No Response"  
           ), 
         SQ23 = replace_na(SQ23, "No Response"), 
         SQ23 = str_to_title(SQ23), 
         SQ24 = case_when( 
           SQ24 %in% c("4 years, between entry and intermediate", 
                       "Entry-level - 1-3 years", 
                       "3-5 years") ~ "Entry-Level", 
           SQ24 %in% "Intermediate level - 5-10 years" ~ "Intermediate", 
           SQ24 %in% "Mid-career - 11-20 years" ~ "Mid-Level", 
           str_detect(SQ24, pattern = "Senior-level|40 years") ~ "Senior", 
           TRUE ~ "No Response"
           ), 
         across(c(SQ25, SQ29, SQ33), ~ case_when( 
           . == "Neutral / Neither agree nor disagree" ~ "Neutral", 
           is.na(.) ~ "No Response",
           TRUE 
           ~ . 
           )), 
         SQ35 = case_when( 
           str_detect(SQ35, "Switching") ~ "Switching Companies",  
           SQ35 == "A promotion" ~ "Promotion", 
           SQ35 == "A raise" ~ "Raise", 
           SQ35 == "A bonus" ~ "Bonus", 
           SQ35 == "N/A" | is.na(SQ35) ~ "No Response",  
           TRUE ~ "Other" 
           ), 
         SQ37 = case_when( 
           SQ37 == "44885.0" ~ "11-20", 
           is.na(SQ37) ~ "No Response", 
           TRUE ~ SQ37
           ),  
         SQ40 = case_when(  
           str_detect(SQ40, "hybrid work|1 work remote day") ~ "Hybrid", 
           str_detect(SQ40, "work remotely|Some are remote") ~ "Remote", 
           SQ40 == "We all work in-person" ~ "In-Person", 
           is.na(SQ40) ~ "No Response",
           TRUE ~ SQ40 
           )) 

# Group by the columns and generate the aggregated figures 

viz1_data <- quant_data |>  
  filter(race != "No Response") |> 
  mutate(race = factor(race, levels = c("Other", 
                                        "Asian", 
                                        "Hispanic", 
                                        "Black",
                                        "White")), 
         education = factor(education, levels = c("Graduate Degree", 
                                                  "Undergraduate Degree", 
                                                  "No Degree"))) |>
  group_by(race, education) |>
  summarize(count = n()) |>
  ungroup() |>
  mutate(pct = count / sum(count)) 

# Create a stacked bar chart

ggplot(viz1_data, aes(x = race, y = pct, fill = education)) +  
  geom_col(position = "stack", width = 0.45) +  
  geom_hline(yintercept = 0, size = 1, color = "black") + 
  geom_text(aes(label = percent(round(after_stat(y), 2)), group = race), 
            size = 4.8, stat = "summary", fun = sum, vjust = -1) + 
  scale_y_continuous(labels = percent, limits = c(0, 0.65), 
                     breaks = seq(0, 0.65, by = 0.15)) + 
  scale_fill_manual(values = c("#004c4c", "#008080", "#b2d8d8")) + 
  labs(title = "Technologists by Race and Education Level", 
       x = "", y = "") +  
  guides(fill = guide_legend(title = "", reverse = TRUE)) +  
  theme_void() + 
  theme(text = element_text(family = "Roboto"),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),  
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 14, color = "black"), 
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")

# Group by the columns and generate the aggregated figures 

viz2_data <- quant_data |> 
  filter(SQ24 != "No Response") |>
  mutate(SQ37 = if_else( 
    SQ37 %in% c("0-10", "11-20", "21-30", "31-40"),
    "40 Hours or Less",
    "More than 40 Hours"  
    )) |>
  group_by(SQ24, SQ37) |>
  summarize(count = n()) |>
  mutate(pct = count / sum(count)) 

# Create a grouped bar chart

ggplot(viz2_data, aes(x = SQ24, y = pct, fill = SQ37)) + 
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  geom_text(aes(label = percent(round(pct, 2)), hjust = 0.5), 
            size = 4.8, position = position_dodge(0.7), vjust = -1) +
  scale_y_continuous(labels = percent, limits = c(0, 0.85), 
                     breaks = seq(0, 0.85, by = 0.2)) + 
  scale_fill_manual(values = c("#b2d8d8", "#008080")) + 
  labs(title = "Technologists by Experience and Avg. Weekly Hours Worked",
       x = "", y = "") +
  guides(fill = guide_legend(title = "")) +  
  theme_void() + 
  theme(text = element_text(family = "Roboto"),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"), 
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 14, color = "black"), 
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")

# Load the data and perform the data preprocessing steps: 
  # These steps include renaming columns, selecting columns, etc.

qual_data <- read_excel("GETCities-Qualitative-Data.xlsx") |> 
  rename_at(vars(2, 3, 4), ~ c("age", "employees", "experience")) |> 
  select(age, experience, employees) |> 
  mutate(age_group = if_else(  
    age < 30, 
    "Younger than 30", 
    "30 Plus" 
    )) |>  
  relocate(age_group, .after = "age") 

# Generate counts for the top thirty-five most frequent unigrams

count1 <- qual_data |> 
  select(experience) |> 
  unnest_tokens(word, experience, token = "ngrams", n = 1) |> 
  anti_join(stop_words, by = "word") |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>  
  mutate(word = str_to_title(word)) |> 
  head(35)
  
# Set a random seed

set.seed(100)

# Create a word cloud 

ggplot(count1, aes(label = word, size = count, color = count)) + 
  geom_text_wordcloud_area() + 
  scale_size_area(max_size = 14) + 
  scale_color_gradient(low = "black", high = "#008080") +
  theme_minimal()
  
# Analyze the sentiment of the responses using VADER

vader1_overall <- vader_df(qual_data$experience)

# Extract just the compound sentiment scores

vader1_compound <- vader1_overall["compound"]

# Add the compound sentiment scores as a new column

qual_data <- qual_data |>
  bind_cols(vader1_compound) |>
  mutate(sentiment_rank = case_when(
    compound > 0.06 ~ "Positive",
    compound < -0.06 ~ "Negative",
    TRUE ~ "Neutral"
    )) |>
  relocate(compound, sentiment_rank, .after = "experience") |>
  arrange(desc(compound))

# Generate the average compound sentiment score

mean(qual_data$compound)

# Create a histogram

ggplot(qual_data, aes(x = compound, fill = sentiment_rank)) + 
  geom_histogram(bins = 20, color = "black") +
  geom_hline(yintercept = 0, size = 0.8, color = "black") +
  scale_x_continuous(labels = label_number(drop0trailing = TRUE), 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_y_continuous(limits = c(0, 32.5), breaks = seq(0, 32.5, by = 10)) + 
  scale_fill_manual(values = c("#ffbaba", "#EBECF0", "#b2d8d8")) + 
  labs(title = "Distribution of Sentiment among Technologists", 
       x = "Compound Scores", y = "Frequency") + 
  guides(fill = guide_legend(title = "")) +  
  theme_void() +
  theme(text = element_text(family = "Roboto"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.x = element_line(linetype = 1, size = 0.2, 
                                          color = "gray"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 14),
        axis.text = element_text(size = 14, color = "black"),
        legend.margin = margin(0, 0, 5, 0),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")

# Create a new data frame for those younger than 30

under_thirty <- qual_data |>
  filter(age_group == "Younger than 30")

# Create a new data frame for those 30 and older

thirty_plus <- qual_data |> 
  filter(age_group == "30 Plus")

# Run a two-sample t-test

t.test(under_thirty$compound, thirty_plus$compound)

# Define a function to split text into tokens based on spaces

space_tokenizer <- function(text) { 
  str_split(text, " ") 
  }

# Define a function to count the occurrences of unigrams that have digits

token_count <- function(data1) {
  data2 <- data1  |> 
    select(employees) |>
    mutate(employees = str_replace_all(employees, "[.,!?]", ""),
           employees = str_replace_all(employees, " - ", "-")) |>
    unnest_tokens(word, employees, token = space_tokenizer) |>
    filter(str_detect(word, "\\d")) |>
    group_by(word) |>
    summarize(count = n()) |>
    mutate(pct = paste0(round(count / sum(count) * 100), "%")) |>
    arrange(desc(count)) 
  return(data2) 
  }

# Apply the "token_count" function

count2 <- token_count(under_thirty)

# Again, apply the "token_count" function

count3 <- token_count(thirty_plus)

# Perform the data preprocessing steps:
  # These steps include filtering, modifying values, aggregating, etc.

fig1_data <- quant_data |> 
  filter(employment == "Work full-time" & 
         SQ16 != "No Response" & 
         SQ23 != "No Response") |> 
  separate_rows(SQ23, sep = ",") |> 
  mutate(SQ23 = str_trim(SQ23)) |> 
  group_by(SQ16, SQ23) |>
  summarize(count = n()) |>
  mutate(pct = count / sum(count)) |>
  arrange(desc(SQ16), desc(pct)) |>
  top_n(5)
 
# Perform the data preprocessing steps:
  # These steps include filtering, grouping, aggregating, sorting etc.
 
fig2_data <- quant_data |>
  filter(employment == "Work full-time" &
         SQ24 != "No Response" &
         SQ35 != "No Response") |> 
  group_by(SQ24, SQ35) |>
  summarize(count = n()) |>
  mutate(pct = count / sum(count)) |>
  arrange(SQ24, desc(pct))

# Perform the data preprocessing steps:
  # These steps include filtering, aggregating, changing data types, etc.

viz3_data <- quant_data |> 
  filter(employment == "Work full-time" & 
         !SQ16 %in% c("No Response", "Plan To Leave Tech")) |> 
  group_by(SQ16, SQ33) |> 
  summarize(count = n()) |> 
  mutate(pct = count / sum(count), 
         label = paste0(round(count / sum(count) * 100), "%"), 
         label = if_else(
           pct < 0.05, 
           "", 
           label 
           ), 
         SQ16 = factor(SQ16, levels = c("Plan To Leave Role", 
                                        "Plan To Stay In Role"), 
                       labels = c("Plan to Leave Role", 
                                  "Plan to Stay in Role")),
         SQ33 = factor(SQ33, levels = c("Strongly Agree", 
                                        "Agree", 
                                        "Neutral", 
                                        "Disagree",  
                                        "Strongly Disagree")))

# Create a stacked bar chart

ggplot(viz3_data, aes(x = SQ16, y = pct, fill = SQ33)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.7) +
  coord_flip() + 
  scale_y_continuous(labels = percent, limits = c(0, 1)) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 4.75, color = "black") + 
  scale_fill_manual(values = c("#008080", "#b2d8d8","#EBECF0",
                               "#ffbaba", "#ff5252")) +
  labs(title = "Satisfaction with Compensation among Technologists",
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  theme_minimal_vgrid() +
  theme(text = element_text(family = "Roboto"),  
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")

# Perform the data preprocessing steps:
  # These steps include filtering, modifying values, changing data types, etc.

viz4_data <- quant_data |>
  filter(employment == "Work full-time" & 
         SQ25 != "No Response") |> 
  mutate(job_title = if_else( 
    job_title %in% c("Graphic Designer", "IT Project Manager"), 
    "Other",
    job_title 
    )) |>  
  group_by(job_title, SQ25) |> 
  summarize(count = n()) |>
  mutate(pct = count / sum(count), 
         label = paste0(round(count / sum(count) * 100), "%"), 
         label = if_else(
           pct < 0.05, 
           "", 
           label 
           ), 
         job_title = factor(job_title, levels = c("Other", 
                                                  "Data Analyst",
                                                  "Product Manager", 
                                                  "Software Developer")), 
         SQ25 = factor(SQ25, levels = c("Strongly Agree", 
                                        "Agree", 
                                        "Neutral", 
                                        "Disagree",  
                                        "Strongly Disagree")))

# Create a stacked bar chart

ggplot(viz4_data, aes(x = job_title, y = pct, fill = SQ25)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.7) +
  coord_flip() + 
  scale_y_continuous(labels = percent, limits = c(0, 1)) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 4.75, color = "black") + 
  scale_fill_manual(values = c("#008080", "#b2d8d8","#EBECF0",
                               "#ffbaba", "#ff5252")) +
  labs(title = "Satisfaction with Career Advancement among Technologists",
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  theme_minimal_vgrid() +
  theme(text = element_text(family = "Roboto"),  
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")

# Perform the data preprocessing steps:
  # These steps include filtering, aggregating, changing data types, etc.

viz5_data <- quant_data |>
  filter(employment == "Work full-time" &
         race != "No Response" &
         SQ29 != "No Response") |>
  group_by(race, SQ29) |>
  summarize(count = n()) |>
  mutate(pct = count / sum(count), 
         label = paste0(round(count / sum(count) * 100), "%"),
         race = factor(race, levels = c("Other", 
                                        "Black",
                                        "Asian", 
                                        "White", 
                                        "Hispanic")),
         SQ29 = factor(SQ29, levels = c("Strongly Agree", 
                                        "Agree", 
                                        "Neutral", 
                                        "Disagree",  
                                        "Strongly Disagree")))

# Create a stacked bar chart

ggplot(viz5_data, aes(x = race, y = pct, fill = SQ29)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.7) +
  coord_flip() + 
  scale_y_continuous(labels = percent, limits = c(0, 1)) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 4.75, color = "black") + 
  scale_fill_manual(values = c("#008080", "#b2d8d8","#EBECF0",
                               "#ffbaba", "#ff5252")) +
  labs(title = "Satisfaction with Work Culture among Technologists",
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  theme_minimal_vgrid() +
  theme(text = element_text(family = "Roboto"),  
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"),
        axis.line.y = element_blank(),
        axis.text = element_text(size = 14, color = "black"),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.485, "cm"),
        legend.position = "top")
