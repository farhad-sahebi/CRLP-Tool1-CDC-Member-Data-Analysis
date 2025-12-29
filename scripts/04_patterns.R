library(tidyverse)
library(writexl)

#Import the file
getwd()
setwd("C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Analyzed_Files")
df <- read_csv("CRLP_Member_Response_Distribution_2023-11-28.csv") 
head(df)
skimr::skim(df)

#Prepare analytically variables 
analysis_df <- df %>%
  group_by(Question) %>%
  mutate(
    total_count_q = sum(Count),
    within_q_pct = Count / total_count_q * 100,
    response_rank = dense_rank(desc(within_q_pct)),
    cumulative_pct = cumsum(within_q_pct)
  )%>%
  ungroup()

#Identify dominant responses (Pattern detection)
dominant_responses <- analysis_df %>%
  filter(response_rank <= 3) %>%
  arrange(Question, response_rank)

dominant_responses

#Detect Pareto / concentration patterns
pareto_flags <- analysis_df %>%
  group_by(Question) %>%
  summarise(
    responses_to_80pct = min(response_rank[cumulative_pct >= 80]),
    .groups = "drop"
  ) %>%
  arrange(responses_to_80pct)

pareto_flags

#Detect multi-select or response inflation (critical insight)
multi_select_analysis <- df %>%
  group_by(Question) %>%
  summarise(
    denominator = first(Denominator),
    total_count = sum(Count),
    response_intensity = total_count / denominator,
    .groups = "drop"
  ) %>%
  arrange(desc(response_intensity))

multi_select_analysis

#Structural imbalance analysis
structural <- analysis_df %>%
  filter(Question %in% c("member_province", "member_gender", "member_position")) %>%
  select(Question, Response_value, Count, within_q_pct) %>%
  arrange(Question, desc(within_q_pct))

structural

#define the list and the sheets names
analysis_outputs <- list(
  "dominant_responses" = dominant_responses,
  "pareto_flags" = pareto_flags,
  "Response_Intensity" = multi_select_analysis,
  "Structural_Distributions" = structural
)


#Export the file
write_xlsx(
  analysis_outputs,
  "CRLP_Analysis_Patterns_141225.xlsx"
)

