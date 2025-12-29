#Load Libraries
library(dplyr)
library(ggplot2)
library(skimr)
library(readr)
library(scales)
library(rlang)

#Import the cleaned dataset
crlp_tool1 <- read_csv(
  "C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/data_cleaned/final_tool1_crlp_cleaned_281125.csv"
)
crlp_tool1 <- as_tibble(crlp_tool1)

#Quick Review
head(crlp_tool1)
str(crlp_tool1)
skim(crlp_tool1)

#Columns to analyze
columns_to_analyze <- c(
  "member_province", "member_district", "member_fp_name", "member_cdc_name",
  "member_position", "member_gender", "member_status",
  "do_you_know_this_member", "is_selected_currently_a_member_cdc",
  "was_mem_before", "why_no_longer_a_mem",
  "role_of_member_in_this_cdc_onsite_view",
  "role_of_member_in_this_cdc_onsite_view_other",
  "is_position_filled_by_someone_else",
  "gender_of_new_member", "position_of_new_member",
  "position_of_new_member_other",
  "available_for_interview_for_at_this_site",
  "Do_u_know_why_mem_is_not_present_for_interview_today_at_this_site",
  "why_not_available_for_interview_other",
  "is_possible_to_trach_the_mem_in_another_location",
  "can_u_give_mem_contact_number"
)

#Reusable function for distribution analysis
summarise_responses <- function(df, vars) {
  
  results_list <- list()
  i <- 1
  
  for (col_name in vars) {
    
    # Unique non-missing responses
    responses <- df[[col_name]] |> unique() |> na.omit()
    
    # Count non-missing values (correct denominator)
    total_respondents <- sum(!is.na(df[[col_name]]))
    
    for (response in responses) {
      
      # Count rows matching that response
      response_count <- sum(df[[col_name]] == response, na.rm = TRUE)
      
      # Percentage
      percentage <- (response_count / total_respondents) * 100
      
      # Save row
      results_list[[i]] <- data.frame(
        Disaggregation = "all",
        Disagg_label = "all",
        Disagg_level = "all",
        Question = col_name,
        Response_value = response,
        Aggregation_method = "perc",
        Count = response_count,
        Denominator = total_respondents,
        Result = round(percentage, 2),
        stringsAsFactors = FALSE
      )
      
      i <- i + 1
    }
  }
  
  # Combine all results
  dplyr::bind_rows(results_list)
}

#Run the analysis
crlp_tool1_anal <- summarise_responses(crlp_tool1, columns_to_analyze)

#Export results
write.csv(
  crlp_tool1_anal,
  file("C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Analyzed_Files/CRLP_Member_Response_Distribution_2023-11-28.csv"),
  row.names = FALSE
)
