# 1. Load required libraries
library(readxl)
library(dplyr)
library(skimr)
library(janitor)
library(naniar)
library(stringr)
library(ggplot2)
library(scales) 

# 2. Import the dataset
tool1_crlp <- read_excel(
  "C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Raw_Dataset/Tool1_CDC_Members List_12324.xlsx"
)
# Quick look
head(tool1_crlp)

# 3. Basic cleaning (names, empty rows, duplicates)
# Fix column names
tool1_crlp <- tool1_crlp %>%
  janitor::clean_names()

# Remove completely empty rows
tool1_crlp <- tool1_crlp %>%
  janitor::remove_empty("rows")

# Remove fully duplicate rows
tool1_crlp <- tool1_crlp %>%
  dplyr::distinct()

# Check dimensions
nrow(tool1_crlp)
ncol(tool1_crlp)

# 4. Standardize key text fields
tool1_crlp <- tool1_crlp %>%
  mutate(
    site_visit_id      = str_squish(site_visit_id),
    member_province    = str_squish(str_to_title(member_province)),
    member_district    = str_squish(str_to_title(member_district)),
    member_cdc_name    = str_squish(str_to_title(member_cdc_name)),
    member_name        = str_squish(str_to_title(member_name))
  )
# 5. Standardize Gender Values
tool1_crlp %>%
  count(member_gender)

tool1_crlp <- tool1_crlp %>%
  mutate(
    member_gender = str_squish(str_to_lower(member_gender)),
    member_gender = case_when(
      member_gender %in% c("m", "male")   ~ "Male",
      member_gender %in% c("f", "female") ~ "Female",
      member_gender %in% c("other")       ~ "Other",
      TRUE                                ~ "Unknown"
    )
  )

tool1_crlp %>%
  select(member_province, member_district, member_cdc_name, member_name, member_gender) %>%
  head(10)

# 6. Clean Yes/No columns
yes_no_cols <- c(
  "do_you_know_this_member",
  "is_selected_currently_a_member_cdc",
  "was_mem_before",
  "is_position_filled_by_someone_else",
  "available_for_interview_for_at_this_site",
  "do_u_know_why_mem_is_not_present_for_interview_today_at_this_site",
  "is_possible_to_trach_the_mem_in_another_location",
  "can_u_give_mem_contact_number"
)

# Keep only columns that actually exist in the data (for safety)
yes_no_cols <- intersect(yes_no_cols, names(tool1_crlp))

tool1_crlp <- tool1_crlp %>%
  mutate(
    across(
      all_of(yes_no_cols),
      ~ {
        x_raw <- .x
        x <- str_to_lower(str_squish(as.character(.x)))
        case_when(
          x %in% c("yes", "y", "1", "true")   ~ "Yes",
          x %in% c("no", "n", "0", "false")   ~ "No",
          is.na(x_raw)                        ~ NA_character_,
          TRUE                                ~ "Unknown"
        )
      }
    )
  )

# Check distribution
lapply(tool1_crlp[yes_no_cols], function(x) table(x, useNA = "ifany"))


# 7. Clean and Normalize Member Status (Active/Inactive)
tool1_crlp %>%
  count(member_status)

tool1_crlp <- tool1_crlp %>%
  mutate(
    member_status = str_squish(str_to_title(member_status)),
    member_status = case_when(
      member_status %in% c("Existing", "E", "Old", "1", "Active", "A") ~ "Active",
      member_status %in% c("New", "N", "O", "Inactive", "I")           ~ "Inactive",
      TRUE                                                             ~ "Unknown"
    )
  )

# 8. Clean role_of_member_in_this_cdc_onsite_view
tool1_crlp %>%
  count(role_of_member_in_this_cdc_onsite_view)

tool1_crlp <- tool1_crlp %>%
  mutate(
    role_of_member_in_this_cdc_onsite_view =
      str_squish(str_to_title(role_of_member_in_this_cdc_onsite_view))
  )
# 9. Clean “new member” fields
tool1_crlp <- tool1_crlp %>%
  mutate(
    name_of_new_member      = str_squish(str_to_title(name_of_new_member)),
    gender_of_new_member    = str_squish(str_to_title(gender_of_new_member)),
    position_of_new_member  = str_squish(str_to_title(position_of_new_member))
  )

tool1_crlp %>%
  count(gender_of_new_member)

tool1_crlp <- tool1_crlp %>%
  mutate(
    gender_of_new_member = case_when(
      gender_of_new_member %in% c("Male", "M", "m")        ~ "Male",
      gender_of_new_member %in% c("Female", "F", "f")     ~ "Female",
      TRUE                                                ~ "Unknown"
    )
  )

# 10. Clean updated fields
tool1_crlp <- tool1_crlp %>%
  mutate(
    updated_name   = str_squish(str_to_title(updated_name)),
    updated_gender = str_squish(str_to_title(updated_gender)),
    updated_gender = case_when(
      updated_gender %in% c("Male", "M")   ~ "Male",
      updated_gender %in% c("Female", "F") ~ "Female",
      TRUE                                 ~ "Unknown"
    )
  )

# 11. Phone Number Cleaning + Validation
tool1_crlp <- tool1_crlp %>%
  mutate(
    mem_phone_number_clean = mem_phone_number %>%
      str_squish() %>%
      str_replace_all("[^0-9]", "") %>%
      str_trim()
  )

tool1_crlp <- tool1_crlp %>%
  mutate(
    phone_length   = nchar(mem_phone_number_clean),
    phone_validity = case_when(
      phone_length == 10                      ~ "Valid",
      phone_length >= 11 & phone_length <=12 ~ "International Format",
      phone_length == 0 | is.na(phone_length)~ "Missing",
      TRUE                                   ~ "Invalid"
    )
  )

# Standardize numbers that start with 7 (Afghan format 07xxxxxxxx)
tool1_crlp <- tool1_crlp %>%
  mutate(
    mem_phone_number_clean = case_when(
      nchar(mem_phone_number_clean) == 9 ~ paste0("0", mem_phone_number_clean),
      TRUE                               ~ mem_phone_number_clean
    )
  )

# 12. Basic CDC Code Cleaning + Validation
tool1_crlp <- tool1_crlp %>%
  mutate(
    cdc_code_clean = member_cdc_code %>%
      str_squish() %>%
      str_to_upper()
  )

tool1_crlp %>%
  count(cdc_code_clean) %>%
  arrange(desc(n)) %>%
  head(20)

tool1_crlp <- tool1_crlp %>%
  mutate(
    cdc_code_length  = nchar(cdc_code_clean),
    cdc_code_numeric = str_detect(cdc_code_clean, "^[0-9]+$"),
    cdc_code_status  = case_when(
      is.na(cdc_code_clean) | cdc_code_clean == "" ~ "Missing",
      cdc_code_length < 7                          ~ "Too short",
      cdc_code_length > 7                          ~ "Too long",
      !cdc_code_numeric                            ~ "Non-Numeric / Mixed",
      TRUE                                         ~ "Valid format"
    )
  )

tool1_crlp %>%
  count(cdc_code_status)

# Duplicate CDC codes
cdc_dup_summary <- tool1_crlp %>%
  count(cdc_code_clean, member_province, member_district) %>%
  arrange(desc(n))

head(cdc_dup_summary, 20)

# CDC codes in multiple locations
cdc_multi_loc <- tool1_crlp %>%
  group_by(cdc_code_clean) %>%
  summarize(
    n_members   = n(),
    n_provinces = n_distinct(member_province),
    n_districts = n_distinct(member_district),
    .groups     = "drop"
  ) %>%
  filter(n_provinces > 1 | n_districts > 1)

cdc_multi_loc

# Quick view
tool1_crlp %>%
  select(member_province, member_district, member_cdc_name, cdc_code_clean, cdc_code_status) %>%
  head(15)

# 13. Consistency Rules
# Rule 1: If NOT currently a member, force status to Inactive
tool1_crlp <- tool1_crlp %>%
  mutate(
    member_status = case_when(
      is_selected_currently_a_member_cdc == "No" & member_status == "Active" ~ "Inactive",
      TRUE                                                                   ~ member_status
    )
  )

# Rule 2: If was NOT a member before, reason "why_no_longer_a_mem" must be NA
tool1_crlp <- tool1_crlp %>%
  mutate(
    why_no_longer_a_mem = case_when(
      was_mem_before == "No" ~ NA_character_,
      TRUE                   ~ why_no_longer_a_mem
    )
  )

# Rule 3: If member WAS interviewed, "why not available" should be NA
tool1_crlp <- tool1_crlp %>%
  mutate(
    why_not_available_for_interview_other = case_when(
      available_for_interview_for_at_this_site == "Yes" ~ NA_character_,
      TRUE                                              ~ why_not_available_for_interview_other
    )
  )

# Rule 4: If position is NOT filled by someone else, new member fields must be NA
tool1_crlp <- tool1_crlp %>%
  mutate(
    name_of_new_member     = if_else(is_position_filled_by_someone_else == "No",
                                     NA_character_, name_of_new_member),
    gender_of_new_member   = if_else(is_position_filled_by_someone_else == "No",
                                     NA_character_, gender_of_new_member),
    position_of_new_member = if_else(is_position_filled_by_someone_else == "No",
                                     NA_character_, position_of_new_member)
  )

# Rule 5: Updated name should only exist if different from original
tool1_crlp <- tool1_crlp %>%
  mutate(
    updated_name = case_when(
      !is.na(updated_name) & updated_name == member_name ~ NA_character_,
      TRUE                                              ~ updated_name
    )
  )

# Rule 6: Updated gender should only exist if different from original
tool1_crlp <- tool1_crlp %>%
  mutate(
    updated_gender = case_when(
      !is.na(updated_gender) & updated_gender == member_gender ~ NA_character_,
      TRUE                                                    ~ updated_gender
    )
  )

# 14. Clean all character columns (trim + placeholders → NA)
char_cols <- names(tool1_crlp)[sapply(tool1_crlp, is.character)]

tool1_crlp <- tool1_crlp %>%
  mutate(
    across(
      all_of(char_cols),
      ~ {
        x <- str_squish(.x)
        x[x %in% c("", "-", "_", "N/A", "n/a", "na", "Na", "NA")] <- NA_character_
        x
      }
    )
  )

# 15. Convert key variables to factors
tool1_crlp <- tool1_crlp %>%
  mutate(
    member_gender = factor(member_gender,
                           levels = c("Male", "Female", "Other", "Unknown")),
    member_status = factor(member_status,
                           levels = c("Active", "Inactive", "Unknown")),
    phone_validity = factor(phone_validity,
                            levels = c("Valid", "International Format", "Missing", "Invalid"))
  )

# 16. Final QA check
skimr::skim(tool1_crlp)
colSums(is.na(tool1_crlp))

# 17. PROFESSIONAL VISUALIZATIONS (for report)
# 17.1 Members by Gender
# Prepare data
df_gender <- tool1_crlp %>%
  filter(!is.na(member_gender), member_gender != "Unknown") %>%
  count(member_gender) %>%
  mutate(
    pct   = n / sum(n),
    label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")
  )

# Futuristic / neon style plot
plot_gender <- ggplot(
  df_gender,
  aes(x = reorder(member_gender, n),
      y = n,
      fill = n)
) +
  # Bars
  geom_col(width = 0.55, color = NA) +
  
  # Glow-style labels on bars
  geom_text(
    aes(label = label),
    hjust = -0.15,
    size  = 4.2,
    fontface = "bold",
    color = "#f9fafb"
  ) +
  
  # Horizontal layout with extra space for labels
  coord_flip(ylim = c(0, max(df_gender$n) * 1.25)) +
  
  # Neon gradient based on counts
  scale_fill_gradientn(
    colours = c("#22d3ee", "#a855f7", "#f97316")
  ) +
  
  # Axis formatting
  scale_y_continuous(labels = comma) +
  
  # Titles
  labs(
    title    = "CDC Members by Gender",
    subtitle = "Counts and percentages of members by reported gender",
    x        = "Gender",
    y        = "Number of Members",
    caption  = "Source: CRLP Tool 1 – CDC Member List (cleaned)"
  ) +
  
  # Dark, minimal, “dashboard” theme
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#020617", color = NA),  # near-black
    panel.background   = element_rect(fill = "#020617", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#1f2937", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    
    plot.title         = element_text(
      face  = "bold",
      size  = 18,
      color = "#e5e7eb"
    ),
    plot.subtitle      = element_text(
      size  = 11,
      color = "#9ca3af"
    ),
    
    axis.text.x        = element_text(color = "#e5e7eb"),
    axis.text.y        = element_text(color = "#e5e7eb"),
    axis.title.x       = element_text(color = "#9ca3af", margin = margin(t = 8)),
    axis.title.y       = element_text(color = "#9ca3af", margin = margin(r = 8)),
    
    plot.caption       = element_text(size = 9, color = "#6b7280", hjust = 0),
    legend.position    = "none"
  )

plot_gender

# 17.4 Top 10 Provinces by number of members
# Prepare top 10 provinces
top_provinces <- tool1_crlp %>%
  filter(!is.na(member_province)) %>%
  count(member_province, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(
    pct = n / sum(n),
    label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")
  )

# Futuristic visualization
plot_province <- ggplot(
  top_provinces,
  aes(x = reorder(member_province, n),
      y = n,
      fill = n)
) +
  # Bars
  geom_col(width = 0.55, color = NA) +
  
  # Neon glowing labels
  geom_text(
    aes(label = label),
    hjust = -0.10,
    size = 4.2,
    fontface = "bold",
    color = "#f1f5f9"
  ) +
  
  # Horizontal layout with extra space for labels
  coord_flip(ylim = c(0, max(top_provinces$n) * 1.25)) +
  
  # Neon gradient color palette
  scale_fill_gradientn(
    colours = c("#22d3ee", "#3b82f6", "#a855f7", "#f97316")
  ) +
  
  # Clean number formatting
  scale_y_continuous(labels = comma) +
  
  # Titles & labels
  labs(
    title    = "Top 10 Provinces by Number of CDC Members",
    subtitle = "Counts and proportions of CDC members across leading provinces",
    x        = "Province",
    y        = "Number of Members",
    caption  = "Source: CRLP Tool 1 – Cleaned Dataset (2025)"
  ) +
  
  # 2025 dark professional theme
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#020617", color = NA),
    panel.background   = element_rect(fill = "#020617", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#1e293b", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    
    plot.title         = element_text(face = "bold", size = 17, color = "#f1f5f9"),
    plot.subtitle      = element_text(size = 11, color = "#94a3b8"),
    
    axis.text.x        = element_text(color = "#e2e8f0"),
    axis.text.y        = element_text(color = "#e2e8f0"),
    axis.title.x       = element_text(color = "#94a3b8", margin = margin(t = 8)),
    axis.title.y       = element_text(color = "#94a3b8", margin = margin(r = 8)),
    
    plot.caption       = element_text(size = 9, color = "#64748b", hjust = 0),
    legend.position    = "none"
  )

plot_province

# 19. Export the cleaned dataset

setwd("C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Cleaned Dataset")

write.csv(
  tool1_crlp,
  "final_tool1_crlp_cleaned_281125.csv",
  row.names = FALSE
)
