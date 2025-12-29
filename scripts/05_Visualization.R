library(dplyr)
library(openxlsx)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(scales)

# ------------------------------
# WORKING DIRECTORY + INPUT
# ------------------------------
setwd("C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Analyzed_Files")

in_file  <- "CRLP_Member_Response_Distribution_2023-11-28.csv"
out_file <- "TESTCRLP_Distribution_Report.xlsx"
sheet_nm <- "All_Questions"

bar_color <- "#69B5A8"
text_col  <- "#2F2F2F"

# If TRUE: show % labels on bars
show_bar_labels <- TRUE

# ------------------------------
# TEXT CLEANING (SAFE)
# ------------------------------
clean_utf8 <- function(x) {
  x <- as.character(x)
  
  # Convert to UTF-8 (best effort)
  x <- stringi::stri_enc_toutf8(x)
  
  # Remove/replace invalid bytes safely
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  
  # Remove control characters safely (ASCII-only)
  x <- gsub("[[:cntrl:]]", " ", x)
  
  # Normalize whitespace
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  x
}

safe_wrap <- function(x, width = 22) {
  x <- clean_utf8(x)
  stringr::str_wrap(x, width = width)
}

# ------------------------------
# READ CSV (TRY WINDOWS-1252 FIRST)
# ------------------------------
crlp_data <- readr::read_csv(
  in_file,
  show_col_types = FALSE,
  locale = readr::locale(encoding = "Windows-1252")
)

# CLEAN IMPORTANT TEXT COLUMNS
# ------------------------------
needed_cols <- c("Question", "Response_value", "Count", "Result")
missing_cols <- setdiff(needed_cols, names(crlp_data))
if (length(missing_cols) > 0) {
  stop(paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
}

crlp_data <- crlp_data %>%
  mutate(
    Question       = clean_utf8(Question),
    Response_value = clean_utf8(Response_value)
  )

# ------------------------------
# CLEAN NUMERIC COLUMNS SAFELY
# ------------------------------
crlp_data <- crlp_data %>%
  mutate(
    Result = as.numeric(gsub("%", "", gsub(",", "", as.character(Result)))),
    Count  = suppressWarnings(as.integer(gsub(",", "", as.character(Count))))
  )

# Preserve original question order exactly as in file
question_order <- unique(crlp_data$Question)

# ------------------------------
# CREATE WORKBOOK
# ------------------------------
wb <- createWorkbook()
addWorksheet(wb, sheet_nm)

# Styles
title_style  <- createStyle(fontSize = 14, textDecoration = "bold", fontColour = text_col)
header_style <- createStyle(textDecoration = "bold", fgFill = "#F2F2F2", border = "Bottom")
cell_style   <- createStyle(border = "TopBottomLeftRight")
total_style  <- createStyle(textDecoration = "bold", fgFill = "#E8F4F2", border = "TopBottomLeftRight")
pct_style    <- createStyle(numFmt = "0.00")

# Column widths
setColWidths(wb, sheet_nm, cols = 1:3, widths = c(38, 12, 16))
setColWidths(wb, sheet_nm, cols = 5:15, widths = 12)

current_row <- 1

# ------------------------------
# LOOP THROUGH QUESTIONS
# ------------------------------
for (q in question_order) {
  
  q_clean <- clean_utf8(q)
  
  # Build table
  tbl <- crlp_data %>%
    filter(Question == q) %>%
    transmute(
      `Response Category` = clean_utf8(Response_value),
      Count = Count,
      `Percentage (%)` = Result
    ) %>%
    arrange(desc(`Percentage (%)`))
  
  if (nrow(tbl) == 0) next
  
  total_row <- tibble(
    `Response Category` = "Total",
    Count = sum(tbl$Count, na.rm = TRUE),
    `Percentage (%)` = 100
  )
  
  tbl_out <- bind_rows(tbl, total_row)
  
  # Write question title
  writeData(wb, sheet_nm, q_clean, startCol = 1, startRow = current_row)
  addStyle(wb, sheet_nm, title_style, rows = current_row, cols = 1, gridExpand = TRUE)
  current_row <- current_row + 1
  
  # Write table
  writeData(wb, sheet_nm, tbl_out, startCol = 1, startRow = current_row)
  addStyle(wb, sheet_nm, header_style, rows = current_row, cols = 1:3, gridExpand = TRUE)
  
  body_rows <- (current_row + 1):(current_row + nrow(tbl_out))
  addStyle(wb, sheet_nm, cell_style, rows = body_rows, cols = 1:3, gridExpand = TRUE)
  addStyle(wb, sheet_nm, pct_style,  rows = body_rows, cols = 3, gridExpand = TRUE)
  
  addStyle(
    wb, sheet_nm, total_style,
    rows = current_row + nrow(tbl_out),
    cols = 1:3, gridExpand = TRUE
  )
  
  # Chart data
  plot_df <- tbl %>%
    mutate(`Response Category` = safe_wrap(`Response Category`, width = 22))
  
  # Dynamic chart height
  chart_height <- min(10, max(4, nrow(plot_df) * 0.22))
  
  # Build plot
  p <- ggplot(
    plot_df,
    aes(x = `Percentage (%)`, y = reorder(`Response Category`, `Percentage (%)`))
  ) +
    geom_col(fill = bar_color) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.10)),
      labels = function(x) sprintf("%.0f%%", x)
    ) +
    labs(x = "Percentage (%)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      text       = element_text(colour = text_col),
      axis.text  = element_text(colour = text_col),
      axis.title = element_text(colour = text_col),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 9)
    )
  
  if (isTRUE(show_bar_labels)) {
    p <- p +
      geom_text(
        aes(label = sprintf("%.1f%%", `Percentage (%)`)),
        hjust = -0.05, size = 3, colour = text_col
      )
  }
  
  # Print plot so openxlsx can capture it
  print(p)
  
  # Insert plot to the right of the table
  insertPlot(
    wb, sheet = sheet_nm,
    startRow = current_row,
    startCol = 5,
    width = 7,
    height = chart_height,
    fileType = "png"
  )
  
  # Move down for next question
  row_padding <- 6
  chart_rows  <- ceiling(chart_height * 5)
  current_row <- current_row + nrow(tbl_out) + max(row_padding, chart_rows)
}

# ------------------------------
# SAVE FILE
# ------------------------------
saveWorkbook(wb, out_file, overwrite = TRUE)

out_file
