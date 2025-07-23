#' Clean and Prepare a REDCap Data Dictionary for Mapping
#'
#' This function loads and cleans a raw REDCap data dictionary exported as CSV, removing irrelevant metadata and preparing the structure for variable mapping. It standardizes columns, strips HTML, expands checkbox variables, and creates mapping strings for yesno and radio fields that can be mapped using `ezmap`.
#'
#' @param path The path to a REDCap data dictionary CSV file.
#'
#' @return A cleaned codebook data frame with standardized columns including `variable`, `label`, `type`, and `levels_mapped`.
#'
#' @examples
#' dd <- zap_dd("data_dictionary.csv")
#'
#' @name zap_dd
#' @export
#'
# Load required packages
library(tidyverse)
library(lubridate)

zap_dd <- function(path) {
  # Step 1: Load the raw codebook
  codebook_raw <- read_csv(path)

  # Step 2: Rename columns
  codebook <- codebook_raw %>%
    select(
      variable = `Variable / Field Name`,
      form_name = `Form Name`,
      type = `Field Type`,
      label = `Field Label`,
      levels = `Choices, Calculations, OR Slider Labels`,
      validation = `Text Validation Type OR Show Slider Number`
    )

  # Step 3: Remove descriptive fields
  codebook <- codebook %>%
    filter(type != "descriptive")

  # Step 4: Strip HTML tags from label column
  codebook <- codebook %>%
    mutate(label = str_remove_all(label, "<.*?>"),
           levels = str_remove_all(levels, "<.*?>"),
           levels = str_remove_all(levels, "\\s*\\{.*?\\}"))

  # Step 5: Trim whitespace
  codebook <- codebook %>%
    mutate(across(everything(), ~ str_trim(as.character(.))))

  # Step 6: If type is "calc", remove levels
  codebook <- codebook %>%
    mutate(levels = if_else(type == "calc", NA_character_, levels))

  # Step 7a: Create levels_mapped for yesno and radio only
  codebook <- codebook %>%
    mutate(
      levels_mapped = case_when(
        type == "yesno" ~ '"1" = "Yes", "0" = "No"',
        type == "radio" & !is.na(levels) ~ levels %>%
          gsub('(\\d+),\\s*([^|]+?)\\s*$', '"\\1" = "\\2"', .) %>%  # fix trailing space at end
          gsub('(\\d+),\\s*([^|]+?)\\s*(?=\\|)', '"\\1" = "\\2"', ., perl = TRUE) %>%
          gsub('\\s*\\|\\s*', ', ', .),
        TRUE ~ NA_character_
      )
    )

  # Step 8: Add date_format for variables with date validation
  codebook <- codebook %>%
    mutate(
      date_format = case_when(
        validation == "date_mdy" ~ "mdy",
        validation == "date_dmy" ~ "dmy",
        validation == "date_ymd" ~ "ymd",
        validation == "datetime_mdy" ~ "mdy_hm",
        validation == "datetime_dmy" ~ "dmy_hm",
        validation == "datetime_ymd" ~ "ymd_hm",
        TRUE ~ NA_character_
      )
    )

  # Step 9: Expand checkbox variables into multiple rows
  checkbox_expanded <- codebook %>%
    filter(type == "checkbox") %>%
    filter(!is.na(levels)) %>%
    mutate(level_split = str_split(levels, " \\| ")) %>%
    unnest(level_split) %>%
    mutate(
      checkbox_code = str_extract(level_split, "^\\d+"),
      checkbox_label = str_remove(level_split, "^\\d+,\\s*"),
      variable = paste0(variable, "___", checkbox_code),
      label = checkbox_label,
      type = "checkbox_item",
      levels = NA_character_,
      levels_mapped = NA_character_,
      date_format = NA_character_
    ) %>%
    select(-level_split, -checkbox_code, -checkbox_label)



  # Step 10: Remove original checkbox rows and append expanded rows
  codebook <- codebook %>%
    filter(type != "checkbox") %>%
    bind_rows(checkbox_expanded) %>%
    arrange(form_name)



  return(codebook)
}
