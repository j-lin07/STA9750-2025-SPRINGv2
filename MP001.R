if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}

#use read_csv from readr package
payroll <- read_csv("data/mp01/nyc_payroll_export.csv") |>
#then use a mutate command and the str_to_title function from the stringr package 
  mutate(
    agency_name = str_to_title(agency_name),
    last_name = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
    )

glimpse(payroll)
    
#create a table summarizing eric l. adams' career
library(knitr)
library(kableExtra)

adams_career <- payroll |>
  filter(last_name == "Adams", first_name == "Eric", mid_init == "L") |>
  group_by(fiscal_year) |>
  summarize(
    Total_Salary = sum(base_salary,na.rm = TRUE),
    fiscal_year = first(fiscal_year),
    title_description = first(title_description),
    agency_name = first(agency_name)
  ) |>
rename(
  `Fiscal Year` = fiscal_year,
  `Position` = title_description,
  `Agency` = agency_name,
  `Total Salary`= Total_Salary
) |>
arrange(`Fiscal Year`)

adams_career  

#calculation of total compensation for each employee
payroll <- payroll |>
  mutate(
    total_compensation = case_when(
      last_name == "Adams" & first_name == "Eric" & mid_init == "L" ~ base_salary,
      !is.na(ot_hours) ~ base_salary * regular_hours + base_salary * ot_hours *1.5,
      TRUE ~ base_salary * ((regular_hours + ot_hours)/7.5)
      )
    )

# Calculate an annualized base pay for each record and then group by job title.
job_title_pay <- payroll |>
  group_by(title_description) |>
  summarize(max_annual_base = max(base_salary, na.rm = TRUE)) |>
  arrange(desc(max_annual_base))

job_title_pay[1, ]

# Which individual worked the most overtime hours in this data set?
most_overtime <- payroll |>
  filter(ot_hours == max(ot_hours, na.rm = TRUE)) |>
  select(first_name, last_name, fiscal_year, ot_hours)

most_overtime

# Which agency has the highest average total annual payroll (base and overtime pay per employee)?
agency_avg_pay <- payroll |>
  group_by(agency_name) |>
  summarize(avg_total_pay = mean(total_compensation, na.rm = TRUE)) |>
  arrange(desc(avg_total_pay))

agency_avg_pay[1, ]

# Which agency has the most employees on payroll in each year?
agency_employee_count <- payroll |>
  group_by(fiscal_year, agency_name) |>
  summarize(num_employees = n()) |>
  arrange(fiscal_year, desc(num_employees))

# how many employees are in each agency?
agency_employee_count <- payroll |>
  group_by(fiscal_year, agency_name) |>
  summarize(num_employees = n(), .groups = "drop") |>
  arrange(fiscal_year, desc(num_employees))

agency_employee_count

# For each fiscal_year, keep only the agency with the maximum employee count
top_agency_each_year <- agency_employee_count |>
  group_by(fiscal_year) |>
  slice(1) |>
  ungroup()

top_agency_each_year

#Which agency has the highest overtime usage (compared to regular hours)?
agency_overtime <- payroll |>
  group_by(agency_name) |>
  summarize(
    total_overtime = sum(ot_hours, na.rm = TRUE),
    total_regular = sum(regular_hours, na.rm = TRUE)
  ) |>
  mutate(overtime_ratio = total_overtime / total_regular) |>
  arrange(desc(overtime_ratio))

agency_overtime[1, ]

#What is the average salary of employees who work outside the five boroughs?


#How much has the city’s aggregate payroll grown over the past 10 years?
aggregate_payroll <- payroll |>
  group_by(fiscal_year) |>
  summarize(total_payroll = sum(total_compensation, na.rm = TRUE)) |>
  arrange(fiscal_year)

aggregate_payroll

#Policy I: Capping Salaries at Mayoral Level
#For each fiscal year, identify the mayor’s total compensation.
#For every other employee in that same year who earned more than the mayor, compute how much would be “saved” if their pay were capped at the mayor’s salary.
#Summarize the total savings, and identify which agencies and job titles would be most affected.

current_overtime_cost <- payroll %>%
  summarize(total_overtime_cost = sum(base_salary * ot_hours * 1.5, na.rm = TRUE))

current_overtime_cost

#if the cap is 10 hours per week, then over 52 weeks that’s 520 overtime hours maximum per employee. Then recalculate overtime pay:
payroll_adjusted <- payroll %>%
  mutate(adjusted_overtime_hours = ifelse(ot_hours > 520, 520, ot_hours),
         adjusted_overtime_cost = base_salary * adjusted_overtime_hours * 1.5)

adjusted_overtime_total <- payroll_adjusted %>%
  summarize(total_adjusted_overtime_cost = sum(adjusted_overtime_cost, na.rm = TRUE))

adjusted_overtime_total

