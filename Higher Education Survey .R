#///////////////////////////////////.  Part II  /////////////////////////////
#read file
dataset = load(file= "/Users/default/Documents/Data Management/PrblemSet/Take_Bakhtiari_C/takehome_C.rda")
View(takehome_edu)
summary(takehome_edu)
str(takehome_edu)

#duplicates obs
#Every person should only be surveyed once per year. there are 94,996 observations duplicated which have to be solved by distinct comman.
library(dplyr)
library(validate)
is_unique(takehome_edu$personid, takehome_edu$year)

# using distinct function to keep id according to year
data_unique <- takehome_edu |>
  distinct(personid, year, .keep_all = TRUE)
is_unique(data_unique$personid, data_unique$year)

#Detecting inconsistencies/////////        in tailing spaces we can use trim function data_unique$gender <- trimws(data_unique$gender)
#Gender : There are some typo errors in gender variable including Female = 260,820 ,Female = 65,415  ,Male = 460,581  ,Male = 115,458 , which caused inconsistency in the variable. by recoding the gender variable the problem has been solved.
table(data_unique$gender)     # Female Female     Male   Male 
data_unique <- data_unique %>%
  mutate(gender = recode(gender, "Male " = "Male", "Female " = "Female"))

#Age : The age variable consists of values between 23 and 99. Because the distance between the last age and the one before reveals a huge gap (24 years) and this category includes only 6 values, it could be considered as a potential missing values in between.
table(data_unique$age)
hist(data_unique$age, main = "Age Distribution" , xlab = "Age", breaks = 60)
data_unique %>% filter(age == 99 ) %>% select(salary , ocedrlp)

#Degree : There are some typo errors in dgrdg(highest degree) variable including Bachelor's = 280112 ,Bachelor's = 70676  ,Doctorate = 237134  ,Doctorate = 58954 , Master's = which caused inconsistency in the variable. by recoding the gender variable the problem has been solved.
table(data_unique$dgrdg)
data_unique <- data_unique |> 
  mutate(dgrdg = recode(dgrdg, "Bachelor's " = "Bachelor's" , "Doctorate" = "Doctorate " ,
                        "Master's " = "Master's" , "Professional " = "Professional" ))

#US citizens
table(data_unique$ctzusin)
data_unique <- data_unique %>% 
  mutate(ctzusin = recode(ctzusin, "no" = "No" , "NO" = "No" , "yes" = "Yes" , "YES" = "Yes" ))

#Employment status
table(data_unique$lfstat)
data_unique <- data_unique %>%
  mutate(lfstat = recode(lfstat, "Not in the labor force " = "Not in the labor force" ,
                         "Employed " = "Employed" , "Unemployed " = "Unemployed" ))

#changing categorical formats to Factor
str(data_unique)
data_unique$biryr <-as.factor(data_unique$biryr)     #mixed formats
class(data_unique$biryr)
data_unique$gender <-as.factor(data_unique$gender)      #categorical variable
class(data_unique$gender)
data_unique$raceth <-as.factor(data_unique$raceth)       #categorical variable
class(data_unique$raceth)
data_unique$chtot <-as.factor(data_unique$chtot)       #categorical variable
class(data_unique$chtot)
data_unique$dgrdg <-as.factor(data_unique$dgrdg)       #categorical variable
class(data_unique$dgrdg)

# Table for missing values. we could also use n_distinct
library(dplyr)
library(tidyr)
# Calculate missing values count and percentage per variable
missing_summary <- data_unique %>%
  summarise(across(everything(), 
                   list(
                     missing_count = ~ sum(is.na(.)),
                     missing_pct = ~ mean(is.na(.)) * 100
                   ),
                   .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(                         # using data, col
    cols = everything(),
    names_to = c("variable", "metric"),
    names_sep = "__"  # Splits column names into 'variable' and 'metric'
  ) %>%
  pivot_wider(
    names_from = "metric",
    values_from = "value"
  ) %>%
  arrange(desc(missing_count)) %>%
  filter(missing_count > 0) %>% # Optional: Show only variables with missing data
  head(10)


# Save to .tex file
print(
  xtable(missing_summary, 
         caption = "Top 10 Missisng summary data", 
         label = "tab:missing_summary"),
  file = "Top 10 Missisng summary.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting 
)
#///////////////////////////////////.  Part III  /////////////////////////////
# III.1
# cleaning data                 failed
# Load required packages
library(dplyr)

# Clean the data 
clean_data <- data_unique %>%
  mutate(across(everything(), ~ trimws(.))) %>%  # Base R whitespace trimming
  mutate(across(where(is.character), ~ tolower(.))) %>% # Base R lowercase conversion
  mutate(across(everything() , ~ ifelse(. == "logical skip", NA, .)))    #ifelse(condition, value_if_TRUE, value_if_FALSE)

# testing cleaning data 
table(data_unique$jobsatis)
table(clean_data$jobsatis)

# dgrdg =Type of highest certificate or degree, ndgmed = Field of major for highest degree
# lfstat = Labor force status, ocedrlp = Principal job related to highest degree,
# jobsatis = Job satisfaction, satadv = Satisfaction principal job's opportunities for advancement,
# creating job satisfaction dataset by degree
Job_satisfaction <- clean_data %>%
   select(personid, year, age, lfstat, dgrdg, jobsatis, satadv, satben, satsal, ndgmed, ocedrlp) %>%
   filter(!is.na(dgrdg))

# cleaning job satisfaction dataset
Job_satisfaction_clean <- Job_satisfaction %>%
           # Filter to keep rows with AT LEAST ONE non-NA satisfaction measure
           # if_any() is useful for row-wise "OR" conditions.
           # if_all() (its counterpart) checks if all selected columns meet a condition (like SQL AND).
           filter(if_any(c(jobsatis, satadv, satben, satsal), ~ !is.na(.)))


# Analyzing the satisfaction by educational status
satisfaction_analysis <- Job_satisfaction_clean %>%
  group_by(dgrdg) %>%
  summarise(
    # Overall job satisfaction
    overall_avg_job_satisfaction = mean(
      case_when(              #case_when() evaluates conditions sequentially and assigns a value based on the first matching condition.
        jobsatis == "very satisfied" ~ 4,
        jobsatis == "somewhat satisfied" ~ 3,
        jobsatis == "somewhat dissatisfied" ~ 2,
        jobsatis == "very dissatisfied" ~ 1,
        TRUE ~ NA_real_      #If jobsatis does not match any of the specified categories, it assigns NA_real_ (a missing numeric value).
      ), 
      na.rm = TRUE           # not affected by misValu
    ),
    
    # Salary satisfaction (% satisfied)  if %in% an element exists within a vector, list, or other data structure
    salary_pct_satisfied = mean(
      satsal %in% c("very satisfied", "somewhat satisfied" , "somewhat dissatisfied", "very dissatisfied"), 
      na.rm = TRUE
    ) * 100,
    
    # Advancement opportunities (% satisfied)
    advance_pct_satisfied = mean(
      satadv %in% c("very satisfied", "somewhat satisfied", "somewhat dissatisfied", "very dissatisfied"), 
      na.rm = TRUE
    ) * 100,
    
    n = n()
  ) %>%
  arrange(desc(overall_avg_job_satisfaction))  # Sort by highest satisfaction

# Change heading of the table of output and prepare table for LateX
library(validate)
library(xtable)
satisfaction_analysis %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  setNames(c(
    "Degree", 
    "Avg Job Satisfaction (1-4)", 
    "% Satisfied with Salary", 
    "% Satisfied with Advancement",
    "N"
  )) %>%
  xtable(
    caption = "Job Satisfaction Summary by Degree",
    label = "tab:satisfaction",
    align = "lrrrrr"  # Left-align first column, right-align others
  ) %>%
  print(
    include.rownames = FALSE,
    booktabs = TRUE,
    sanitize.text.function = identity  # Preserves special chars
  )

# Save to .tex file
print(
  xtable(satisfaction_analysis, 
         caption = "Job Satisfaction Summary by Degree", 
         label = "tab:missing_summary"),
  file = "Job Satisfaction Summary by Degree.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting 
)




library(xtable)

# III.2  examines how closely respondents' jobs align with their field of study aand relationship between alignment and job satisfaction.
satisfaction_jobRelate_by_field <- Job_satisfaction_clean %>%
  filter(!is.na(ocedrlp))%>%
  group_by(ocedrlp) %>%
  summarise(
    # Overall job satisfaction
    overall_avg_jobRelate = mean(
      case_when(              #case_when() evaluates conditions sequentially and assigns a value based on the first matching condition.
        jobsatis == "very satisfied" ~ 4,
        jobsatis == "somewhat satisfied" ~ 3,
        jobsatis == "somewhat dissatisfied" ~ 2,
        jobsatis == "very dissatisfied" ~ 1,
        TRUE ~ NA_real_      #If jobsatis does not match any of the specified categories ("very satisfied", "somewhat satisfied", etc.), it assigns NA_real_ (a missing numeric value).
      ), 
      na.rm = TRUE
    ),
    
    # Salary satisfaction (% satisfied)
    salary_pct_satisfied = mean(
      satsal %in% c("very satisfied", "somewhat satisfied" , "somewhat dissatisfied", "very dissatisfied"), 
      na.rm = TRUE
    ) * 100,
    
    # Advancement opportunities (% satisfied)
    advance_pct_satisfied = mean(
      satadv %in% c("very satisfied", "somewhat satisfied", "somewhat dissatisfied", "very dissatisfied"), 
      na.rm = TRUE
    ) * 100,
    
    n = n()
  ) %>%
  arrange(desc(overall_avg_jobRelate))  # Sort by highest satisfaction


# Generate LaTeX table code with vertical lines and formatting
latex_code_satis_Jobrelated_by_field <- satisfaction_jobRelate_by_field %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  setNames(c(
    "Job related degree", 
    "Avg Job Satisfaction (1-4)", 
    "% Satisfied Salary", 
    "% Satisfied with Advancement",
    "N"
  )) %>%
  xtable(
    caption = "Job Satisfaction Summary related to degree",
    label = "tab:satisfaction",
    align = "|l|r|r|r|r|r|"  # Vertical lines added with | between columns
  ) %>%
  print(
    include.rownames = FALSE,
    booktabs = FALSE,      # Must be FALSE for vertical lines
    hline.after = c(-1, 0, nrow(satisfaction_jobRelate_by_field)), # Lines above/below header and at bottom
    sanitize.text.function = function(x) gsub("%", "\\%", x, fixed = TRUE), # Escape % properly
    comment = FALSE
  )

# Save to .tex file
print(
  xtable(satisfaction_jobRelate_by_field, 
         caption = "Job Satisfaction to Degree by field of study", 
         label = "tab:satisfaction_by_fiels"),
  file = "Job Satisfaction to Degree by field of study.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting 
)


# Fields by Percentage Closely Related to Degree
# ndgmed = Field of major for highest degree , ocedrlp = Principal job related to highest degree,
alignment_summary <- Job_satisfaction_clean %>%
  group_by( ndgmed, ocedrlp) %>%      # Group by field of study and alignment category
  summarise(count = n(), .groups = "drop") %>%  # Count cases in each group
  group_by(ndgmed) %>%               # Regroup by field to calculate percentages
  mutate(
    total = sum(count),              # Total respondents per field
    pct = round(count/total * 100, 1)) # Calculate percentage 


# Aggregating data and make wider
alignment_summary_agg <- alignment_summary %>%
  subset(select = -c(count , total)) %>%
  filter(!is.na(ocedrlp)) %>%
  group_by(ndgmed, pct) %>%
  pivot_wider(names_from = ocedrlp, values_from = pct)

# Select top 8 values
top_8_pct <- alignment_summary_agg%>%
  arrange(desc(`closely related`)) %>%
  head(8)

# Rename columns and display
colnames(top_8_pct) <- c("Field of Study", "Closely Related", "Not Related", "Somewhat Related")

#  Generate LaTeX table with xtable
latex_table_field_related_job <- xtable(
  top_8_pct,
  caption = "Top 8 Fields by Percentage Closely Related to Degree",
  label = "tab:top8alignment",
  align = "|l|r|r|r|r|"  # Left-align first column, right-align others
)

# Save to .tex file
print(
  xtable(top_8_pct, 
         caption = "Top 8 Fields by Percentage Relation to Degree", 
         label = "tab:Tob_relation_by_fiels"),
  file = "Top 8 Fields by Percentage Relation to Degree.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting 
)






# III.3
# work activities vary by educational attainment and field of study. 
# dgrdg =Type of highest certificate or degree, ndgmed = Field of major for highest degree
# lfstat = Labor force status, ocedrlp = Principal job related to highest degree,
# jobsatis = Job satisfaction, satadv = Satisfaction principal job's opportunities for advancement,
# satben = Satisfaction principal job benefits, satsal = Satisfaction principal job salary,
# actcap = Activity, computer applications, actmgt = Activity, management/sales,
# actrd = Activity, research and development, acttch = Activity, teaching
work_by_degree <- clean_data %>%
  select(personid, year, age, dgrdg, ndgmed, actcap, actmgt, actrd, acttch ) %>%
  filter(!is.na(dgrdg))%>%
  group_by(dgrdg) %>%
  summarise(
    # Average of activities
        RD = mean(actrd == "yes", na.rm = TRUE) *100,
        Computer_Application = mean(actcap == "yes", na.rm = TRUE) *100,
        Management = mean(actmgt == "yes", na.rm = TRUE) *100 ,
        Teaching = mean(acttch == "yes", na.rm = TRUE) *100,
        n = n()
    )%>%
  mutate(across(where(is.numeric), ~ round(., 1)))%>%
  arrange(desc(RD)) %>%
  rename(
    "Type degree" = dgrdg, 
    "RD(%)" = RD,
    "Computer Application (%)" = Computer_Application,
    "Management (%)" = Management,
    "Teaching (%)" = Teaching
  )

# Generate LaTeX table
latex_table_work_educ <- work_by_degree %>%
  xtable(
    caption = "Education Attainment by Working Activity",
    label = "tab:work_by_field",
    align = c("l", "r", "r", "r", "r", "r", "r"),  # Alignment (left for field name, right for others)
    digits = c(0, 1, 1, 1, 1, 1, 0)  # Digits (0 for field name and n, 1 for percentages)
  )
# Print LaTeX output with formatting
print(latex_table_work_educ,
      include.rownames = FALSE,
      booktabs = TRUE,            # Professional quality horizontal lines
      sanitize.text.function = function(x) x,  # Preserve special characters
      comment = FALSE,
      hline.after = c(-1, 0: nrow(work_by_degree)))  # Lines above/below header and at bottom
# To save directly to a .tex file:
print(latex_table_work_educ, file = "Education Attainment by Working Activity.tex")



# Work analysis by field of study
work_by_field <- clean_data %>%
  select(personid, year, age, dgrdg, ndgmed, actcap, actmgt, actrd, acttch ) %>%
  filter(!is.na(ndgmed))%>%
  group_by(ndgmed) %>%
  summarise(
    RD = mean(actrd == "yes", na.rm = TRUE) *100,
    Computer_Application = mean(actcap == "yes", na.rm = TRUE) *100,
    Management = mean(actmgt == "yes", na.rm = TRUE) *100 ,
    Teaching = mean(acttch == "yes", na.rm = TRUE) *100,
    n = n()
  )%>%
  mutate(across(where(is.numeric), ~ round(., 1)))%>%
  arrange(desc(RD)) %>%
  rename(
    "Field of major for highest degree"= ndgmed, 
    "RD(%)" = RD,
    "Computer Application (%)" = Computer_Application,
    "Management (%)" = Management,
    "Teaching (%)" = Teaching
  )
top_10_field <- work_by_field %>% 
  head(10)


# Generate LaTeX table
latex_table_work_field <- top_10_field %>%
  xtable(
    caption = "Top 10 Fields by Working Activity",
    label = "tab:work_by_field",
    align = c("l", "r", "r", "r", "r", "r", "r"),  # Alignment (left for field name, right for others)
    digits = c(0, 1, 1, 1, 1, 1, 0)  # Digits (0 for field name and n, 1 for percentages)
  )
# Print LaTeX output with formatting
print(latex_table_work_field,
      include.rownames = FALSE,
      booktabs = TRUE,            # Professional quality horizontal lines
      sanitize.text.function = function(x) x,  # Preserve special characters
      comment = FALSE,
      hline.after = c(-1, 0: nrow(top_10_field)))  # Lines above/below header and at bottom
# To save directly to a .tex file:
print(latex_table_work_field, file = " Top 10 Fields by Working Activity.tex")



# IV        Regression Analysis
# IV.1
# degree type, field of study major groups, job-education relationship, training participation, and demographic factors (e.g., age, gender, ethnicity)
# dgrdg =Type of highest certificate or degree,       ndgmed = Field of major for highest degree,
# wktrni = Attended work-related training,             jobsatis = Job satisfaction, raceth = Race/ethnicity
# lfstat = Labor force status,                         ocedrlp = Principal job related to highest degree,
# satadv = Satisfaction principal job's opportunities for advancement,
# satben = Satisfaction principal job benefits,       satsal = Satisfaction principal job salary,
# actcap = Activity, computer applications,          actmgt = Activity, management/sales,
# actrd = Activity, research and development,        acttch = Activity, teaching


Job_satisfaction_data <- clean_data %>%
  select(personid, year, age, gender, raceth, dgrdg, salary, ndgmed,  ocedrlp, wktrni, jobsatis, satsal, satben) %>%
  filter(!is.na(personid)) %>%
  mutate(
    year = factor(year) , age = as.numeric(age) , gender = as.numeric(gender == "male"),salary = as.numeric(salary),
    raceth = factor(raceth), dgrdg = factor(dgrdg) , ndgmed = factor(ndgmed), satsal = factor(satsal),
    ocedrlp = factor(ocedrlp) , wktrni = as.numeric(wktrni == "yes") , jobsatis = as.numeric(as.factor(jobsatis))
)


job_sat_model <- lm(jobsatis ~  age+ gender+ raceth+ dgrdg+ ndgmed+ salary + ocedrlp+ wktrni, data = Job_satisfaction_data)

# Display results using `xtable` for publication-ready output
# Generate LaTeX table
latex_table_job_sat_model <- xtable(job_sat_model, 
                      caption = "Linear regression model predicting overall job satisfaction",
                      label = "tab:job_sat_results",
                      digits = 3)

# Print the LaTeX code
print(latex_table_job_sat_model, 
      include.rownames = TRUE,
      caption.placement = "top",
      table.placement = "htbp",
      sanitize.text.function = function(x) x)

# To save directly to a .tex file:
print(latex_table_job_sat_model, file = " Linear regression model predicting overall job satisfaction.tex")




# IV.2               models predicting salary satisfaction
#  models predicting salary satisfaction for different degree types. Include field of study, job benefits, and demographic factors
sal_satis_clean <- Job_satisfaction_data %>%
  filter(!is.na(satsal))%>%
  mutate(satsal_num = as.numeric(as.factor(satsal)))
  
sal_sat_model <- lm(satsal_num ~  age + gender+ raceth + dgrdg + ndgmed +  satben, data = sal_satis_clean)

# Generate LaTeX table code with vertical lines and formatting
# Save to .tex file
print(
  xtable(sal_sat_model, 
         caption = "Salary Satisfaction Summary model related to degree", 
         label = "tab:Salary satisfaction model"),
  file = "Salary Satisfaction Summary model.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting (recommended)
)

# IV.3.   career advancement satisfaction
# factors associated with career advancement satisfaction by estimating a model using satisfaction with advancement opportunitie
# Include educational background, job-education match, work activities, training participation, employer characteristics, and demographic factors

# ocedrlp = Principal job related to highest degree,           wktrni = Attended work-related training,  
# satadv = Satisfaction principal job's opportunities for advancement, 
# emsec = Employer sector ,                                    emsize = Size of employer, wapri = Most hours, work activity,
# waprsm = Summarized primary work activity,                    wasec = Second most hours, work activity,
# wascsm = Summarized secondary work activity,                 dgrdg =Type of highest certificate or degree,

#  career advancement satisfaction model
career_adv_sat_clean <- clean_data %>%
  select(personid, year, age, gender, raceth, dgrdg, ocedrlp, wktrni, satadv, waprsm, wascsm, emsize, wasec ) %>%
  filter(!is.na(satadv)) %>%
  mutate(
    satadv = case_when(
      satadv == "very dissatisfied" ~ 1,
      satadv == "somewhat dissatisfied" ~ 2,
      satadv == "somewhat satisfied" ~ 3,
      satadv == "very satisfied" ~ 4,
      TRUE ~ NA_real_
    ) ,
    year = factor(year) , age = as.numeric(age) , Female = as.numeric(gender == "female"),
    raceth = factor(raceth), dgrdg = factor(dgrdg) , 
    ocedrlp = factor(ocedrlp) , wktrni = as.numeric(wktrni == "yes") 
)

career_adv_sat_model <- lm(satadv ~  age+ Female+ raceth+ dgrdg+ ocedrlp+
                           wktrni+ waprsm+ emsize, data = career_adv_sat_clean, na.action = na.omit)
setwd("/Users/default/Documents/Semester 4/Data Management/PrblemSets/take_home_C")
# Generate LaTeX table code with vertical lines and formatting
# Save to .tex file
print(
  xtable(career_adv_sat_model, 
         caption = "Career advancement satisfaction Summary model related to degree", 
         label = "tab:career advancement satisfaction model"),
  file = "career advancement satisfaction Summary model.tex",  # Output filename
  include.rownames = TRUE,              # Keep variable names
  caption.placement = "top",            # Position of caption
  booktabs = TRUE                      # Professional formatting (recommended)
)







# V Graphs
# V.1   scatter plot showing the relationship between salary and overall job satisfaction
library(ggplot2)
library(dplyr)
library(scales) # For better axis formatting

# 1. Data Preparation
plot_data_scatter <- Job_satisfaction_data %>%
  mutate(
    salary = as.numeric(salary)  # Force conversion to numeric
  ) %>%
  filter(
    !is.na(salary),
    !is.na(jobsatis),
    !is.na(dgrdg),
    salary > 20000 & salary < 2e6  # Reasonable salary range
  ) %>%
  mutate(
    dgrdg = factor(dgrdg,
                   levels = c("bachelor's", "master's", "doctorate", "professional"),
                   labels = c("Bachelor's", "Master's", "Doctorate", "Professional"))
  )

library(ggplot2)
# 2. Create Plot
ggplot(plot_data_scatter, aes(x = salary, y = jobsatis)) +
  geom_point(aes(color = dgrdg), alpha = 0.6, size = 2) +
  geom_smooth(aes(color = dgrdg), method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Salary", y = "Job Satisfaction", 
       color = "Degree Level",
       title = "Salary vs Job Satisfaction by Degree Level") +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave("job_satisfaction_plot.png", 
       plot = simple_plot,
       device = "png",
       width = 8,        # Width in inches
       height = 6,       # Height in inches
       dpi = 300)       # Resolution (dots per inch)



# V.2
# 1 Prepare the data
plot_data_box <- Job_satisfaction_data %>%
  select(dgrdg, ndgmed, satsal) %>%
  filter(!is.na(satsal) & !is.na(ndgmed) & !is.na(dgrdg))%>%
  mutate(dgrdg = case_when(
    dgrdg %in% c("bachelor's", "master's") ~ "Bachelor/Master",
    dgrdg %in% c("doctorate", "professional") ~ "Doctorate/Professional",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(dgrdg))


# 2. Create the box plot
ggplot(plot_data_box, aes(x = ndgmed, y = satsal, fill = dgrdg)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Salary Satisfaction Across Fields of Study by Degree Level",
    x = "Field of Study",
    y = "Salary Satisfaction",
    fill = "Degree Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
