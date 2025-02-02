# ---- script header ----
# script name: analysis_script.R
# purpose of script: analyze survey data
# author: sheila saia
# author email: ssaia@ncsu.edu
# date created: 2022-07-27


# ---- notes ----
# notes:


# ---- to do ----
# to do list:
# questions that need to be coded: Q2a, Q2b, Q2c,

# ---- load libraries ----
library(tidyverse)
library(readxl)
library(here)



# ---- load data ----
# load cleaned data in xlsx sheet
cleaned_data <- readxl::read_xlsx(path = here::here("data", "data_20220315.xlsx"),
                                  sheet = "CleanedData",
                                  col_names = TRUE)

# load question look up table
question_lookup <- readxl::read_xlsx(path = here::here("data", "data_20220315.xlsx"),
                                     sheet = "QuestionLookup",
                                     col_names = TRUE)


# ---- remove responses that did not consent ----
names(cleaned_data)
consented_data <- cleaned_data %>%
  filter(`Informed Consent` == "I consent, begin the survey") %>%
  filter(ResponseId != "R_2s0E9TjxBEMwFzj")
# take out testing data > this was submitted by us as a test and is not valid


# ---- separate out only completed surveys ----
hist(consented_data$Progress, xlab = "Qualtrics Progress (%)", ylab = "Frequency")
# looks like most people were 80% complete or above

# take 100% complete for now
completed_data <- consented_data %>%
  filter(Progress == 100)
# 164/246 people = 66% response rate


# ---- select only columns we need ----
selected_data <- completed_data %>%
  select(ResponseId, Q1:Q17)

# number of complete responses
num_responses <- dim(selected_data)[1]
# num_responses # 164


# ---- question 1 - profession ----
# recode profession categories for select individuals
# (i.e., people that selected "Other" but descriptions indicates "Extension Staff")
q1_data <- selected_data %>%
  mutate(Q1_unique = stringr::str_replace_all(Q1, pattern = "e.g.,", replacement = "e.g.")) %>% # remove comma after e.g. for next step
  separate_rows(Q1_unique, sep = ",") %>%
  select(ResponseId, Q1_unique, Q1_10_TEXT) %>%
  mutate(Q1_recode = case_when(Q1_10_TEXT == "Extension Program Assistant" ~ "Extension Staff",
                               Q1_10_TEXT == "nc state extension master gardener volunteer" ~ "Extension Volunteer",
                               Q1_10_TEXT == "county Ag Econ Director" ~ "State and Federal Agency",
                               Q1_10_TEXT == "County Agency - Conservation" ~ "State and Federal Agency",
                               Q1_10_TEXT == "Soil & Water" ~ "State and Federal Agency",
                               Q1_10_TEXT == "small retail vegetable procucer." ~ "Field or Specialty Crops Farmer",
                               Q1_10_TEXT == "Small scale diverse vegetable producer" ~ "Field or Specialty Crops Farmer",
                               TRUE ~ as.character(Q1_unique))) %>%
  mutate(Q1_short = case_when(Q1_recode == "Other (please specify)" ~ "Other",
                              Q1_recode == "Extension Specialist" ~ "Extension Staff",
                              Q1_recode == "Extension Agent" ~ "Extension Staff",
                              Q1_recode == "State Agency (e.g. NC Department of Environmental Quality)" ~ "State and Federal Agency",
                              Q1_recode == "Forest Owner or Manager" ~ "Forest Owner or Manager",
                              Q1_recode == "Federal Agency (e.g. National Weather Service)" ~ "State and Federal Agency",
                              Q1_recode == "Extension Volunteer (e.g. a Volunteer Master Gardener)" ~ "Extension Volunteer",
                              Q1_recode == "Livestock Farmer" ~ "Livestock Farmer",
                              Q1_recode == "Farmer - Field or Specialty Crops" ~ "Field or Specialty Crops Farmer",
                              is.na(Q1_recode) == TRUE ~ "No Response",
                              TRUE ~ as.character(Q1_recode))) %>%
  mutate(Q1_short = fct_relevel(Q1_short, "Field or Specialty Crops Farmer", "Livestock Farmer", "Forest Owner or Manager", "Extension Staff", "Extension Volunteer", "State and Federal Agency", "Other")) %>%
  select(ResponseId, Q1_short)
# aggregated extension and agency categories to protect privacy

# summarize
q1_summary_data <- q1_data %>%
  group_by(Q1_short) %>%
  summarize(total_count = n()) %>%
  mutate(count_text = paste0("n = ", total_count))

# get percent
q1_summary_data_perc <- q1_summary_data %>%
  mutate(perc = round((total_count / num_responses) * 100, 0),
         perc_text = paste0(perc, " %"))

# jobs lookup (save as more helpful name for later)
job_lookup <- q1_data # %>%
  # left_join(q1_summary_data, by = "Q1_short") %>%
  # select(-count_text)

# job lookup counts per person
job_lookup_selection_counts <- job_lookup %>%
  ungroup() %>%
  group_by(ResponseId) %>%
  summarize(job_count = n())

# plot
# ggplot(data = q1_summary_data) +
#   geom_col(mapping = aes(x = Q1_short, y = total_count)) +
#   geom_text(mapping = aes(x = Q1_short, y = total_count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Respondent Counts by Job Category") +
#   ylim(0, 80) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
# figure 1
png(here::here("figures", "fig_1.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q1_summary_data_perc) +
  geom_col(mapping = aes(x = Q1_short, y = perc)) +
  geom_text(mapping = aes(x = Q1_short, y = perc + 2, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 50) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20))
dev.off()


# ---- question 17 - experience in your field ----
q17_data <- selected_data %>%
  select(ResponseId, Q17) %>%
  mutate(Q17_short = case_when(Q17 == "Less than 1 year" ~ "< 1",
                               Q17 == "1-2 years" ~ "1-2",
                               Q17 == "3-5 years" ~ "3-5",
                               Q17 == "6-10 years" ~ "6-10",
                               Q17 == "11-20 years" ~ "11-20",
                               Q17 == "21 or more years" ~ "> 21",
                               is.na(Q17) == TRUE ~ "No Response")) %>%
  mutate(Q17_short = fct_relevel(Q17_short, "< 1", "1-2", "3-5", "6-10", "11-20","> 21", "No Response"))


# experience lookup table
experience_lookup <- q17_data %>%
  select(ResponseId, Q17_short)

# summarize
q17_summary_data <- q17_data %>%
  group_by(Q17_short) %>%
  summarize(total_count = n()) %>%
  mutate(count_text = paste0("n = ", total_count))

# plot
# ggplot(data = q17_summary_data) +
#   geom_col(mapping = aes(x = Q17_short, y = total_count)) +
#   geom_text(mapping = aes(x = Q17_short, y = total_count + 2, label = count_text)) +
#   labs(x = "Experience (years)", y = "Count", title = "Respondent Counts by Years of Experience") +
#   ylim(0, 40) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# calculate percent
q17_summary_data_perc <- q17_summary_data %>%
  mutate(perc = round((total_count / num_responses) * 100, 0),
         perc_text = paste(perc, "%"))

# plot
# figure s1
png(here::here("figures", "fig_s1.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q17_summary_data_perc) +
  geom_col(mapping = aes(x = Q17_short, y = perc)) +
  geom_text(mapping = aes(x = Q17_short, y = perc + 1, label = perc_text), size = 5) +
  labs(x = "Experience (years)", y = "Responses (%)") +
  ylim(0, 30) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20))
dev.off()


# ---- question 1 and 17 - combine job and years experience into one table ----
# create profession + experience lookup
job_exp_lookup <- job_lookup %>%
  left_join(experience_lookup, by = "ResponseId")

# get levels
job_levels = levels(job_exp_lookup$Q1_short)
experience_levels = levels(job_exp_lookup$Q17_short)
# this looks ok...

# summarize
q1_q17_summary_data <- job_exp_lookup %>%
  group_by(Q1_short, Q17_short) %>%
  summarize(count = n()) %>%
  na.omit() %>% # omit NA's for now
  pivot_wider(names_from = Q17_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `< 1`:`> 21`,
               names_to = "Q17_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 0)) %>%
  mutate(Q17_short = fct_relevel(Q17_short, "< 1", "1-2", "3-5", "6-10", "11-20","> 21")) %>%
  mutate(Q1_short = fct_relevel(Q1_short, "Field or Specialty Crops Farmer", "Livestock Farmer", "Forest Owner or Manager", "Extension Staff", "Extension Volunteer", "State and Federal Agency", "Other"))

# plot
ggplot(data = q1_q17_summary_data) +
  geom_col(mapping = aes(x = Q1_short, y = perc, fill = Q17_short),
           position = position_dodge(width = 0.9, preserve = "single"), col = "grey25") +
  # geom_text(mapping = aes(x = Q3_short, y = percent + 5, label = percent_text)) +
  # facet_wrap(~Q3_short) +
  labs(x = "", y = "Percent (%)", fill = "Experience (years)", title = "Percent of Respondents by Experience and Job") +
  ylim(0, 50) +
  scale_fill_brewer() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# ---- question 2 - crops or livestock of interest ----
# find field and specialty crops
q1_data_crops_lookup <- q1_data %>%
  filter(Q1_short == "Field or Specialty Crops Farmer") %>%
  select(ResponseId)  %>%
  distinct()

# q2 data
q2_data <- selected_data %>%
  select(ResponseId, Q2a:Q2c) %>%
  mutate(Q2a_lower = str_to_lower(Q2a),
         Q2b_lower = str_to_lower(Q2b),
         Q2c_lower = str_to_lower(Q2c)) %>%
  select(ResponseId, Q2a_lower:Q2c_lower) %>%
  distinct()

# q2 field and specialty crops users only
q2_data_crops_users <- q2_data %>%
  right_join(q1_data_crops_lookup, by = "ResponseId") %>%
  select(ResponseId, Q2a_lower) %>%
  na.omit()

d# not really sure how to process this at this time
# will likely have to aggregate it manually

# export for qualitative coding
write_csv(x = q2_data, file = here::here("data", "q2_data.csv"))
write_csv(x = q2_data_crops_users, file = here::here("data", "q2_data_crops_user_grows.csv"))

# ---- question 3 - types of decisions in your job ----
q3_data <- selected_data %>%
  select(ResponseId, Q3, Q3_5_TEXT) %>%
  mutate(Q3_unique = stringr::str_replace_all(Q3, pattern = "e.g.,", replacement = "e.g.")) %>% # remove commas
  mutate(Q3_unique2 = stringr::str_replace_all(Q3_unique, pattern = "when, where, what, and", replacement = "when where what and")) %>% # remove commas
  mutate(Q3_unique_final = stringr::str_replace_all(Q3_unique2, pattern = "seeds, fertilizers, pesticides, insurance", replacement = "seeds fertilizers pesticides insurance")) %>% # remove commas
  separate_rows(Q3_unique_final, sep = ",") %>%
  mutate(Q3_short = case_when(Q3_unique_final == "Production Planning (e.g. when where what and how much to produce; pesticide or fertilizer application; grazing rotations)" ~ "Production Planning",
                              Q3_unique_final == "Agricultural or resource management (e.g. irrigation; pasture management; prescribed burning; water conservation; conservation planning; farm infrastructure)" ~ "Ag./Resource Mgmt.",
                              Q3_unique_final == "Purchases made on an annual basis (e.g. seeds fertilizers pesticides insurance)" ~ "Short-term Purchases",
                              Q3_unique_final == "Purchases made on a longer-term basis (e.g. equipment)" ~ "Long-term Purchases",
                              Q3_unique_final == "Other (please describe)" ~ "Other")) %>%
  mutate(Q3_short = fct_relevel(Q3_short, "Production Planning", "Ag./Resource Mgmt.", "Short-term Purchases", "Long-term Purchases", "Other"))

# average selection per person
q3_summary_selection_count <- q3_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q3_summary_selection_count$count, na.rm = TRUE)
# 2.5 responses per person

# summarize
q3_summary_data <- q3_data %>%
  group_by(Q3_short) %>%
  summarize(total_count = n()) %>%
  mutate(count_text = paste0("n = ", total_count),
         perc = round((total_count / num_responses) * 100, 1),
         perc_text = paste(perc, "%"))

# plot count
ggplot(data = q3_summary_data) +
  geom_col(mapping = aes(x = Q3_short, y = total_count)) +
  geom_text(mapping = aes(x = Q3_short, y = total_count + 5, label = count_text)) +
  labs(x = "", y = "Count", title = "Decisions Made by Category") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# plot percent
ggplot(data = q3_summary_data) +
  geom_col(mapping = aes(x = Q3_short, y = perc)) +
  geom_text(mapping = aes(x = Q3_short, y = perc + 5, label = perc_text)) +
  labs(x = "", y = "Percent (%)", title = "Decisions Made by Category") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q3_summary_data_by_job <- q3_data %>%
  select(-Q3_5_TEXT) %>% # remove for now
  na.omit() %>% # remove NA's for now
  left_join(job_lookup, by = "ResponseId") %>%
  group_by(Q3_short, Q1_short) %>%
  summarize(count = n()) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(percent_text = paste0(percent, "%")) %>%
  mutate(Q3_short = fct_relevel(Q3_short, "Production Planning", "Ag./Resource Mgmt.", "Short-term Purchases", "Long-term Purchases", "Other")) %>%
  mutate(Q1_short = fct_relevel(Q1_short, "Field or Specialty Crops Farmer", "Livestock Farmer", "Forest Owner or Manager", "Extension Staff", "Extension Volunteer", "State and Federal Agency", "Other"))

# plot
ggplot(data = q3_summary_data_by_job) + # %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q3_short, y = percent, fill = Q3_short),
           position = "dodge", color = "grey25") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", fill = "Decision Category") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")

# summarize by experience
q3_summary_data_exp <- q3_data %>%
  select(-Q3_5_TEXT) %>% # remove for now
  na.omit() %>% # remove NA's for now
  left_join(experience_lookup, by = "ResponseId") %>%
  group_by(Q3_short, Q17_short) %>%
  summarize(count = n()) %>%
  na.omit() %>% # remove NA's for now
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(percent_text = paste0(percent, "%"))

# plot
ggplot(data = q3_summary_data_exp) +
  geom_col(mapping = aes(x = Q3_short, y = percent, fill = Q3_short),
           position = "dodge", color = "grey25") +
  labs(x = "", y = "Percent", fill = "Professional Decisions Made") +
  facet_wrap(~ Q17_short) +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")


# ---- question 3 text - look at "other" category ----
q3_data_other <- q3_data %>%
  select(ResponseId, Q3_short, Q3_5_TEXT) %>%
  filter(Q3_short == "Other")

# export for qualitative coding
write_csv(x = q3_data_other, file = here::here("data", "q3_data.csv"))


# ---- question 4a - data types for production planning ----
q4a_data <- selected_data %>%
  select(ResponseId, Q4a) %>%
  separate_rows(Q4a, sep = ",") %>%
  mutate(Q4_short = case_when(Q4a == "Historical climate data or observations" ~ "Historical Weather",
                              Q4a == "Weather over the past 12 months" ~ "Annual Weather",
                              Q4a == "Current weather conditions" ~ "Current Weather",
                              Q4a == "1-7 day weather forecasts" ~ "Daily-Weekly Weather Forecasts",
                              Q4a == "1-3 week weather outlooks" ~ "Weekly Weather Outlooks",
                              Q4a == "Monthly or seasonal climate outlooks" ~ "Monthly-Seasonal Weather Outlooks",
                              Q4a == "Hurricane season outlooks" ~ "Hurricane Season Outlooks",
                              Q4a == "Annual climate outlooks" ~ "Annual Climate Outlooks",
                              Q4a == "Climate projections for the next decade" ~ "Decadal Climate Projections",
                              Q4a == "Climate projections for the next 10-100 years" ~ "Decadal-Century Climate Projections",
                              Q4a == "A tool specifically designed for this decision (please describe the tool)" ~ "Specific Tool",
                              Q4a == "I do not use weather or climate information for this type of decision." ~ "Do Not Use",
                              Q4a == "Other (please specify)" ~ "Other")) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use", "Other")) %>%
  mutate(decision_type = "Production Planning")

# average selection per person
q4a_summary_selection_count <- q4a_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q4a_summary_selection_count$count, na.rm = TRUE)
# 3.2 responses per person

# number of people responding
dim(na.omit(q4a_data))[1]
# 453 non-NA responses (has duplicate people)
length(unique(q4a_summary_selection_count$ResponseId))
# 164 unique people

# summarize
q4a_summary_data <- q4a_data %>%
  group_by(Q4_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Production Planning")

# plot
# ggplot(data = q4a_summary_data) +
#   geom_col(mapping = aes(x = Q4_short, y = count)) +
#   geom_text(mapping = aes(x = Q4_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Information Types Used for Production Planning Decisions") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))
# what to do with "Do Not Use"?

# summarize by job
q4a_summary_data_job <- q4a_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q4a) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Other`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use", "Other")) # %>%
  # mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4a_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q4_short, y = perc),
           position = "dodge", color = "grey25", fill = "grey80") +
  facet_wrap(~Q1_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percent of Respondents Using Weather & Climate Data \nfor Production Planning by Job (excluding Job = 'Other')") +
  ylim(0, 100) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")

# summarize by experience
q4a_summary_data_exp <- q4a_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q4a) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Other`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use", "Other")) # %>%
  # mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4a_summary_data_exp) +
  geom_col(mapping = aes(x = Q4_short, y = perc),
           position = "dodge", color = "grey25", fill = "grey80") +
  facet_wrap(~Q17_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percent of Respondents Using Weather & Climate Data \nfor Production Planning by Experience") +
  ylim(0, 65) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")


# ---- question 4b - data types for short-term (annual) purchases ----
q4b_data <- selected_data %>%
  select(ResponseId, Q4b) %>%
  separate_rows(Q4b, sep = ",") %>%
  mutate(Q4_short = case_when(Q4b == "Historical climate data or observations" ~ "Historical Weather",
                               Q4b == "Weather over the past 12 months" ~ "Annual Weather",
                               Q4b == "Current weather conditions" ~ "Current Weather",
                               Q4b == "1-7 day weather forecasts" ~ "Daily-Weekly Weather Forecasts",
                               Q4b == "1-3 week weather outlooks" ~ "Weekly Weather Outlooks",
                               Q4b == "Monthly or seasonal climate outlooks" ~ "Monthly-Seasonal Weather Outlooks",
                               Q4b == "Hurricane season outlooks" ~ "Hurricane Season Outlooks",
                               Q4b == "Annual climate outlooks" ~ "Annual Climate Outlooks",
                               Q4b == "Climate projections for the next decade" ~ "Decadal Climate Projections",
                               Q4b == "Climate projections for the next 10-100 years" ~ "Decadal-Century Climate Projections",
                               Q4b == "A tool specifically designed for this decision (please describe the tool)" ~ "Specific Tool",
                               Q4b == "I do not use weather or climate information for this type of decision." ~ "Do Not Use")) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use"))  %>%
  mutate(decision_type = "Short-term Purchases")

# average selection per person
q4b_summary_selection_count <- q4b_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q4b_summary_selection_count$count, na.rm = TRUE)
# 2.3 responses per person

# number of people responding
dim(na.omit(q4b_data))[1]
# 294 non-NA responses (has duplicate people)
length(unique(q4b_summary_selection_count$ResponseId))
# 164 unique people

# summarize
q4b_summary_data <- q4b_data %>%
  group_by(Q4_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Short-term Purchases")

# plot
ggplot(data = q4b_summary_data) +
  geom_col(mapping = aes(x = Q4_short, y = count)) +
  geom_text(mapping = aes(x = Q4_short, y = count + 5, label = count_text)) +
  labs(x = "Data Type", y = "Count", title = "Weather and Climate Data Used for Short-Term Purchases") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))
# what to do with "Do Not Use"?

# summarize by job
q4b_summary_data_job <- q4b_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q4b) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather`:`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4b_summary_data_job) +
  geom_col(mapping = aes(x = Q1_short, y = percent, fill = Q4_short),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q4_short) +
  labs(x = "Profession", y = "Percent", fill = "Data Type", title = "Weather and Climate Data Used for Short-Term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q4b_summary_data_exp <- q4b_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q4b) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4b_summary_data_exp) +
  geom_col(mapping = aes(x = Q17_short, y = percent, fill = Q4_short),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q4_short) +
  labs(x = "Years of Experience", y = "Percent", fill = "Data Type", title = "Weather and Climate Data Used for Short-Term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 4c - data types for long-term purchases ----
q4c_data <- selected_data %>%
  select(ResponseId, Q4c) %>%
  separate_rows(Q4c, sep = ",") %>%
  mutate(Q4_short = case_when(Q4c == "Historical climate data or observations" ~ "Historical Weather",
                               Q4c == "Weather over the past 12 months" ~ "Annual Weather",
                               Q4c == "Current weather conditions" ~ "Current Weather",
                               Q4c == "1-7 day weather forecasts" ~ "Daily-Weekly Weather Forecasts",
                               Q4c == "1-3 week weather outlooks" ~ "Weekly Weather Outlooks",
                               Q4c == "Monthly or seasonal climate outlooks" ~ "Monthly-Seasonal Weather Outlooks",
                               Q4c == "Hurricane season outlooks" ~ "Hurricane Season Outlooks",
                               Q4c == "Annual climate outlooks" ~ "Annual Climate Outlooks",
                               Q4c == "Climate projections for the next decade" ~ "Decadal Climate Projections",
                               Q4c == "Climate projections for the next 10-100 years" ~ "Decadal-Century Climate Projections",
                               Q4c == "A tool specifically designed for this decision (please describe the tool)" ~ "Specific Tool",
                               Q4c == "I do not use weather or climate information for this type of decision." ~ "Do Not Use")) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use"))  %>%
  mutate(decision_type = "Long-term Purchases")

# average selection per person
q4c_summary_selection_count <- q4c_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q4c_summary_selection_count$count, na.rm = TRUE)
# 1.6 responses per person


# number of people responding
dim(na.omit(q4c_data))[1]
# 161 non-NA responses (has duplicate people)
length(unique(q4c_summary_selection_count$ResponseId))
# 164 unique people

# summarize
q4c_summary_data <- q4c_data %>%
  group_by(Q4_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))  %>%
  mutate(decision_type = "Long-term Purchases")

# plot
ggplot(data = q4c_summary_data) +
  geom_col(mapping = aes(x = Q4_short, y = count)) +
  geom_text(mapping = aes(x = Q4_short, y = count + 5, label = count_text)) +
  labs(x = "Data Type", y = "Count", title = "Weather and Climate Data Used for Long-Term Purchases") +
  ylim(0, 120) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))
# what to do with "Do Not Use"?

# summarize by job
q4c_summary_data_job <- q4c_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q4c) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4c_summary_data_job) +
  geom_col(mapping = aes(x = Q1_short, y = percent, fill = Q4_short),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q4_short) +
  labs(x = "Profession", y = "Percent", fill = "Data Type", title = "Weather and Climate Data Used for Long-Term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q4c_summary_data_exp <- q4c_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q4c) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4c_summary_data_exp) +
  geom_col(mapping = aes(x = Q17_short, y = percent, fill = Q4_short),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q4_short) +
  labs(x = "Years of Experience", y = "Percent", fill = "Data Type", title = "Weather and Climate Data Used for Long-Term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 4d - data types for ag and resource mgmt ----
q4d_data <- selected_data %>%
  select(ResponseId, Q4d) %>%
  separate_rows(Q4d, sep = ",") %>%
  mutate(Q4_short = case_when(Q4d == "Historical climate data or observations" ~ "Historical Weather",
                               Q4d == "Weather over the past 12 months" ~ "Annual Weather",
                               Q4d == "Current weather conditions" ~ "Current Weather",
                               Q4d == "1-7 day weather forecasts" ~ "Daily-Weekly Weather Forecasts",
                               Q4d == "1-3 week weather outlooks" ~ "Weekly Weather Outlooks",
                               Q4d == "Monthly or seasonal climate outlooks" ~ "Monthly-Seasonal Weather Outlooks",
                               Q4d == "Hurricane season outlooks" ~ "Hurricane Season Outlooks",
                               Q4d == "Annual climate outlooks" ~ "Annual Climate Outlooks",
                               Q4d == "Climate projections for the next decade" ~ "Decadal Climate Projections",
                               Q4d == "Climate projections for the next 10-100 years" ~ "Decadal-Century Climate Projections",
                               Q4d == "A tool specifically designed for this decision (please describe the tool)" ~ "Specific Tool",
                               Q4d == "I do not use weather or climate information for this type of decision." ~ "Do Not Use")) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use"))  %>%
  mutate(decision_type = "Ag./Resource Mgmt.")

# average selection per person
q4d_summary_selection_count <- q4d_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q4d_summary_selection_count$count, na.rm = TRUE)
# 2.7 responses per person

# number of people responding
dim(na.omit(q4d_data))[1]
# 368 non-NA responses (has duplicate people)
length(unique(q4d_summary_selection_count$ResponseId))
# 164 unique people

# summarize
q4d_summary_data <- q4d_data %>%
  group_by(Q4_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Ag./Resource Mgmt.")

# plot
# ggplot(data = q4d_summary_data) +
#   geom_col(mapping = aes(x = Q4_short, y = count)) +
#   geom_text(mapping = aes(x = Q4_short, y = count + 5, label = count_text)) +
#   labs(x = "Data Type", y = "Count", title = "Weather and Climate Data Used for Ag. & Res. Mgmt.") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))
# what to do with "Do Not Use"?

# summarize by job
q4d_summary_data_job <- q4d_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q4d) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4d_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q4_short, y = perc),
           position = "dodge", color = "grey25", fill = "grey80") +
  facet_wrap(~Q1_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percent of Respondents Using Weather & Climate Data \nfor Ag. & Res. Mgmt. by Job (excluding Job = 'Other')") +
  ylim(0, 100) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")

# summarize by experience
q4d_summary_data_exp <- q4d_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q4d) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q4_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q4_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Historical Weather` :`Do Not Use`,
               names_to = "Q4_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q4_short = fct_relevel(Q4_short, "Historical Weather", "Current Weather", "Daily-Weekly Weather Forecasts", "Weekly Weather Outlooks", "Monthly-Seasonal Weather Outlooks", "Hurricane Season Outlooks", "Annual Weather", "Annual Climate Outlooks", "Decadal Climate Projections", "Decadal-Century Climate Projections", "Specific Tool", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q4d_summary_data_exp) +
  geom_col(mapping = aes(x = Q4_short, y = percent),
           position = "dodge", color = "grey25", fill = "grey80") +
  facet_wrap(~Q17_short) +
  labs(x = "", y = "Percent", fill = "Data Type", title = "Percent of Respondents Using Weather & Climate Data \nfor Ag. & Res. Mgmt.") +
  ylim(0, 100) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- combine summaries of 4a - 4d into one ----
q4_summary_data <- bind_rows(q4a_summary_data, q4b_summary_data, q4c_summary_data, q4d_summary_data) %>%
  na.omit() %>% # drop this b/c this is the # that didn't select the decision category
  mutate(decision_type = fct_relevel(decision_type, "Production Planning", "Ag./Resource Mgmt.", "Short-term Purchases", "Long-term Purchases"))

# plot
# ggplot(data = q4_summary_data) +
#   geom_col(mapping = aes(x = Q4_short, y = count, fill = decision_type),
#            position = "dodge", color = "grey25") +
#   # geom_text(mapping = aes(x = Q4_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Information Used for Various Professional Decisions") +
#   ylim(0, 100) +
#   scale_fill_brewer(palette = "Set3") +
#   facet_wrap(~ decision_type) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14),
#         legend.position = "none")
# what to do with "Do Not Use"?

# percent
q4_summary_data_perc <- q4_summary_data %>%
  select(-count_text) %>%
  left_join(q3_summary_data, by = c("decision_type" = "Q3_short")) %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 1))

# plot
# figure 3
png(here::here("figures", "fig_3.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q4_summary_data_perc) +
  geom_col(mapping = aes(x = Q4_short, y = perc, fill = decision_type),
           position = "dodge", color = "grey25") +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ decision_type) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 16),
        legend.position = "none")
dev.off()


# ---- question 5a - concern for climate impacts on production planning ----
q5a_data <- selected_data %>%
  select(ResponseId, Q5 = Q5_1) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) %>%
  mutate(decision_type = "Production Planning")

# summarize
q5a_summary_data <- q5a_data %>%
  group_by(Q5) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Production Planning")

# plot
ggplot(data = q5a_summary_data) +
  geom_col(mapping = aes(x = Q5, y = count)) +
  geom_text(mapping = aes(x = Q5, y = count + 5, label = count_text)) +
  labs(x = "Concern for Climate Change Impacts on Production Planning Decisions", y = "Count") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q5a_summary_data_job <- q5a_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5a_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q5, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percentage of Respondents vs Concern for Climate Impacts \non Production Planning by Job (excluding Job = 'Other')") +
  ylim(0, 50) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q5a_summary_data_exp <- q5a_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5a_summary_data_exp %>% filter(Q17_short != "Other")) +
  geom_col(mapping = aes(x = Q5, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percentage of Respondents vs Concern for Climate Impacts \non Production Planning by Experience") +
  ylim(0, 50) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 5b - concern for climate impacts on short-term (annual) purchases ----
q5b_data <- selected_data %>%
  select(ResponseId, Q5 = Q5_2) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) %>%
  mutate(decision_type = "Short-term Purchases")

# summarize
q5b_summary_data <- q5b_data %>%
  group_by(Q5) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Short-term Purchases")

# plot
ggplot(data = q5b_summary_data) +
  geom_col(mapping = aes(x = Q5, y = count)) +
  geom_text(mapping = aes(x = Q5, y = count + 5, label = count_text)) +
  labs(x = "Concern for Climate Change Impacts on Annual Purchase Decisions", y = "Count") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q5b_summary_data_job <- q5b_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5b_summary_data_job) +
  geom_col(mapping = aes(x = Q1_short, y = percent, fill = Q5),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q5) +
  labs(x = "Profession", y = "Percent", fill = "Data Type", title = "Concern for Climate Impacts on Short-term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q5b_summary_data_exp <- q5b_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5b_summary_data_exp) +
  geom_col(mapping = aes(x = Q17_short, y = percent, fill = Q5),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q5) +
  labs(x = "Years of Experience", y = "Percent", fill = "Data Type", title = "Concern for Climate Impacts on Short-term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 5c - concern for climate impacts on long-term (annual) purchases ----
q5c_data <- selected_data %>%
  select(ResponseId, Q5 = Q5_3) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) %>%
  mutate(decision_type = "Long-term Purchases")

# summarize
q5c_summary_data <- q5c_data %>%
  group_by(Q5) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Long-term Purchases")

# plot
ggplot(data = q5c_summary_data) +
  geom_col(mapping = aes(x = Q5, y = count)) +
  geom_text(mapping = aes(x = Q5, y = count + 5, label = count_text)) +
  labs(x = "Concern for Climate Change Impacts on Long-term Purchase Decisions", y = "Count") +
  ylim(0, 115) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q5c_summary_data_job <- q5c_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5c_summary_data_job) +
  geom_col(mapping = aes(x = Q1_short, y = percent, fill = Q5),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q5) +
  labs(x = "Profession", y = "Percent", fill = "Data Type", title = "Concern for Climate Impacts on Long-term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q5c_summary_data_exp <- q5c_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5c_summary_data_exp) +
  geom_col(mapping = aes(x = Q17_short, y = percent, fill = Q5),
           position = "dodge", color = "grey25") +
  # facet_wrap(~Q5) +
  labs(x = "Years of Experience", y = "Percent", fill = "Data Type", title = "Concern for Climate Impacts on Long-term Purchases") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 5d - concern for climate impacts on ag and resource mgmt ----
q5d_data <- selected_data %>%
  select(ResponseId, Q5 = Q5_4) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) %>%
  mutate(decision_type = "Ag./Resource Mgmt.")

# summarize
q5d_summary_data <- q5d_data %>%
  group_by(Q5) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(decision_type = "Ag./Resource Mgmt.")

# plot
ggplot(data = q5d_summary_data) +
  geom_col(mapping = aes(x = Q5, y = count)) +
  geom_text(mapping = aes(x = Q5, y = count + 5, label = count_text)) +
  labs(x = "Concern for Climate Change Impacts on Ag. & Resource Mgmt. Decisions", y = "Count") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q5d_summary_data_job <- q5d_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5d_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q5, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percentage of Respondents vs Concern for Climate Impacts \non Ag. & Res. Mgmt. by Job (excluding Job = 'Other')") +
  ylim(0, 50) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q5d_summary_data_exp <- q5d_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q5, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q5,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `A great deal` :`None at all`,
               names_to = "Q5",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q5 = fct_relevel(Q5, "A great deal", "A lot", "A moderate amount", "A little", "None at all")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q5d_summary_data_exp %>% filter(Q17_short != "Other")) +
  geom_col(mapping = aes(x = Q5, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", fill = "Data Type", title = "Percentage of Respondents vs Concern for Climate Impacts \non Ag. & Res. Mgmt. by Experience") +
  ylim(0, 50) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- combine summaries of 5a - 5d into one ----
q5_summary_data <- bind_rows(q5a_summary_data, q5b_summary_data, q5c_summary_data, q5d_summary_data) %>%
  na.omit() %>% # drop this b/c this is the # that didn't select the decision category
  mutate(decision_type = fct_relevel(decision_type, "Production Planning", "Ag./Resource Mgmt.", "Short-term Purchases", "Long-term Purchases"))

# plot
# ggplot(data = q5_summary_data) +
#   geom_col(mapping = aes(x = Q5, y = count, fill = decision_type),
#            position = "dodge", color = "grey25") +
#   geom_text(mapping = aes(x = Q5, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Concern for Climate Change Impacting Various Professional Decisions") +
#   ylim(0, 100) +
#   scale_fill_brewer(palette = "Set3") +
#   facet_wrap(~ decision_type) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14),
#         legend.position = "none")
# what to do with "Do Not Use"?

# percent
q5_summary_data_perc <- q5_summary_data %>%
  select(-count_text) %>%
  left_join(q3_summary_data, by = c("decision_type" = "Q3_short")) %>%
  select(-count_text) %>%
  mutate(perc = round((count / total_count) * 100, 1),
         perc_text = paste(perc, "%"))

# plot
# figure s3
png(here::here("figures", "fig_s3.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q5_summary_data_perc) +
  geom_col(mapping = aes(x = Q5, y = perc, fill = decision_type),
           position = "dodge", color = "grey25") +
  geom_text(mapping = aes(x = Q5, y = perc + 2, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 50) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ decision_type) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 16),
        legend.position = "none")
dev.off()




# ---- question 6 - thinking about climate change in your work ----
q6_data <- selected_data %>%
  select(ResponseId, Q6) %>%
  mutate(Q6 = fct_relevel(Q6, "All the time", "Often", "Sometimes", "Rarely", "Never"))

# summarize
q6_summary_data <- q6_data %>%
  group_by(Q6) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(Q6_fix = case_when(Q6 == "All the time" ~ "All the time",
                            Q6 == "Often" ~ "Often",
                            Q6 == "Sometimes" ~ "Sometimes",
                            Q6 == "Rarely" ~ "Rarely",
                            Q6 == "Never" ~ "Never",
                            is.na(Q6) == TRUE ~ "No Response"),
         Q6_fix = fct_relevel(Q6_fix, "All the time", "Often", "Sometimes", "Rarely", "Never", "No Response"))

# plot
# ggplot(data = q6_summary_data) +
#   geom_col(mapping = aes(x = Q6, y = count)) +
#   geom_text(mapping = aes(x = Q6, y = count + 2, label = count_text)) +
#   labs(x = "", y = "Count", title = "Time Spent Thinking About Climate Change") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# percent
q6_summary_data_perc <- q6_summary_data %>%
  mutate(perc = round((count / num_responses) * 100, 1),
         perc_text = paste(perc, "%"))
  

# plot percent
# figure s4
png(here::here("figures", "fig_s4.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q6_summary_data_perc) +
  geom_col(mapping = aes(x = Q6_fix, y = perc)) +
  geom_text(mapping = aes(x = Q6_fix, y = perc + 2, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 50) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20))
dev.off()

# summarize by job
q6_summary_data_job <- q6_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q6, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q6,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `All the time` :`Never`,
               names_to = "Q6",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q6 = fct_relevel(Q6, "All the time", "Often", "Sometimes", "Rarely", "Never")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q6_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q6, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~Q1_short) +
  labs(x = "", y = "Percen (%)", fill = "Frequency", title = "Percent of Respondents vs Time Spent Thinking About Climate Change \nby Job (excluding Job = 'Other')") +
  ylim(0, 60) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")

# summarize by experience
q6_summary_data_exp <- q6_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q6, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q6,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `All the time` :`Never`,
               names_to = "Q6",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q6 = fct_relevel(Q6, "All the time", "Often", "Sometimes", "Rarely", "Never")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q6_summary_data_exp) +
  geom_col(mapping = aes(x = Q6, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", title = "Percent of Respondents vs Time Spend Thinking About Climate Change by Experience") +
  ylim(0, 60) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 7 - using future climate data in your work ----
q7_data <- selected_data %>%
  select(ResponseId, Q7) %>%
  mutate(Q7_short = case_when(Q7 == "I'm starting to learn about and use this, but would like assistance" ~ "Starting to use and need help",
                              Q7 == "I am not currently using this, but I see myself using it someday" ~ "No experience but will use someday",
                              Q7 == "I am actively using this to inform my decisions or planning" ~ "Using now",
                              Q7 == "I am not currently using this, nor do I see myself using it" ~ "Not using and not interested",
                              Q7 == "I would like to use this, but I don't know where to begin" ~ "No experience but need help",
                              is.na(Q7) == TRUE ~ "No Response")) %>%
           mutate(Q7_short = fct_relevel(Q7_short, "Using now", "Starting to use and need help", "No experience but will use someday", "No experience but need help", "Not using and not interested", "No Response"))

# summarize
q7_summary_data <- q7_data %>%
  group_by(Q7_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
# ggplot(data = q7_summary_data) +
#   geom_col(mapping = aes(x = Q7_short, y = count)) +
#   geom_text(mapping = aes(x = Q7_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Current Climate Data Use") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# percent
q7_summary_data_perc <- q7_summary_data %>%
  mutate(perc = round((count / num_responses) * 100, 1),
         perc_text = paste(perc, "%"))

# plot percent
# figure s5
png(here::here("figures", "fig_s5.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q7_summary_data_perc) +
  geom_col(mapping = aes(x = Q7_short, y = perc)) +
  geom_text(mapping = aes(x = Q7_short, y = perc + 2, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 50) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20))
dev.off()

# summarize by job
q7_summary_data_job <- q7_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q7) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q7_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q7_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Using now` :`Not using and not interested`,
               names_to = "Q7_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q7_short = fct_relevel(Q7_short, "Using now", "Starting to use and need help", "No experience but will use someday", "No experience but need help", "Not using and not interested")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot percent
ggplot(data = q7_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q7_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", title = "Percent Respondents vs Use of Future Climate Data at Work \nby Job (exluding Job = 'Other')") +
  ylim(0, 60) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q7_summary_data_exp <- q7_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q7) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q7_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q7_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Using now` :`Not using and not interested`,
               names_to = "Q7_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q7_short = fct_relevel(Q7_short, "Using now", "Starting to use and need help", "No experience but will use someday", "No experience but need help", "Not using and not interested")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q7_summary_data_exp) +
  geom_col(mapping = aes(x = Q7_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Use of Future Climate Data at Work by Experience") +
  ylim(0, 50) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 8a - parameters used for professional work ----
q8a_data <- selected_data %>%
  select(ResponseId, Q8a:Q8b_9) %>%
  mutate(Q8_unique = stringr::str_replace_all(Q8a, pattern = ", " , replacement = " ")) %>%
  separate_rows(Q8_unique, sep = ",") %>%
  select(ResponseId, Q8_unique) %>%
  mutate(Q8_short = case_when(Q8_unique == "Temperature" ~ "Temperature",
                              Q8_unique == "Precipitation" ~ "Precipitation",
                              Q8_unique == "Wind Speed" ~ "Wind Speed",
                              Q8_unique == "Wind Direction" ~ "Wind Direction",
                              Q8_unique == "Soil Moisture" ~ "Soil Moisture",
                              Q8_unique == "Soil Temperature" ~ "Soil Temperature",
                              Q8_unique == "Solar Radiation" ~ "Solar Radiation",
                              Q8_unique == "Relative Humidity" ~ "Relative Humidity",
                              Q8_unique == "Streamflow" ~ "Streamflow",
                              Q8_unique == "Air Pressure" ~ "Air Pressure",
                              Q8_unique == "Drought indices like Standard Precipitation Index (SPI) or the US Drought Monitor drought categories" ~ "Drought Indices",
                              Q8_unique == "Other parameters such as growing degree days or chilling hours" ~ "GDD and Chilling Hours",
                              Q8_unique == "Other parameters not listed here (please describe)" ~ "Other",
                              is.na(Q8_unique) == TRUE ~ "No Response")) %>%
  mutate(Q8_short = fct_relevel(Q8_short, "Temperature", "Precipitation", "Air Pressure", "Relative Humidity", "Solar Radiation", "Wind Speed", "Wind Direction", "Soil Moisture", "Soil Temperature", "Streamflow", "Drought Indices", "GDD and Chilling Hours", "Other", "No Response")) %>%
  select(-Q8_unique)

# summarize
q8a_summary_data <- q8a_data %>%
  group_by(Q8_short) %>%
  summarize(count = n()) %>%
  mutate(perc = round((count / num_responses) * 100, 0),
         perc_text = paste0(perc, " %"))

# plot
# figure 2
png(here::here("figures", "fig_2.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q8a_summary_data) +
  geom_col(mapping = aes(x = Q8_short, y = perc)) +
  geom_text(mapping = aes(x = Q8_short, y = perc + 2, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20),
        legend.position = "none")
dev.off()


# ---- question 8b - ranking of parameters used for professional work ----
# wrangle data and use custom function
q8b_data <- selected_data %>%
  select(ResponseId, Q8a:Q8b_13_TEXT) %>%
  mutate(Q8_unique = stringr::str_replace_all(Q8a, pattern = ", " , replacement = " ")) %>%
  mutate(Q8_short = stringr::str_replace_all(Q8_unique, c("Drought indices like Standard Precipitation Index \\(SPI\\) or the US Drought Monitor drought categories" = "Drought Indices",
                                                          "Other parameters such as growing degree days or chilling hours" = "GDD and Chilling Hours",
                                                          "Other parameters not listed here \\(please describe\\)" = "Other"))) %>%
  select(ResponseId, Q8_short, Q8b_1:Q8b_13_TEXT) %>%
  unite("rank_list", Q8b_1:Q8b_13, sep = ",", na.rm = TRUE) %>%
  mutate(parameter_count = stringr::str_count(Q8_short, pattern = ",") + 1,
         rank_count = stringr::str_count(rank_list, pattern = ",") + 1,
         check = if_else(parameter_count == rank_count, TRUE, FALSE)) %>%
  filter(check == TRUE) %>% # removes entries without ranks, n = 122 %>%
  mutate(rank_list_fix = if_else(str_count(rank_list) == 0, "1", rank_list)) %>% # adds rank 1 to rank list in event that person only selected one parameter
  select(ResponseId, Q8_short, rank_list_fix)

# make custom purrr map function to run on each row
tidy_rank_data <- function(ResponseId, Q8_short, rank_list_fix) {
  # input must look like a dataframe with three columnns
  # ResponseID is first column
  # Q8_short is second column with comma separated string of parameter names
  # rank_list_fix is third column with comma separated string of ranks for each corresponding parameter name
  # order of entry in the list matters/must be conserved for second and third column and these columns must also be equal!

  # get id
  ResponseId_temp <- ResponseId

  # convert to list format
  param_list <- str_split(Q8_short, pattern = ",")
  ranks_list <- str_split(rank_list_fix, pattern = ",")

  # combine list
  combo_list <- c(param_list, ranks_list)

  # reformat list into tibble data.frame
  combo_dataframe <- exec(bind_cols, combo_list)
  colnames(combo_dataframe) <- c("parameter", "parameter_rank")

  # finalize dataframe
  final_dataframe <- data.frame(ResponseId = rep(ResponseId_temp, dim(combo_dataframe)[1]),
                                parameter = as.character(combo_dataframe$parameter),
                                parameter_rank = as.numeric(combo_dataframe$parameter_rank))

  # return result
  return(final_dataframe)
}

# make empty dataframe to append to
q8b_data_tidy <- NULL

# finalize data wrangling
for (i in 1:dim(q8b_data)[1]) {
  # get temp data for custom function
  ResponseId_temp <- q8b_data$ResponseId[i]
  Q8_short_temp <- q8b_data$Q8_short[i]
  rank_list_fix_temp <- q8b_data$rank_list_fix[i]

  # run function
  temp_df <- tidy_rank_data(ResponseId_temp, Q8_short_temp, rank_list_fix_temp)

  # append
  q8b_data_tidy <- bind_rows(q8b_data_tidy, temp_df)
}

# # summarize
q8b_data_tidy_summarize <- q8b_data_tidy %>%
  filter(parameter_rank <= 3) %>%
  group_by(parameter_rank, parameter) %>%
  summarize(perc = round(n()/num_responses * 100, 0),
            perc_text = paste0(perc, " %")) %>%
  arrange(parameter_rank, desc(perc))

# plot
ggplot(data = q8b_data_tidy_summarize) +
  geom_col(mapping = aes(x = reorder(parameter, -perc), y = perc), color = "black", fill = "grey80") +
  # geom_text(mapping = aes(x = Q8_short, y = perc + 2, label = perc_text)) +
  facet_wrap(~ parameter_rank) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents versus 1st, 2nd, 3rd Ranked Parameters") +
  ylim(0, 40) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14),
        legend.position = "none")



# ---- question 9 - parameters used ----
q9_data <- selected_data %>%
  select(ResponseId, Q9, Q9_6_TEXT) %>%
  separate_rows(Q9, sep = ",") %>%
  mutate(Q9_short = case_when(Q9 == "Cooling degree days" ~ "CDD",
                              Q9 == "Growing degree days" ~ "GDD",
                              Q9 == "Stress degree days" ~ "SDD",
                              Q9 == "Chilling hours" ~ "Chilling hours",
                              Q9 == "Number of days since last frost or freeze" ~ "Days since last frost/freeze",
                              Q9 == "Other (please specify)" ~ "Other",
                              is.na(Q9) == TRUE ~ "No Response")) %>%
  mutate(Q9_short = fct_relevel(Q9_short, "CDD", "SDD", "GDD", "Chilling hours", "Days since last frost/freeze", "Other", "No Response"))
# will need to code "other" by hand

# average selection per person
q9_summary_selection_count <- q9_data %>%
  group_by(ResponseId) %>%
  summarize(count = n())

# find mean
mean(q9_summary_selection_count$count, na.rm = TRUE)
# 1.95 responses per person

# summarize
q9_summary_data <- q9_data %>%
  group_by(Q9_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = paste0("n = ", count)) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count))) %>%
  mutate(perc = round((count / num_responses) * 100, 0),
         perc_text = paste0(perc, " %"))

# plot
# ggplot(data = q9_summary_data) +
#   geom_col(mapping = aes(x = Q9_short, y = count)) +
#   geom_text(mapping = aes(x = Q9_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Number of Respondents Who Use Various Parameters (cont.)") +
#   ylim(0, 120) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
# figure s2
png(here::here("figures", "fig_s2.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = q9_summary_data) +
  geom_col(mapping = aes(x = Q9_short, y = perc)) +
  geom_text(mapping = aes(x = Q9_short, y = perc + 5, label = perc_text), size = 5) +
  labs(x = "", y = "Responses (%)") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20))
dev.off()

# summarize by job
q9_summary_data_job <- q9_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q9, -Q9_6_TEXT) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q9_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q9_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `CDD` :`Other`,
               names_to = "Q9_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q9_short = fct_relevel(Q9_short, "CDD", "SDD", "GDD", "Chilling hours", "Days since last frost or freeze", "Other")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot by job
ggplot(data = q9_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q9_short, y = percent),
           position = "dodge", color = "grey25", fill = "grey80") +
  facet_wrap(~Q1_short) +
  labs(x = "Profession", y = "Percent", fill = "Parameter Type", title = "Parameters Used (excpet Job = 'Other')") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q9_summary_data_exp <- q9_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q9, -Q9_6_TEXT) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q9_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q9_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `CDD` :`Other`,
               names_to = "Q9_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q9_short = fct_relevel(Q9_short, "CDD", "SDD", "GDD", "Chilling hours", "Days since last frost or freeze", "Other")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot by experience
ggplot(data = q9_summary_data_exp) +
  geom_col(mapping = aes(x = Q9_short, y = percent),
           position = "dodge", color = "grey25", fill = "grey90") +
  facet_wrap(~Q17_short) +
  labs(x = "Years of Experience", y = "Percent", fill = "Parameter Type", title = "Parameters Used") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# look at "other" data
q9_data_other <- q9_data %>%
  select(ResponseId, Q9_short, Q9_6_TEXT) %>%
  filter(Q9_short == "Other")

# growing/planting/USDA zone


# ---- question 10 - freq. of time seeking for weather info ----
q10_data <- selected_data %>%
  select(ResponseId, Q10) %>%
  mutate(Q10_short = case_when(Q10 == "Daily" ~ "Daily",
                               Q10 == "Monthly" ~ "Monthly",
                               Q10 == "More than once per day" ~ "More than Once per Day",
                               Q10 == "I do not use weather information in this context." ~ "Do Not Use",
                               Q10 == "Weekly" ~ "Weekly",
                               Q10 == "Less than once per month but more than once every 6 months" ~ "Yearly",
                               Q10 == "Less than once every 6 months but more than once per year" ~ "Yearly")) %>%
  mutate(Q10_short = fct_relevel(Q10_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"))

# summary of data
q10_summary_data <- q10_data %>%
  group_by(Q10_short) %>%
  summarize(count = n()) %>%
  mutate(perc = round(count / num_responses * 100, 0),
         perc_text = paste0(perc, " %"),
         count_text = paste0("n = ", count))
# aggregate to yearly to protect privacy

# plot count
# ggplot(data = q10_summary_data) +
#   geom_col(mapping = aes(x = Q10_short, y = count)) +
#   geom_text(mapping = aes(x = Q10_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Time Spent Seeking Out Weather Information") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
ggplot(data = q10_summary_data) +
  geom_col(mapping = aes(x = Q10_short, y = perc)) +
  geom_text(mapping = aes(x = Q10_short, y = perc + 2, label = perc_text)) +
  labs(x = "", y = "Percentage (%)", title = "Percentage of the Time Respondents Spent Seeking Out Weather Information") +
  ylim(0, 60) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q10_summary_data_job <- q10_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q10) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q10_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q10_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `More than Once per Day` :`Do Not Use`,
               names_to = "Q10_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q10_short = fct_relevel(Q10_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"))
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q10_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q10_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~Q1_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of the Time Respondents Spent Seeking Out Weather Information \nby Job (excluding Job = 'Other')") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q10_summary_data_exp <- q10_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q10) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q10_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q10_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `More than Once per Day` :`Do Not Use`,
               names_to = "Q10_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q10_short = fct_relevel(Q10_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"))
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q10_summary_data_exp) +
  geom_col(mapping = aes(x = Q10_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~Q17_short) +
  labs(x = "", y = "Percent (%)", fill = "Frequency", title = "Percentage of the Time Respondents Spent Seeking Out Weather Information by Experience") +
  ylim(0, 100) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 11 - satisfaction of weather info ----
q11_data <- selected_data %>%
  select(ResponseId, Q11) %>%
  mutate(Q11 = fct_relevel(Q11, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied"))

# summary of data
q11_summary_data <- q11_data %>%
  group_by(Q11) %>%
  summarize(count = n()) %>%
  mutate(perc = round(count / num_responses * 100, 0),
         perc_text = paste0(perc, " %"),
         count_text = paste0("n = ", count))

# plot count
# ggplot(data = q11_summary_data) +
#   geom_col(mapping = aes(x = Q11, y = count)) +
#   geom_text(mapping = aes(x = Q11, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Satisfaction with Weather Information") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
ggplot(data = q11_summary_data) +
  geom_col(mapping = aes(x = Q11, y = perc)) +
  geom_text(mapping = aes(x = Q11, y = perc + 2, label = perc_text)) +
  labs(x = "", y = "Percentage (%)", title = "Percentage of Respondents and Their Satisfaction with Weather Information") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q11_summary_data_job <- q11_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q11, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q11,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Extremely satisfied` :`Somewhat dissatisfied`,
               names_to = "Q11",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q11 = fct_relevel(Q11, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q11_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q11, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Satisfaction with Weather Information Sources \nby Job (excluding Job = 'Other')") +
  ylim(0, 80) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q11_summary_data_exp <- q11_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q11, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q11,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Extremely satisfied` :`Somewhat dissatisfied`,
               names_to = "Q11",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q11 = fct_relevel(Q11, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q11_summary_data_exp) +
  geom_col(mapping = aes(x = Q11, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Satisfaction with Weather Information Sources by Experience") +
  ylim(0, 100) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- cross question 10 and 11 ----
q10_q11_join_data <- selected_data %>%
  select(ResponseId) %>%
  left_join(q10_data, by = "ResponseId") %>%
  left_join(q11_data, by = "ResponseId") %>%
  select(-Q10, Q10 = Q10_short) %>%
  na.omit() # remove NA's for now

# summarize
q10_q11_summary_data <- q10_q11_join_data %>%
  ungroup() %>%
  group_by(Q10, Q11) %>%
  summarize(count = n()) %>%
  mutate(Q10 = fct_relevel(Q10, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"),
         Q11 = fct_relevel(Q11, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied"),
         perc = round(count / num_responses * 100, 0)) %>%
  mutate(data_type = "weather")

# plot heatmap of percents
ggplot(data = q10_q11_summary_data) +
  geom_tile(mapping = aes(x = Q10, y = Q11, fill = perc)) +
  labs(x = "Freq. of Time Spent Seeking Weather Info.", y = "Satisfaction w/ Weather Info.", title = "", fill = "Percent (%)") +
  scale_fill_gradient2(low = "white",
                       high = "grey20") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 12 - freq. of time seeking for climate info ----
q12_data <- selected_data %>%
  select(ResponseId, Q12) %>%
  mutate(Q12_short = case_when(Q12 == "Daily" ~ "Daily",
                               Q12 == "Monthly" ~ "Monthly",
                               Q12 == "More than once per day" ~ "More than Once per Day",
                               Q12 == "I do not use climate information in this context." ~ "Do Not Use",
                               Q12 == "Weekly" ~ "Weekly",
                               Q12 == "Less than once per month but more than once every 6 months" ~ "Yearly",
                               Q12 == "Less than once every 6 months but more than once per year" ~ "Yearly")) %>%
  mutate(Q12_short = fct_relevel(Q12_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"))

# summary of data
q12_summary_data <- q12_data %>%
  group_by(Q12_short) %>%
  summarize(count = n()) %>%
  mutate(perc = round(count / num_responses * 100, 0),
         perc_text = paste0(perc, " %"),
         count_text = paste0("n = ", count))
# aggregate to yearly to protect privacy

# plot count
# ggplot(data = q12_summary_data) +
#   geom_col(mapping = aes(x = Q12_short, y = count)) +
#   geom_text(mapping = aes(x = Q12_short, y = count + 2, label = count_text)) +
#   labs(x = "", y = "Count", title = "Time Spent Seeking Out Climate Information") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
ggplot(data = q12_summary_data) +
  geom_col(mapping = aes(x = Q12_short, y = perc)) +
  geom_text(mapping = aes(x = Q12_short, y = perc + 2, label = perc_text)) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Time Spent Seeking Out Climate Information") +
  ylim(0, 40) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q12_summary_data_job <- q12_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  select(-Q12) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q12_short, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q12_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `More than Once per Day` :`Do Not Use`,
               names_to = "Q12_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q12_short = fct_relevel(Q12_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q12_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q12_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Frequency of Time Seeking Out Climate Information \nby Job (excluding Job = 'Other')") +
  ylim(0, 60) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q12_summary_data_exp <- q12_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  select(-Q12) %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q12_short, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q12_short,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `More than Once per Day` :`Do Not Use`,
               names_to = "Q12_short",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q12_short = fct_relevel(Q12_short, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use")) # %>%
# mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)))

# plot
ggplot(data = q12_summary_data_exp) +
  geom_col(mapping = aes(x = Q12_short, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Frequency of Time Seeking Out Climate Information \nby Experience") +
  ylim(0, 40) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- question 13 - satisfaction of climate info ----
q13_data <- selected_data %>%
  select(ResponseId, Q13) %>%
  mutate(Q13 = fct_relevel(Q13, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied"))

# summary of data
q13_summary_data <- q13_data %>%
  group_by(Q13) %>%
  summarize(count = n()) %>%
  mutate(perc = round(count / num_responses * 100, 0),
         perc_text = paste0(perc, " %"),
         count_text = paste0("n = ", count))

# plot count
# ggplot(data = q13_summary_data) +
#   geom_col(mapping = aes(x = Q13, y = count)) +
#   geom_text(mapping = aes(x = Q13, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Satisfaction with Climate Information") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
ggplot(data = q13_summary_data) +
  geom_col(mapping = aes(x = Q13, y = perc)) +
  geom_text(mapping = aes(x = Q13, y = perc + 2, label = perc_text)) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Satisfaction with Climate Information") +
  ylim(0, 50) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by job
q13_summary_data_job <- q13_data %>%
  left_join(job_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q13, Q1_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q13,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Extremely satisfied`:`Extremely dissatisfied`,
               names_to = "Q13",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q1_summary_data, by = "Q1_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q13 = fct_relevel(Q13, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied"))

# plot
ggplot(data = q13_summary_data_job %>% filter(Q1_short != "Other")) +
  geom_col(mapping = aes(x = Q13, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q1_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Satisfication with Climate Information \nby Job (excluding Job = 'Other')") +
  ylim(0, 60) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# summarize by experience
q13_summary_data_exp <- q13_data %>%
  left_join(experience_lookup, by = "ResponseId") %>%
  na.omit() %>% # drop NA's for now
  ungroup() %>%
  group_by(Q13, Q17_short) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = Q13,
              values_from = count,
              values_fill = 0) %>%
  pivot_longer(cols = `Extremely satisfied`:`Extremely dissatisfied`,
               names_to = "Q13",
               values_to = "count",
               values_drop_na = FALSE) %>%
  left_join(q17_summary_data, by = "Q17_short") %>%
  select(-count_text) %>%
  mutate(percent = round((count / total_count) * 100, 0)) %>%
  mutate(Q13 = fct_relevel(Q13, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Extremely dissatisfied"))

# plot
ggplot(data = q13_summary_data_exp) +
  geom_col(mapping = aes(x = Q13, y = percent),
           position = "dodge", color = "black", fill = "grey80") +
  facet_wrap(~ Q17_short) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Satisfication with Climate Information \nby Experience") +
  ylim(0, 80) +
  # scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# ---- cross question 12 and 13 ----
q12_q13_join_data <- selected_data %>%
  select(ResponseId) %>%
  left_join(q12_data, by = "ResponseId") %>%
  left_join(q13_data, by = "ResponseId") %>%
  select(-Q12, Q12 = Q12_short) %>%
  na.omit() # remove NA's for now

# summarize
q12_q13_summary_data <- q12_q13_join_data %>%
  ungroup() %>%
  group_by(Q12, Q13) %>%
  summarize(count = n()) %>%
  mutate(Q12 = fct_relevel(Q12, "More than Once per Day", "Daily", "Weekly", "Monthly", "Yearly", "Do Not Use"),
         Q13 = fct_relevel(Q13, "Extremely satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied"),
         perc = round(count / num_responses * 100, 0)) %>%
  mutate(data_type = "climate")

# plot heatmap of percents
ggplot(data = q12_q13_summary_data) +
  geom_tile(mapping = aes(x = Q12, y = Q13, fill = perc)) +
  labs(x = "Freq. of Time Spent Seeking Climate Info.", y = "Satisfaction w/ Climate Info.", title = "", fill = "Percent (%)") +
  scale_fill_gradient2(low = "white",
                       high = "grey20") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))


# ---- heat maps for weather and climate plots ----
# reformat so can bind rows
q10_q11_summary_data_fix <- q10_q11_summary_data %>%
  select(frequency = Q10,
         satisfaction = Q11,
         count:data_type)
q12_q13_summary_data_fix <- q12_q13_summary_data %>%
  select(frequency = Q12,
         satisfaction = Q13,
         count:data_type)

# bind
heat_map_summary_data <- bind_rows(q10_q11_summary_data_fix, q12_q13_summary_data_fix) %>%
  mutate(data_type = fct_relevel(data_type, "weather", "climate"))

# plot heatmap of percents
# figure 4
png(here::here("figures", "fig_4.png"), height = 8, width = 10, units = "in", res = 300)
ggplot(data = heat_map_summary_data) +
  geom_tile(mapping = aes(x = satisfaction, y = frequency, fill = perc), color = "black", lwd = 0.5) +
  labs(x = "Satisfaction", y = "Frequency", title = "", fill = "Percent (%)") +
  scale_x_discrete(limits = rev) +
  scale_y_discrete(limits = rev) +
  scale_fill_gradient2(low = "white",
                       high = "grey20") +
  facet_wrap(~data_type) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 16))
dev.off()


# ---- question 14 - where to data least meet your needs ----
q14_data <- selected_data %>%
  select(ResponseId, Q14) %>%
  na.omit()

# export for qualitative coding
write_csv(x = q14_data, file = here::here("data", "q14_data.csv"))


# ---- question 15 - how do you currently get weather and climate info ----
q15_data <- selected_data %>%
  select(ResponseId, Q15, Q15_12_TEXT) %>%
  mutate(Q15_unique = stringr::str_replace_all(Q15, pattern = "[\r\n]" , replacement = "")) %>%
  mutate(Q15_unique2 = stringr::str_replace_all(Q15_unique, pattern = "Office " , replacement = "Office")) %>%
  mutate(Q15_unique_final = stringr::str_replace_all(Q15_unique2, pattern = "provider,", replacement = "provider")) %>% # remove commas
  separate_rows(Q15_unique_final, sep = ",") %>%
  mutate(Q15_short = case_when(Q15_unique_final == "North Carolina State Climate Office" ~ "NCSCO",
                               Q15_unique_final == "National Weather Service or other Federal Government" ~ "NWS/Federal Govt.",
                               Q15_unique_final == "Local TV Meteorologists" ~ "Local TV Meteorologists",
                               Q15_unique_final == "Phone apps" ~ "Phone Apps",
                               Q15_unique_final == "Cooperative Extension" ~ "Extension",
                               Q15_unique_final == "Private weather provider such as Weather.com or Accuweather" ~ "Private Weather Provider",
                               Q15_unique_final == "Colleagues or others in your profession" ~ "Colleagues",
                               Q15_unique_final == "Advisors/Consultants" ~ "Advisors/Consultants",
                               Q15_unique_final == "Farmer’s Almanac" ~ "Farmer’s Almanac",
                               Q15_unique_final == "Family Members or Friends" ~ "Family/Friends",
                               Q15_unique_final == "Social media" ~ "Social Media",
                               Q15_unique_final == "Other (please describe)" ~ "Other")) %>%
  select(ResponseId, Q15_short, Q15_12_TEXT)

# summarize
q15_summary_data <- q15_data %>%
  group_by(Q15_short) %>%
  summarize(count = n()) %>%
  mutate(count_text = if_else(count < 5, "n < 5", paste0("n = ", count)),
         perc = round(count / num_responses * 100, 0),
         perc_text = paste0(perc, " %"))

# plot count
# ggplot(data = q15_summary_data) +
#   geom_col(mapping = aes(x = Q15_short, y = count)) +
#   geom_text(mapping = aes(x = Q15_short, y = count + 5, label = count_text)) +
#   labs(x = "", y = "Count", title = "Current Sources for Weather and Climate Info") +
#   ylim(0, 125) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         text = element_text(size = 14))

# plot percent
ggplot(data = q15_summary_data) +
  geom_col(mapping = aes(x = Q15_short, y = perc)) +
  geom_text(mapping = aes(x = Q15_short, y = perc + 2, label = perc_text)) +
  labs(x = "", y = "Percent (%)", title = "Percentage of Respondents vs Current Sources for Weather and Climate Info") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 14))

# other data
q15_data_other <- q15_data %>%
  select(ResponseId, Q15_short, Q15_12_TEXT) %>%
  filter(Q15_short == "Other")


# ---- question 16 - how do you use the Farmer's Almanac ----
q16_data <- selected_data %>%
  select(ResponseId, Q16) %>%
  na.omit()
# this one might be one that we code by hand (only 27 responses)

# export for qualitative coding
write_csv(x = q16_data, file = here::here("data", "q16_data.csv"))
