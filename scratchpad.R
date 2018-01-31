#per pupil expenditure grouped by county
districts_redux <- districts %>% 
  dplyr::select(system, system_name, Enrollment, Per_Pupil_Expenditures, ACT_Composite, County.Name, grade, grade_enrollment) %>% 
  dplyr::mutate(dist_expenditure = Enrollment*Per_Pupil_Expenditures)

names(districts_redux) <- c("dist_no", "dist", "dist_enrollment", "dollars_per_pupil", "ACT_composite", "county_name", "grade", "dollars_per_grade", "dollars_per_dist")

# 1.0 Column plot of agi grouped by zipcode
col_plot <- irs13 %>% 
  dplyr::filter(zipcode != "00000") %>% 
  dplyr::filter(!is.na(adjusted_gross_income)) %>% 
  dplyr::group_by(zipcode) %>% 
  dplyr::summarise(agi = sum(adjusted_gross_income, na.rm = T))

col_plot <- col_plot[order(col_plot$agi), ]

ggplot(col_plot, aes(zipcode, agi)) + 
  geom_col() +
  labs(title = "Adjusted Gross Income of Each Zipcode in TN", 
       y = "AGI", 
    caption = "we can see the variation across zipcodes can be quite large") +
  theme(axis.text.x = element_text(size = 0.6, angle = 45))

ggplot(col_plot, aes(zipcode, agi)) + 
  geom_col() +
  labs(title = "Y-axis on a logarithmic scale") +
  theme(axis.text.x = element_text(size = 0.6, angle = 45)) +
  scale_y_log10()

# zip code dot plot of 2014 irs population for TN
ggplot(zip_code, aes(x = longitude, y = latitude, alpha = .125, size = irs_estimated_population_2014)) +
  geom_point() +
  coord_fixed(ratio = 1/1)

# Create the getmode function. (because R is dumb)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# create a plotable df that has zip, lat, lon, pop, and agi
irs13_zip_plotable <- merge(zip_code, irs13, by.y = "zipcode", by.x = "zip", all.y = T, all.x = T) %>% 
  dplyr::select("zip", "latitude", "longitude", "irs_estimated_population_2014", "adjusted_gross_income") %>% 
  dplyr::filter(zip != "00000", irs_estimated_population_2014 != 0) %>% 
  dplyr::group_by(zip) %>% 
  dplyr::summarise(agi = sum(adjusted_gross_income, na.rm = T),
                   latitude = getmode(latitude), longitude = getmode(longitude),
                   pop = getmode(irs_estimated_population_2014))

#plot it
ggplot(irs13_zip_plotable, aes(x = longitude, y = latitude, alpha = agi/pop, size = pop, color = "agi/pop")) +
  geom_point(color = "dark green") +
  coord_fixed(ratio = 1/1) +
  labs(title = "Income Distribution Grouped by Zipcode")
