library(tidyverse)
library(dplyr)
library(ggrepel)
leaddata <- readxl::read_xlsx("Netsuite_Lead-MQL_Audience.xlsx", "ALL" )

grouped <- leaddata %>% group_by(Month, Audience) %>% summarise(sum(Leads), sum(MQL), Lead_MQL = sum(MQL)/sum(Leads))
grouped <- grouped %>% filter(grepl("Fin|Mgmt|SFTInt|SMB|TMG", Audience))

plot <- ggplot(grouped) +
  aes(x = Month, y = Lead_MQL, color = Audience) +
  geom_line() + theme_minimal() + geom_text_repel(label = round(grouped$Lead_MQL, 2), color = "black")

plot

library(plotly)

grouped <- grouped %>% rename(Date = Month)
grouped$Month <- month(grouped$Date)
grouped$Month <- case_when(
  grouped$Month == 1 ~ "January",
  grouped$Month == 12 ~ "December",
  grouped$Month == 11 ~ "November",
  grouped$Month == 10 ~ "October",
  grouped$Month == 9 ~ "September",
  grouped$Month == 8 ~ "August",
  grouped$Month == 7 ~ "July",
  TRUE ~ "June"
)

grouped$Lead_MQL <- round(grouped$Lead_MQL, 3)

grouped <- grouped %>% ungroup()
plotly1 <- plot_ly(data = grouped, x = ~Date, y = ~Lead_MQL, color = ~Audience, 
                   type = "scatter", mode = "lines+text", text = ~round(Lead_MQL, 2),
                   textposition = "top",
                   textfont = list(color = "black"),
                   hovertemplate = ~paste("Month: ", Month, 
                                          "\n", "Audience: ", Audience,
                                          "\n", "Lead-MQL%: ", Lead_MQL)) %>%
  layout(title = "Sponsored Content Campaign Performance By Audience",
         xaxis = list(title = ""),
         yaxis = list(title = "Lead-MQL%", showticklabels = FALSE),
         showlegend = TRUE) 


plotly1

output_file2 <- "/Users/danielbeim/Downloads/SC_Audience_Lead-MQL_Jan24/SC_Lead-MQL_Peformance_Plot.html"

# Save the Plotly plot as an HTML file
saveWidget(plot2, file = output_file2, selfcontained = TRUE)



