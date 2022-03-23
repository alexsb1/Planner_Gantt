# PlannerAsGantt

## Visualise Microsoft Planner as a Gantt chart
### By Alex Searle-Barnes

This is an interactive webpage displaying the tasks made in Microsoft Planner as a Gantt chart.

View the interactive timeline here. [https://alexsb.shinyapps.io/PlannerAsGantt/](https://alexsb.shinyapps.io/PlannerAsGantt/)


The code is written in R and Shiny. 

## Notes
* The date range pickers control the filtering of tasks that start and end within the selected date range. Because of how Tidyverse works, tasks that either start or end outside of the selected dates are not shown at all. A future update will show a partial geom_segment on the Gantt chart to include all ongoing tasks during the selected date range.