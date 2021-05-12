
# chart type for quick stats panel

 chartXAxisStats <- function(btnNr) {
  
  switch (btnNr,
          '1' = "statusHrsMin",
          '2' = "driverName"
          #'2' = "Kierowca"
  )
  
}

 chartTypeStats <- function(btnNr) {
  
  switch (btnNr,
          '1' = echarts4r::e_area,
          '2' = echarts4r::e_bar#,
           #echarts4r::e_line  #default
  )
  
 }
 
 chartTitleStats <- function(btnNr) {
   
   switch (btnNr,
           '1' = "Number of Orders (every 30 min.)",
           '2' = "Number of Orders (per driver)"
   )
   
 }
 
 chartColorStats <- function(btnNr) {
   
   switch (btnNr,
           '1' = "#0089d9",
           '2' = "#a730ba"
   )
   
 }
 
 
 chartLabelShowStats <- function(btnNr) {
   
   switch (btnNr,
           '1' = TRUE,
           '2' = FALSE
   )
   
 }
 
 
 
 
 
 
 
 
 
 