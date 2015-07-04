library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cranlogs)
library(zoo)
library(scales)

get_initial_release_date = function(packages, date_from)
{
    min_date = Sys.Date() - 1
    
    for (pkg in packages)
    {
        # api data for package. we want the initial release - the first element of the "timeline"
        pkg_data = httr::GET(paste0("http://crandb.r-pkg.org/", pkg, "/all"))
        pkg_data = httr::content(pkg_data)
        
        initial_release = pkg_data$timeline[[1]]
        min_date = min(min_date, max(as.Date(initial_release), date_from))    
    }
    
    min_date
}

shinyServer(function(input, output) {
  downloads <- reactive({
      packages <- input$package
      date_from <- as.Date(input$date_from)
      cran_downloads0 <- failwith(NULL, cran_downloads, quiet = TRUE)
      cran_downloads0(package = packages, 
                      from    = get_initial_release_date(packages, date_from), 
                      to      = Sys.Date()-1)
  })
    
  output$downloadsPlot <- renderPlot({
      d <- downloads()
      if (input$transformation=="weekly") {
          d$count=rollapply(d$count, 7, sum, fill=NA)
      } else if (input$transformation=="monthly") {
          d <- d %>%
              mutate(year=year(date), month=month(date)) %>% 
              group_by(package, year, month) %>%
              summarise(count=sum(count)) %>%
              mutate(date1=as.Date(paste(year, month, "1", sep="-")), date2=as.Date(paste(year, month, days_in_month(month), sep="-"))) 
      } else if (input$transformation=="cumulative") {
          d <- d %>%
              group_by(package) %>%
              transmute(count=cumsum(count), date=date)
      }
      
      if (input$transformation=="monthly") {
          ggplot(d, aes(x=date1, xend=date2, y=count, yend=count, colour = package)) + geom_segment() +
              xlab("Date") +
              scale_y_continuous(name="Number of downloads", labels = comma)
      } else {
          ggplot(d, aes(date, count, color = package)) + geom_line() +
              xlab("Date") +
              scale_y_continuous(name="Number of downloads", labels = comma)
      }
  })

})
