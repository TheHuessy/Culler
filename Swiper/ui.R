## THIS: https://livefreeordichotomize.com/2017/03/12/introducing-shinyswipr-swipe-your-way-to-a-great-shiny-ui/

## HOLY SHIT, ALSO THIS: https://github.com/RinteRface/cheatsheets/blob/master/shinyMobile/shinyMobile.pdf

library(shiny)
library(shinysense)
library(magick)


shinyUI(
  fixedPage(
    includeCSS("style.css"),
    uiOutput("ticker"),
    uiOutput("stats"),
    shinyswipr_UI("swiper_object",
                  imageOutput("image_output")
                  )
    )
  )


