library(shiny)

options(shiny.host = "192.168.0.113")
options(shiny.port = 6513) 

# Adjust the path to match where the server file is.
#runApp('Jupyter Notebooks/Mobile Culler/Swiper')

# Running the runner as a standalone script from the command prompt
# Standalone requires relative path
  # Also requires user access control toggling in Windows, which is not ideal
runApp('Swiper')
