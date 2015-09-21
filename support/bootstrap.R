# script to install dependencies
# **run this script outside of StatET's R cmd line**
# 
# Author: vitid
###############################################################################

#install RJ for working with StatET plugin
install.packages(c("rj", "rj.gd"), repos="http://download.walware.de/rj-2.0");

install.packages("RMySQL");
install.packages("dplyr");
install.packages("quantmod");
install.packages("tseries");
install.packages("forecast");
