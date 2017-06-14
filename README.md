# wifianalyser
This project will give you the oportunity to look the correlations between each pair of Access Points (AP) and analyse whether the resources of an AP are beeing drained or not. There will also be additional information to get a more detailed conclusions like the throughput, retransmissions, packet rates, and relations between APs.

To run the code it is recommended to have Rstudio, since it organizes the data better than the default IDE. The code will show us 
different tables as well as some plots. 

For analysing the wireless traffic you will first need to make a capture with Wireshark in monitor mode and then export the file as csv.
Then the code will import the file and calculate the corresponding metrics. Before exporting the file you will first need to specify what
information you want from the traces by applying them into columns. This section will be explain in section Column Parameters.


# Column Parameters
