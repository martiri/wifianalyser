# wifianalyser
This project will give you the oportunity to look the correlations between each pair of Access Points (AP) and analyse whether the resources of an AP are beeing drained or not. There will also be additional information to get a more detailed conclusions like the throughput, retransmissions, packet rates, and relations between APs.

To run the code it is recommended to have Rstudio, since it organizes the data better than the default IDE. The code will show us 
different tables as well as some plots. 

For analysing the wireless traffic you will first need to make a capture with Wireshark in monitor mode and then export the file as csv.
Then the code will import the file and calculate the corresponding metrics. Before exporting the file you will first need to specify what
information you want from the traces by applying them into columns. This section will be explain in section Column Parameters.


# Column Parameters

Before starting this code you must make sure you have extracted the capture correctly with the following columns and with exctly the same name:

No.                  -------------> The number of packet
 
Time                 -------------> The moment in time the packet was captured

Transmitter address  -------------> The transmitter address (MAC)

Source               -------------> The source address (MAC)

Destination          -------------> The destination address (MAC)

BSS Id               -------------> The BSS ID of each packet. i.e. the AP addres (MAC)

SSID                 -------------> The name with we can recognize the network

Channel              -------------> The channel the packet is travelling trhough

Signal strength .dBm.-------------> The RSSI in (dBm)

Type Subtype         -------------> The subtype of packet (Beacon frame, Acknowledgement, etc.)

Subtype              -------------> The subtype code recognized by Wireshark

Length               -------------> The length of each packet

Type                 -------------> The type of packet (Management, Data or Control frame)

DS status            -------------> This is the status it tells you if the packet is being send to an ap or from an ap

Current Channel      -------------> The channel the packet is suposed to be travelling through

Receiver address     -------------> 

Duration             -------------> The duration of the packet in be transmitted (micro seconds)

Retry                -------------> This tels you if a packet has been retransmitted or not

