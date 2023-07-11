 

    
# Using PEA in network mode

PEA is designed to estimate the GNSS error parameters that cannot be precisely determined by a single receiver. The PEA will estimate the following parameters:

* Correction to satellite initial conditions estimated by the POD component
* Satellite clock offset and drift
* Satellite hardware bias for two signal carriers
* Satellite differential bias for two signal pseudoranges 
* Ionospheric propagation delay
* Tropospheric propagation delay
* Receiver/station position and velocity
* Receiver/station clock offset and drift
* Receiver/station hardware bias for signal carriers
* Receiver/station differential bias for two signal pseudoranges 
* Relative carrier phase ambiguities

In order to estimate the full range of parameters, the PEA will need to ingest GNSS observation data from a Global network of sufficient density. 
