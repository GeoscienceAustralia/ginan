 

# Using PEA in user mode

When set to end user mode, the PEA component of Ginan will process each station separately. This mode will allow the estimation of parameters available to users with single receivers. 

* Receiver position
* Receiver clock offset
* Tropospheric delay at receiver location
* Ionospheric delay at the receiver location (not yet available)
* Carrier phase ambiguities

The results of PEA run in end user mode are printed in the trace files.
Station trace file outputs can be activated by setting the `outputs: trace: output_stations: true`
The most commonly used outputs from the PEA used in end-user mode are expected to be: the receiver position, receiver velocity, receiver clocks and tropospheric delays.

### Receiver position 

Receiver position results are preceded by the `$POS` label and thus, in Linux, can be extracted using the command:

    grep "$POS" <path_to_trace_file>

the output line for the for receiver position will have comma separated fields with the following format:

    $POS, 2166, 278015.000, -4052053.0060, 4212836.8682, -2545105.0796, 0.0245227, 0.0231919, 0.0163678

the fields represent, from left to right:

 * `$POS` label
 * GPS week
 * GPS TOW in seconds
 * Receiver ECEF X position in meters
 * Receiver ECEF Y position in meters
 * Receiver ECEF Z position in meters
 * Standard deviation of ECEF X positions in meters
 * Standard deviation of ECEF X positions in meters
 * Standard deviation of ECEF X positions in meters


### Receiver clock

Receiver clock offset results are preceded by the `$CLK` label and thus, in Linux, can be extracted using the command:

    grep "$CLK" <path_to_trace_file>

the output line for the for receiver position will have comma separated fields with the following format:

    $CLK, 2166, 278015.000, 3.1902, 0.0000, 1.1924, 0.0000, 0.0860, 0.0000, 0.0953, 0.0000

the fields represent, from left to right:

1. `$CLK` label
1. GPS week
1. GPS TOW in seconds
1. Receiver clock offset for with respect to GPS clock, in nanoseconds
1. Receiver clock offset for with respect to GLONASS clock, in nanoseconds
1. Receiver clock offset for with respect to Galileo clock, in nanoseconds
1. Receiver clock offset for with respect to Beidou clock, in nanoseconds   
1. Standard deviation of clock offset wrt. GPS, in nanoseconds
1. Standard deviation of clock offset wrt. GLONASS, in nanoseconds
1. Standard deviation of clock offset wrt. Galileo, in nanoseconds
1. Standard deviation of clock offset wrt. Beidou, in nanoseconds

If clock offsets for a particular constellation are not available both the offset and its variance will be set to 0.

### Tropospheric delays 

Tropospheric delays at the receiver position are preceded by the `$TROP` label and thus, in Linux, can be extracted using the command:

    grep "$TROP" <path_to_trace_file>

the tropospheric delay solutions will be represented to either a single line, with the `$TROP` or three lines, as follows:

```
$TROP, 2166, 278015.000, 14 ,2.294950, 0.0030977
$TROP_N, 2166, 278015.000, 14, -0.174797, 0.0181385
$TROP_E, 2166, 278015.000, 14, -0.223868, 0.0250276
```

each of the troposphere output line will contain comma separated fields, of which the first are:

* Label, `$TROP`, `$TROP_N` or `$TROP_E`
* GPS week
* GPS TOW in seconds
* Number of satellites used in the solution

The line starting with `$TROP` contain the Zenith Tropospheric Delay (ZTD) and its standards deviation, both in meters, as their last two fields.  The line starting with `$TROP_N` contains the tropospheric delay gradient in north-south direction, and  the line starting with `$TROP_E` contains the tropospheric delay gradient in east-west direction.
