# Examples to run the PEA from a python script
The following examples will show how to use the python scripts in the `source` directory to automatically gather files and run the pea to produce results (as .TRACE files and plots). 


### Run the PEA
The easiest way to run the is to choose a reference station, a start time, end time and directories to store the gathered files and resultant outputs.  
Doing this for station _HOB200AUS_ and processing 2 days of data, starting 20 Dec 2020 00:00 and ending 21 Dec 2020 23:59:30, we can run the following code (assuming the `pwd` is your pea directory):

`
python3 python/source/run_pea_PPP.py HOB200AUS 2020-12-20_00:00:00 2020-12-21_23:59:30 <directory_to_download_files_to> <results_output_directory> -md -del_yaml
`

This will produce a .TRACE file that contains XYZ position information for that station and estaimes for the Zenith Total Delay (ZTD).
The flags are as follows:  
- `-md` is used to allow dates to be input in _YYYY-MM-DD_ format as above. If the flag is not selected, the format must be _YYYY-DOY_ (where _DOY_ is day-of-year).  
- `-del_yaml` is used to delete the `.yaml` config file after the pea run. Without this flag, the `.yaml` file will be kept (in `pwd`)


### Near-Real-Time (NRT) Daily Run
In this example, we will see how to run the pea to produce ZTD and station coordinate results for reference station _HOB200AUS_, using the most rececnt data available on GA and CDDIS servers.  
Assuming the pwd is your `pea` directory, then the following code should run to produce a .TRACE file and plots of ZTD and XYZ station coordinates for the most recent day:

`
python3 python/source/run_pea_PPP.py HOB200AUS _ _ <directory_to_download_files_to> <results_output_directory> -rapid -m_r -plt_m_r
`

This differs to the code above in that the flags `-rapid`, `-m_r` and `plt_m_r` have been selected.  
- `-rapid` will find the rapid versions of files - these are less accurate than final versions but are available sooner at CDDIS  
- `-m_r` will get the code to search the CDDIS database and find the most recent rapid files available for `clk` and `sp3` files. The date and time is then selected based on this, allowing us to \_ \_ for the start and end times  
- `-plt_m_r` will plot the ZTD and XYZ results as `.png` files and place in a `plots` directory 


# Latency Tool

To run currently do something like... 

    nohup python source/main/python/npi/latency.py 2>&1 & 
  
A simple configuration file `config/config.yaml` looks like...

    streams:
    - type: NTRIP
      protocol: http
      host: auscors.ga.gov.au
      port: 2101
      username: a_username
      password: a_password
      count: 10
      stations:
      - {format: RTCM3, id: COCO7}
     
and again with more streams looks something like...

    streams:
    - type: NTRIP
      protocol: http
      host: auscors.ga.gov.au
      port: 2101
      username: a_username
      password: a_password
    #  count: 10
      stations:
      - {id: TID17}
      - {id: 00NA7}
      - {id: 01NA7}
      - {id: ABMF7}
      - {id: ALBY7}
      - {id: ALGO7}
      - {id: ALIC7}
      - {id: ANDA7}
      - {id: AREG7}
      - {id: ARUB7}
      - {id: ASCG7}
      - {id: AUT07}
      - {id: BBOO7}
      - {id: BDLE7}
      - {id: BEEC7}
      - {id: BNDY7}
      - {id: BRLA7}
      - {id: BRST7}
      - {id: BRUX7}
      - {id: BUR27}
      - {id: CEDU7}
      - {id: CHOF7}
      - {id: CHPG7}
      - {id: CLYT7}
      - {id: COBG7}
      - {id: COCO7}
      - {id: COEN7}
      - {id: COOB7}
      - {id: COOL7}
      - {id: CRAN7}
      - {id: CUT07}
      - {id: DARW7}
      - {id: DAV17}
      - {id: DLF17}
      - {id: DODA7}
      - {id: DRAO7}
      - {id: DYNG7}
      - {id: EUR27}
      - {id: FROY7}
      - {id: GAMB7}
      - {id: GRAC7}
      - {id: HARB7}
      - {id: HIL17}
      - {id: HNIS7}
      - {id: HOB27}
      - {id: HRAG7}
      - {id: JAB27}
      - {id: JFNG7}
      - {id: JOG27}
      - {id: KAT17}
      - {id: KIR87}
 
# THE REST IS A BRAIN DUMP TO BE CLEANED UP RESTRUCTURED!!!




# Messages

## COCO7

From 10,000 messages...
- 1006
- 1008
- 1013
- 1019
- 1020
- 1033
- 1044
- 1077
- 1087
- 1097
- 1117
- 1127

From 1,000 messages...
- 1006	Station co-ordinates
- 1008	Antenna Description
- 1013	Auxiliary Operation Information - System Parameters
- 1019	Auxiliary Operation Information - Satellite Ephemeris Data
- 1020	Auxiliary Operation Information - Satellite Ephemeris Data
- 1033	Receiver and Antenna Description
- 1044	Auxiliary Operation Information - Satellite Ephemeris Data
- 1077	Observations - GPS MSMs	(MSM7)
- 1087	Observations - GLONASS MSMs (MSM7)
- 1097	Observations - Galileo MSMs (MSM7)
- 1117	Observations - QZSS MSMs (MSM7)
- 1127	Observations - BDS MSMs (MSM7)

2018-07-20 12:05:51,887 INFO Connected - starting to read messages...
2018-07-20 12:05:51,894 INFO frame length=[None] message number=[None]
2018-07-20 12:05:51,895 INFO frame length=[56] message number=[1117]
2018-07-20 12:05:51,896 INFO frame length=[294] message number=[1127]
2018-07-20 12:05:51,898 INFO frame length=[422] message number=[1077]
2018-07-20 12:05:51,899 INFO frame length=[261] message number=[1087]
2018-07-20 12:05:51,899 INFO frame length=[22] message number=[1097]
2018-07-20 12:05:51,900 INFO frame length=[56] message number=[1117]
2018-07-20 12:05:51,901 INFO frame length=[294] message number=[1127]
2018-07-20 12:05:51,902 INFO frame length=[422] message number=[1077]
2018-07-20 12:05:51,903 INFO frame length=[261] message number=[1087]
2018-07-20 12:05:51,904 INFO frame length=[22] message number=[1097]
2018-07-20 12:05:51,904 INFO frame length=[56] message number=[1117]
2018-07-20 12:05:51,906 INFO frame length=[294] message number=[1127]
2018-07-20 12:05:51,906 INFO frame length=[56] message number=[1033]
2018-07-20 12:05:51,908 INFO frame length=[422] message number=[1077]
2018-07-20 12:05:51,909 INFO frame length=[261] message number=[1087]
...
2018-07-20 12:08:27,591 INFO frame length=[22] message number=[1097]
2018-07-20 12:08:27,591 INFO frame length=[56] message number=[1117]
2018-07-20 12:08:27,593 INFO frame length=[294] message number=[1127]
2018-07-20 12:08:28,718 INFO frame length=[21] message number=[1006]
2018-07-20 12:08:28,718 INFO frame length=[29] message number=[1008]
2018-07-20 12:08:28,720 INFO frame length=[422] message number=[1077]
2018-07-20 12:08:28,720 INFO frame length=[256] message number=[1087]
2018-07-20 12:08:28,720 INFO frame length=[22] message number=[1097]
2018-07-20 12:08:28,721 INFO frame length=[56] message number=[1117]
2018-07-20 12:08:28,721 INFO frame length=[294] message number=[1127]
2018-07-20 12:08:29,566 INFO frame length=[422] message number=[1077]
2018-07-20 12:08:29,567 INFO count=[1000] skipped=[1]

9 secs -> 06:00 then 2:29 to 08:29 so 2:38 secs to get 1,000 messages = 158 secs to get 1,000 messages = 0.158 secs / message or 6.3 messages / sec

- 1033 Received and Antenna Description
- 1077 GPS MSMs
- 1087 GLONASS MSMs
- 1097 Galileo MSMs
- 1117 QZSS MSMs
- 1127 BDS MSMs


## MSM (1,2,3,4,5,6,7)

MSM message contains
- message header
- satellite data
- signal data

The message header contains
- message number 		DF002	uint12	12-bits
- reference station id		DF003	uint12	12-bits	
- GNSS epoch time		???	uint30	30-bits
- multiple message bit		DF393	bit(1)	1-bit

GNSS Epoch Time
- GPS is
--TOW DF004						DF004	GPS Epoch Time (TOW)	0-604,799,999ms	1ms	uint30	milliseconds from beginning of GPS week (midnight GMT on Saturday night/Sunday morning measured in GPS time)
- GLONASS is
-- GLONASS DAY OF WEEK	DF416 int3 3-bits		DF416	GLONASS Day of Week	0-7		1	uint3	0=sunday, 1=monday, ..., 6=saturday, 7=not known
-- GLONASS EPOCH TIME DF034 uint27 27-bits		DF034	GLONASS Epoch Time	0-86,400,999ms	1ms	uint27	
- Galileo is
-- TOW DF248						DF248	GALILEO Epoch Time	0-604,799,999ms	1ms	uint30
- QZSS is
-- TOW DF428						DF428	QZSS Epoch Time		0-604,799,999ms	1ms	uint30	
- BeiDou is
-- TOW DF+002						DF427	BeiDou Epoch Time	0-604,799,999ms	1ms	uint30

# RTCM v3

The following is a Hex-ASCII example of a message type 1005 (Stationary Antenna Reference Point, No Height Information).

D3 00 13 3E D7 D3 02 02 98 0E DE EF 34 B4 BD 62 AC 09 41 98 6F 33 36 0B 98

The parameters for this message are:
 Reference Station Id = 2003
 GPS Service supported, but not GLONASS or Galileo
 ARP ECEF-X = 1114104.5999 meters
 ARP ECEF-Y = -4850729.7108 meters
 ARP ECEF-Z = 3975521.4643 meters




# ACS Metrics

## RTCM v3

An `RTCM v3` stream of data consists of a set of `RTCM v3` packets.

An `RTCM v3` packet consists of:

* 8-bit preamble
* 16-bits
** 6-bit reserved (should be `0`s)
** 10-bit length
* variable length message
** 12-bit message type
* 24-bit message CRC

|   x  |  x   |  x   |
| --- | --- | --- |
| Preamble | 8-bits |
| 
* 8-bit pre-amble character
* 
The start or an `RTCM v3` packet is marked by the `0xD3` pre-amble character

Following this is 






## Reading RTCM3 Data...

- RTCM3 packet starts with 0xd3 byte so first skip to start of packet

See [https://pdfs.semanticscholar.org/bf93/21e569cc0f009982af3158a6489accc8f3e5.pdf]

RTCM v3 frame looks like:
  - preamble 8-bits (0xD3)
  - reserved 6-bits
  - message length 10-bits
  - message variable length
  - CRC 24-bits
  
So to parse it we do
1.  find the start of packet byte - i.e. the preamble byte - i.e. 0xd3  
2.  read the next 2 bytes (aka 16-bits) and extract the message length from the right-hand 10-bits




# RTCM v3

d3 74 63

                   09   8765 4321
1101 0011 | 0111 0100 | 0110 0011 |

so the length is 63 (hex which is 99 decimal)



d3 74 63 18 d1 94 65 0d 43 4c 53 14 b1 2c 4f


1101 0011 | 0111 0100 | 0110 0011 |
1101 0011 | 0111 0100 | 0110 0011


00011000 00011000


byte 0 = preamble = 0xD3

bytes 1 & 2 = reserved (6-bits) + message length (10-bits)

    byte1 & 0x03 << 8 | byte2
    
    byte1 = 00011000
    byte2 = 00011000
    
    00011000 & 0x03 = 00000000
    
bytes 3...n-1 = variable length message

bytes n...n+2 = 24-bit CRC (CRC24) 

## RTCM v3 Message

reserved : 1-bit
message number : 12-bits uint12

18 d1 is...

 123456789012
0001100011010001 



# RTCM v3

## GPS RTK Observable Messages

message header (aka number aka type):
- 1001
- 1002
- 1003
- 1004

                                                                                                        
|DATA FIELD|DF NUMBER|DATA TYPE|NO. OF BITS|
|----------|---------|---------|-----------|
|Message Number (e.g.,“1001”= 0011 1110 1001)|DF002|uint12|12|
|Reference Station ID|DF003|uint12|12|
|GPS Epoch Time (TOW)|DF004|uint30|30|
|Synchronous GNSS Flag|DF005|bit(1)|1|
|No. of GPS Satellite Signals Processed|DF006|uint5|5|
|GPS Divergence-free Smoothing Indicator|DF007|bit(1)|1|
|GPS Smoothing Interval|DF008|bit(3)|3|
|TOTAL| | |64|


## Station Antenna Reference Point Messages

- 1005
- 1006

### 1005

|DATA FIELD|DF NUMBER|DATA TYPE|NO. OF BITS|
|----------|---------|---------|-----------|
|Message Number (e.g. “1005”= 0011 1110 1101)|DF002|uint12|12|
|Reference Station ID|DF003|uint12|12|
|...| | |
|TOTAL| | |152|

### 1006

|DATA FIELD|DF NUMBER|DATA TYPE|NO. OF BITS|
|----------|---------|---------|-----------|
|Message Number (e.g. “1006”= 0011 1110 1110)|DF002|uint12|12|
|Reference Station ID|DF003|uint12|12|
|...| | |
|TOTAL| | |168|

## Antenna Description Messages

- 1007
- 1008

### 1007

|DATA FIELD|DF NUMBER|DATA TYPE|NO. OF BITS|
|----------|---------|---------|-----------|
|Message Number (e.g. “1007”= 0011 1110 1111)|DF002|uint12|12|
|Reference Station ID|DF003|uint12|12|
|...| | |
|TOTAL| | |40+8*N|







The Python RTCM3 library knows about the following messages:
- 1001
- 1002
- 1003
- 1004
- 1005
- 1006
- 1008
- 1009
- 1010
- 1011
- 1012
- 1033



# CRC24 STUFF FROM RTKLIB...

static const unsigned int tbl_CRC24Q[]={
    0x000000,0x864CFB,0x8AD50D,0x0C99F6,0x93E6E1,0x15AA1A,0x1933EC,0x9F7F17,
    0xA18139,0x27CDC2,0x2B5434,0xAD18CF,0x3267D8,0xB42B23,0xB8B2D5,0x3EFE2E,
    0xC54E89,0x430272,0x4F9B84,0xC9D77F,0x56A868,0xD0E493,0xDC7D65,0x5A319E,
    0x64CFB0,0xE2834B,0xEE1ABD,0x685646,0xF72951,0x7165AA,0x7DFC5C,0xFBB0A7,
    0x0CD1E9,0x8A9D12,0x8604E4,0x00481F,0x9F3708,0x197BF3,0x15E205,0x93AEFE,
    0xAD50D0,0x2B1C2B,0x2785DD,0xA1C926,0x3EB631,0xB8FACA,0xB4633C,0x322FC7,
    0xC99F60,0x4FD39B,0x434A6D,0xC50696,0x5A7981,0xDC357A,0xD0AC8C,0x56E077,
    0x681E59,0xEE52A2,0xE2CB54,0x6487AF,0xFBF8B8,0x7DB443,0x712DB5,0xF7614E,
    0x19A3D2,0x9FEF29,0x9376DF,0x153A24,0x8A4533,0x0C09C8,0x00903E,0x86DCC5,
    0xB822EB,0x3E6E10,0x32F7E6,0xB4BB1D,0x2BC40A,0xAD88F1,0xA11107,0x275DFC,
    0xDCED5B,0x5AA1A0,0x563856,0xD074AD,0x4F0BBA,0xC94741,0xC5DEB7,0x43924C,
    0x7D6C62,0xFB2099,0xF7B96F,0x71F594,0xEE8A83,0x68C678,0x645F8E,0xE21375,
    0x15723B,0x933EC0,0x9FA736,0x19EBCD,0x8694DA,0x00D821,0x0C41D7,0x8A0D2C,
    0xB4F302,0x32BFF9,0x3E260F,0xB86AF4,0x2715E3,0xA15918,0xADC0EE,0x2B8C15,
    0xD03CB2,0x567049,0x5AE9BF,0xDCA544,0x43DA53,0xC596A8,0xC90F5E,0x4F43A5,
    0x71BD8B,0xF7F170,0xFB6886,0x7D247D,0xE25B6A,0x641791,0x688E67,0xEEC29C,
    0x3347A4,0xB50B5F,0xB992A9,0x3FDE52,0xA0A145,0x26EDBE,0x2A7448,0xAC38B3,
    0x92C69D,0x148A66,0x181390,0x9E5F6B,0x01207C,0x876C87,0x8BF571,0x0DB98A,
    0xF6092D,0x7045D6,0x7CDC20,0xFA90DB,0x65EFCC,0xE3A337,0xEF3AC1,0x69763A,
    0x578814,0xD1C4EF,0xDD5D19,0x5B11E2,0xC46EF5,0x42220E,0x4EBBF8,0xC8F703,
    0x3F964D,0xB9DAB6,0xB54340,0x330FBB,0xAC70AC,0x2A3C57,0x26A5A1,0xA0E95A,
    0x9E1774,0x185B8F,0x14C279,0x928E82,0x0DF195,0x8BBD6E,0x872498,0x016863,
    0xFAD8C4,0x7C943F,0x700DC9,0xF64132,0x693E25,0xEF72DE,0xE3EB28,0x65A7D3,
    0x5B59FD,0xDD1506,0xD18CF0,0x57C00B,0xC8BF1C,0x4EF3E7,0x426A11,0xC426EA,
    0x2AE476,0xACA88D,0xA0317B,0x267D80,0xB90297,0x3F4E6C,0x33D79A,0xB59B61,
    0x8B654F,0x0D29B4,0x01B042,0x87FCB9,0x1883AE,0x9ECF55,0x9256A3,0x141A58,
    0xEFAAFF,0x69E604,0x657FF2,0xE33309,0x7C4C1E,0xFA00E5,0xF69913,0x70D5E8,
    0x4E2BC6,0xC8673D,0xC4FECB,0x42B230,0xDDCD27,0x5B81DC,0x57182A,0xD154D1,
    0x26359F,0xA07964,0xACE092,0x2AAC69,0xB5D37E,0x339F85,0x3F0673,0xB94A88,
    0x87B4A6,0x01F85D,0x0D61AB,0x8B2D50,0x145247,0x921EBC,0x9E874A,0x18CBB1,
    0xE37B16,0x6537ED,0x69AE1B,0xEFE2E0,0x709DF7,0xF6D10C,0xFA48FA,0x7C0401,
    0x42FA2F,0xC4B6D4,0xC82F22,0x4E63D9,0xD11CCE,0x575035,0x5BC9C3,0xDD8538
};

/* crc-24q parity --------------------------------------------------------------
* compute crc-24q parity for sbas, rtcm3
* args   : unsigned char *buff I data
*          int    len    I      data length (bytes)
* return : crc-24Q parity
* notes  : see reference [2] A.4.3.3 Parity
*-----------------------------------------------------------------------------*/
extern unsigned int crc24q(const unsigned char *buff, int len)
{
    unsigned int crc=0;
    int i;
    
    trace(4,"crc24q: len=%d\n",len);
    
    for (i=0;i<len;i++) crc=((crc<<8)&0xFFFFFF)^tbl_CRC24Q[(crc>>16)^buff[i]];
    return crc;
}

# TIME STUFF FROM RTKLIB...

const static double gpst0[]={1980,1, 6,0,0,0}; /* gps time reference */
const static double gst0 []={1999,8,22,0,0,0}; /* galileo system time reference */
const static double bdt0 []={2006,1, 1,0,0,0}; /* beidou time reference */

static double leaps[MAXLEAPS+1][7]={ /* leap seconds (y,m,d,h,m,s,utc-gpst) */
        {2017,1,1,0,0,0,-18},
        {2015,7,1,0,0,0,-17},
        {2012,7,1,0,0,0,-16},
        {2009,1,1,0,0,0,-15},
        {2006,1,1,0,0,0,-14},
        {1999,1,1,0,0,0,-13},
        {1997,7,1,0,0,0,-12},
        {1996,1,1,0,0,0,-11},
        {1994,7,1,0,0,0,-10},
        {1993,7,1,0,0,0, -9},
        {1992,7,1,0,0,0, -8},
        {1991,1,1,0,0,0, -7},
        {1990,1,1,0,0,0, -6},
        {1988,1,1,0,0,0, -5},
        {1985,7,1,0,0,0, -4},
        {1983,7,1,0,0,0, -3},
        {1982,7,1,0,0,0, -2},
        {1981,7,1,0,0,0, -1},
        {0}
};

/* string to time --------------------------------------------------------------
* convert substring in string to gtime_t struct
* args   : char   *s        I   string ("... yyyy mm dd hh mm ss ...")
*          int    i,n       I   substring position and width
*          gtime_t *t       O   gtime_t struct
* return : status (0:ok,0>:error)
*-----------------------------------------------------------------------------*/
extern int str2time(const char *s, int i, int n, gtime_t *t)
{
    double ep[6];
    char str[256],*p=str;
    
    if (i<0||(int)strlen(s)<i||(int)sizeof(str)-1<i) return -1;
    for (s+=i;*s&&--n>=0;) *p++=*s++; *p='\0';
    if (sscanf(str,"%lf %lf %lf %lf %lf %lf",ep,ep+1,ep+2,ep+3,ep+4,ep+5)<6)
        return -1;
    if (ep[0]<100.0) ep[0]+=ep[0]<80.0?2000.0:1900.0;
    *t=epoch2time(ep);
    return 0;
}

/* convert calendar day/time to time -------------------------------------------
* convert calendar day/time to gtime_t struct
* args   : double *ep       I   day/time {year,month,day,hour,min,sec}
* return : gtime_t struct
* notes  : proper in 1970-2037 or 1970-2099 (64bit time_t)
*-----------------------------------------------------------------------------*/
extern gtime_t epoch2time(const double *ep)
{
    const int doy[]={1,32,60,91,121,152,182,213,244,274,305,335};
    gtime_t time={0};
    int days,sec,year=(int)ep[0],mon=(int)ep[1],day=(int)ep[2];
    
    if (year<1970||2099<year||mon<1||12<mon) return time;
    
    /* leap year if year%4==0 in 1901-2099 */
    days=(year-1970)*365+(year-1969)/4+doy[mon-1]+day-2+(year%4==0&&mon>=3?1:0);
    sec=(int)floor(ep[5]);
    time.time=(time_t)days*86400+(int)ep[3]*3600+(int)ep[4]*60+sec;
    time.sec=ep[5]-sec;
    return time;
}

/* time to calendar day/time ---------------------------------------------------
* convert gtime_t struct to calendar day/time
* args   : gtime_t t        I   gtime_t struct
*          double *ep       O   day/time {year,month,day,hour,min,sec}
* return : none
* notes  : proper in 1970-2037 or 1970-2099 (64bit time_t)
*-----------------------------------------------------------------------------*/
extern void time2epoch(gtime_t t, double *ep)
{
    const int mday[]={ /* # of days in a month */
        31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,
        31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31
    };
    int days,sec,mon,day;
    
    /* leap year if year%4==0 in 1901-2099 */
    days=(int)(t.time/86400);
    sec=(int)(t.time-(time_t)days*86400);
    for (day=days%1461,mon=0;mon<48;mon++) {
        if (day>=mday[mon]) day-=mday[mon]; else break;
    }
    ep[0]=1970+days/1461*4+mon/12; ep[1]=mon%12+1; ep[2]=day+1;
    ep[3]=sec/3600; ep[4]=sec%3600/60; ep[5]=sec%60+t.sec;
}

/* gps time to time ------------------------------------------------------------
* convert week and tow in gps time to gtime_t struct
* args   : int    week      I   week number in gps time
*          double sec       I   time of week in gps time (s)
* return : gtime_t struct
*-----------------------------------------------------------------------------*/
extern gtime_t gpst2time(int week, double sec)
{
    gtime_t t=epoch2time(gpst0);
    
    if (sec<-1E9||1E9<sec) sec=0.0;
    t.time+=86400*7*week+(int)sec;
    t.sec=sec-(int)sec;
    return t;
}

/* time to gps time ------------------------------------------------------------
* convert gtime_t struct to week and tow in gps time
* args   : gtime_t t        I   gtime_t struct
*          int    *week     IO  week number in gps time (NULL: no output)
* return : time of week in gps time (s)
*-----------------------------------------------------------------------------*/
extern double time2gpst(gtime_t t, int *week)
{
    gtime_t t0=epoch2time(gpst0);
    time_t sec=t.time-t0.time;
    int w=(int)(sec/(86400*7));
    
    if (week) *week=w;
    return (double)(sec-w*86400*7)+t.sec;
}

/* galileo system time to time -------------------------------------------------
* convert week and tow in galileo system time (gst) to gtime_t struct
* args   : int    week      I   week number in gst
*          double sec       I   time of week in gst (s)
* return : gtime_t struct
*-----------------------------------------------------------------------------*/
extern gtime_t gst2time(int week, double sec)
{
    gtime_t t=epoch2time(gst0);
    
    if (sec<-1E9||1E9<sec) sec=0.0;
    t.time+=86400*7*week+(int)sec;
    t.sec=sec-(int)sec;
    return t;
}

/* time to galileo system time -------------------------------------------------
* convert gtime_t struct to week and tow in galileo system time (gst)
* args   : gtime_t t        I   gtime_t struct
*          int    *week     IO  week number in gst (NULL: no output)
* return : time of week in gst (s)
*-----------------------------------------------------------------------------*/
extern double time2gst(gtime_t t, int *week)
{
    gtime_t t0=epoch2time(gst0);
    time_t sec=t.time-t0.time;
    int w=(int)(sec/(86400*7));
    
    if (week) *week=w;
    return (double)(sec-w*86400*7)+t.sec;
}

/* beidou time (bdt) to time ---------------------------------------------------
* convert week and tow in beidou time (bdt) to gtime_t struct
* args   : int    week      I   week number in bdt
*          double sec       I   time of week in bdt (s)
* return : gtime_t struct
*-----------------------------------------------------------------------------*/
extern gtime_t bdt2time(int week, double sec)
{
    gtime_t t=epoch2time(bdt0);
    
    if (sec<-1E9||1E9<sec) sec=0.0;
    t.time+=86400*7*week+(int)sec;
    t.sec=sec-(int)sec;
    return t;
}

/* time to beidouo time (bdt) --------------------------------------------------
* convert gtime_t struct to week and tow in beidou time (bdt)
* args   : gtime_t t        I   gtime_t struct
*          int    *week     IO  week number in bdt (NULL: no output)
* return : time of week in bdt (s)
*-----------------------------------------------------------------------------*/
extern double time2bdt(gtime_t t, int *week)
{
    gtime_t t0=epoch2time(bdt0);
    time_t sec=t.time-t0.time;
    int w=(int)(sec/(86400*7));
    
    if (week) *week=w;
    return (double)(sec-w*86400*7)+t.sec;
}

/* add time --------------------------------------------------------------------
* add time to gtime_t struct
* args   : gtime_t t        I   gtime_t struct
*          double sec       I   time to add (s)
* return : gtime_t struct (t+sec)
*-----------------------------------------------------------------------------*/
extern gtime_t timeadd(gtime_t t, double sec)
{
    double tt;
    
    t.sec+=sec; tt=floor(t.sec); t.time+=(int)tt; t.sec-=tt;
    return t;
}

/* time difference -------------------------------------------------------------
* difference between gtime_t structs
* args   : gtime_t t1,t2    I   gtime_t structs
* return : time difference (t1-t2) (s)
*-----------------------------------------------------------------------------*/
extern double timediff(gtime_t t1, gtime_t t2)
{
    return difftime(t1.time,t2.time)+t1.sec-t2.sec;
}

/* get current time in utc -----------------------------------------------------
* get current time in utc
* args   : none
* return : current time in utc
*-----------------------------------------------------------------------------*/
static double timeoffset_=0.0;        /* time offset (s) */

extern gtime_t timeget(void)
{
    double ep[6]={0};
#ifdef WIN32
    SYSTEMTIME ts;
    
    GetSystemTime(&ts); /* utc */
    ep[0]=ts.wYear; ep[1]=ts.wMonth;  ep[2]=ts.wDay;
    ep[3]=ts.wHour; ep[4]=ts.wMinute; ep[5]=ts.wSecond+ts.wMilliseconds*1E-3;
#else
    struct timeval tv;
    struct tm *tt;
    
    if (!gettimeofday(&tv,NULL)&&(tt=gmtime(&tv.tv_sec))) {
        ep[0]=tt->tm_year+1900; ep[1]=tt->tm_mon+1; ep[2]=tt->tm_mday;
        ep[3]=tt->tm_hour; ep[4]=tt->tm_min; ep[5]=tt->tm_sec+tv.tv_usec*1E-6;
    }
#endif
    return timeadd(epoch2time(ep),timeoffset_);
}


/* set current time in utc -----------------------------------------------------
* set current time in utc
* args   : gtime_t          I   current time in utc
* return : none
* notes  : just set time offset between cpu time and current time
*          the time offset is reflected to only timeget()
*          not reentrant
*-----------------------------------------------------------------------------*/
extern void timeset(gtime_t t)
{
    timeoffset_+=timediff(t,timeget());
}

/* gpstime to utc --------------------------------------------------------------
* convert gpstime to utc considering leap seconds
* args   : gtime_t t        I   time expressed in gpstime
* return : time expressed in utc
* notes  : ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
extern gtime_t gpst2utc(gtime_t t)
{
    gtime_t tu;
    int i;
    
    for (i=0;leaps[i][0]>0;i++) {
        tu=timeadd(t,leaps[i][6]);
        if (timediff(tu,epoch2time(leaps[i]))>=0.0) return tu;
    }
    return t;
}

/* utc to gpstime --------------------------------------------------------------
* convert utc to gpstime considering leap seconds
* args   : gtime_t t        I   time expressed in utc
* return : time expressed in gpstime
* notes  : ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
extern gtime_t utc2gpst(gtime_t t)
{
    int i;
    
    for (i=0;leaps[i][0]>0;i++) {
        if (timediff(t,epoch2time(leaps[i]))>=0.0) return timeadd(t,-leaps[i][6]);
    }
    return t;
}

/* gpstime to bdt --------------------------------------------------------------
* convert gpstime to bdt (beidou navigation satellite system time)
* args   : gtime_t t        I   time expressed in gpstime
* return : time expressed in bdt
* notes  : ref [8] 3.3, 2006/1/1 00:00 BDT = 2006/1/1 00:00 UTC
*          no leap seconds in BDT
*          ignore slight time offset under 100 ns
*-----------------------------------------------------------------------------*/
extern gtime_t gpst2bdt(gtime_t t)
{
    return timeadd(t,-14.0);
}

/* bdt to gpstime --------------------------------------------------------------
* convert bdt (beidou navigation satellite system time) to gpstime
* args   : gtime_t t        I   time expressed in bdt
* return : time expressed in gpstime
* notes  : see gpst2bdt()
*-----------------------------------------------------------------------------*/
extern gtime_t bdt2gpst(gtime_t t)
{
    return timeadd(t,14.0);
}

/* time to day and sec -------------------------------------------------------*/
static double time2sec(gtime_t time, gtime_t *day)
{
    double ep[6],sec;
    time2epoch(time,ep);
    sec=ep[3]*3600.0+ep[4]*60.0+ep[5];
    ep[3]=ep[4]=ep[5]=0.0;
    *day=epoch2time(ep);
    return sec;
}
/* utc to gmst -----------------------------------------------------------------
* convert utc to gmst (Greenwich mean sidereal time)
* args   : gtime_t t        I   time expressed in utc
*          double ut1_utc   I   UT1-UTC (s)
* return : gmst (rad)
*-----------------------------------------------------------------------------*/
extern double utc2gmst(gtime_t t, double ut1_utc)
{
    const double ep2000[]={2000,1,1,12,0,0};
    gtime_t tut,tut0;
    double ut,t1,t2,t3,gmst0,gmst;
    
    tut=timeadd(t,ut1_utc);
    ut=time2sec(tut,&tut0);
    t1=timediff(tut0,epoch2time(ep2000))/86400.0/36525.0;
    t2=t1*t1; t3=t2*t1;
    gmst0=24110.54841+8640184.812866*t1+0.093104*t2-6.2E-6*t3;
    gmst=gmst0+1.002737909350795*ut;
    
    return fmod(gmst,86400.0)*PI/43200.0; /* 0 <= gmst <= 2*PI */
}

/* time to string --------------------------------------------------------------
* convert gtime_t struct to string
* args   : gtime_t t        I   gtime_t struct
*          char   *s        O   string ("yyyy/mm/dd hh:mm:ss.ssss")
*          int    n         I   number of decimals
* return : none
*-----------------------------------------------------------------------------*/
extern void time2str(gtime_t t, char *s, int n)
{
    double ep[6];
    
    if (n<0) n=0; else if (n>12) n=12;
    if (1.0-t.sec<0.5/pow(10.0,n)) {t.time++; t.sec=0.0;};
    time2epoch(t,ep);
    sprintf(s,"%04.0f/%02.0f/%02.0f %02.0f:%02.0f:%0*.*f",ep[0],ep[1],ep[2],
            ep[3],ep[4],n<=0?2:n+3,n<=0?0:n,ep[5]);
}

/* get time string -------------------------------------------------------------
* get time string
* args   : gtime_t t        I   gtime_t struct
*          int    n         I   number of decimals
* return : time string
* notes  : not reentrant, do not use multiple in a function
*-----------------------------------------------------------------------------*/
extern char *time_str(gtime_t t, int n)
{
    static char buff[64];
    time2str(t,buff,n);
    return buff;
}

/* time to day of year ---------------------------------------------------------
* convert time to day of year
* args   : gtime_t t        I   gtime_t struct
* return : day of year (days)
*-----------------------------------------------------------------------------*/
extern double time2doy(gtime_t t)
{
    double ep[6];

    time2epoch(t,ep);
    ep[1]=ep[2]=1.0; ep[3]=ep[4]=ep[5]=0.0;
    return timediff(t,epoch2time(ep))/86400.0+1.0;
}

/* adjust gps week number ------------------------------------------------------
* adjust gps week number using cpu time
* args   : int   week       I   not-adjusted gps week number
* return : adjusted gps week number
*-----------------------------------------------------------------------------*/
extern int adjgpsweek(int week)
{
    int w;
    (void)time2gpst(utc2gpst(timeget()),&w);
    if (w<1560) w=1560; /* use 2009/12/1 if time is earlier than 2009/12/1 */
    return week+(w-week+512)/1024*1024;
}

/* get tick time ---------------------------------------------------------------
* get current tick in ms
* args   : none
* return : current tick in ms
*-----------------------------------------------------------------------------*/
extern unsigned int tickget(void)
{
#ifdef WIN32
    return (unsigned int)timeGetTime();
#else
#if (0)
    struct timespec tp={0};
#endif
    struct timeval  tv={0};
    
#ifdef CLOCK_MONOTONIC_RAW
    /* linux kernel > 2.6.28 */
#if (1)
    gettimeofday(&tv,NULL);
    return tv.tv_sec*1000u+tv.tv_usec/1000u;
#endif
#if (0)
    if (!clock_gettime(CLOCK_MONOTONIC_RAW,&tp)) {
        return tp.tv_sec*1000u+tp.tv_nsec/1000000u;
    }
#endif
#else
    gettimeofday(&tv,NULL);
    return tv.tv_sec*1000u+tv.tv_usec/1000u;
#endif
#endif /* WIN32 */
}

/* sleep ms --------------------------------------------------------------------
* sleep ms
* args   : int   ms         I   miliseconds to sleep (<0:no sleep)
* return : none
*-----------------------------------------------------------------------------*/
extern void sleepms(int ms)
{
#ifdef WIN32
    if (ms<5) Sleep(1); else Sleep(ms);
#else
    struct timespec ts;
    if (ms<=0) return;
    ts.tv_sec=(time_t)(ms/1000);
    ts.tv_nsec=(long)(ms%1000*1000000);
    nanosleep(&ts,NULL);
#endif
}






