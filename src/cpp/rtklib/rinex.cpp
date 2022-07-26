
#include <boost/log/trivial.hpp>


#include <string>

using std::string;


#include "rinexNavWrite.hpp"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "biasSINEX.hpp"
#include "constants.hpp"
#include "station.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "rinex.hpp"
#include "enum.h"

#define MAXRNXLEN   (16*MAXOBSTYPE+4)   ///< max rinex record length
#define MAXPOSHEAD  1024            	///< max head line position
#define MINFREQ_GLO -7              	///< min frequency number glonass
#define MAXFREQ_GLO 13              	///< max frequency number glonass

const double ura_eph[]=
{
	///< ura values (ref [3] 20.3.3.3.1.1)
	2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24, 48, 96, 192, 384, 768, 1536, 3072, 6144, 0
};

BETTER_ENUM(E_EphType,	short int,
			NONE,			///< NONE for unknown
			EPH,			///< GPS/QZS LNAV, GAL IFNV, BDS D1D2 Ephemeris
			GEPH,			///< GLO Ephemeris
			SEPH,			///< SBAS Ephemeris
			CEPH,			///< GPS/QZS/BDS CNVX Ephemeris
			STO,			///< STO message
			EOP,			///< EOP message
			ION)			///< ION message

/** Default navigation massage type for RINEX 3 and 2
*/
map<E_Sys, E_NavMsgType> defNavMsgType =
{
	{E_Sys::GPS, E_NavMsgType::LNAV},
	{E_Sys::GLO, E_NavMsgType::FDMA},
	{E_Sys::GAL, E_NavMsgType::IFNV},
	{E_Sys::BDS, E_NavMsgType::D1D2},
	{E_Sys::QZS, E_NavMsgType::LNAV},
	{E_Sys::IRN, E_NavMsgType::LNAV},
	{E_Sys::SBS, E_NavMsgType::SBAS}
};

/** Set string without tail space
*/
void setstr(char *dst, const char *src, int n)
{
	char *p=dst;
	const char *q=src;
	while (*q&&q<src+n) *p++=*q++;
	*p--='\0';
	while (p>=dst&&*p==' ') *p--='\0';
}

/** Adjust time considering week handover
*/
GTime adjweek(GTime t, GTime t0)
{
	double tt = t - t0;
	if (tt < -302400)		return t + 604800.0;
	if (tt > +302400)		return t - 604800.0;
							return t;
}

/** Adjust time considering day handover
*/
GTime adjday(GTime t, GTime t0)
{
	double tt = t - t0;
	if (tt < -43200.0)		return t + 86400.0;
	if (tt > +43200.0)		return t - 86400.0;
							return t;
}

/** URA value (m) to ura index
*/
int uraindex(double value)
{
	int index;
	for (index = 0; index < 15; index++)
		if (ura_eph[index] >= value)
			break;
	return index;
}



/** Convert rinex obs type ver.2 -> ver.3
*/
void convcode(
	double		ver,
	int			sys,
	const char*	str, 
	char*		type)
{
	strcpy(type,"   ");

	if      (!strcmp(str,"P1"))
	{
		// ver.2.11 GPS L1PY,GLO L2P
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1W",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1P",'C');
	}
	else if (!strcmp(str,"P2"))
	{
		// ver.2.11 GPS L2PY,GLO L2P
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2W",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2P",'C');
	}
	else if (!strcmp(str,"C1"))
	{
		// ver.2.11 GPS L1C,GLO L1C/A
		if      (ver>=2.12) ; // reject C1 for 2.12
		else if (sys==+E_Sys::GPS) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",'C'); // ver.2.12
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",'C');
	}
	else if (!strcmp(str,"C2"))
	{
		if (sys==+E_Sys::GPS)
		{
			if (ver>=2.12) sprintf(type,"%c2W",'C'); // L2P(Y)
			else           sprintf(type,"%c2X",'C'); // L2C
		}
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2C",'C');
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",'C');
		else if (sys==+E_Sys::BDS) sprintf(type,"%c1X",'C'); // ver.2.12 B1
	}
	else if (ver>=2.12&&str[1]=='A')
	{
		// ver.2.12 L1C/A
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",str[0]);
	}
	else if (ver>=2.12&&str[1]=='B')
	{
		// ver.2.12 GPS L1C
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1X",str[0]);
	}
	else if (ver>=2.12&&str[1]=='C')
	{
		// ver.2.12 GPS L2C
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",str[0]);
	}
	else if (ver>=2.12&&str[1]=='D')
	{
		// ver.2.12 GLO L2C/A
		if      (sys==+E_Sys::GLO) sprintf(type,"%c2C",str[0]);
	}
	else if (ver>=2.12&&str[1]=='1')
	{
		// ver.2.12 GPS L1PY,GLO L1P
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1W",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1P",str[0]);
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",str[0]); // tentative
		else if (sys==+E_Sys::BDS) sprintf(type,"%c1X",str[0]); // extension
	}
	else if (ver<2.12&&str[1]=='1')
	{
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",str[0]); // tentative
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",str[0]);
	}
	else if (str[1]=='2')
	{
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2W",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2P",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",str[0]);
		else if (sys==+E_Sys::BDS) sprintf(type,"%c1X",str[0]); // ver.2.12 B1
	}
	else if (str[1]=='5')
	{
		if      (sys==+E_Sys::GPS) sprintf(type,"%c5X",str[0]);
		else if (sys==+E_Sys::GAL) sprintf(type,"%c5X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c5X",str[0]);
		else if (sys==+E_Sys::SBS) sprintf(type,"%c5X",str[0]);
	}
	else if (str[1]=='6')
	{
		if      (sys==+E_Sys::GAL) sprintf(type,"%c6X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c6X",str[0]);
		else if (sys==+E_Sys::BDS) sprintf(type,"%c6X",str[0]); // ver.2.12 B3
	}
	else if (str[1]=='7')
	{
		if      (sys==+E_Sys::GAL) sprintf(type,"%c7X",str[0]);
		else if (sys==+E_Sys::BDS) sprintf(type,"%c7X",str[0]); // ver.2.12 B2
	}
	else if (str[1]=='8')
	{
		if      (sys==+E_Sys::GAL) sprintf(type,"%c8X",str[0]);
	}

	BOOST_LOG_TRIVIAL(debug)
	<< "convcode: ver=" << ver
	<< " sys="		<< sys
	<< " type= "	<< str
	<< " -> "		<< type;
}


/** Decode obs header
*/
void decodeObsh(
	std::istream& 					inputStream,
	string&							line,
	double							ver,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	Navigation&						nav,
	RinexStation*					sta)
{
	double del[3];
	int n,prn,fcn;
	const char *p;
	char* buff	= &line[0];
	char* label	= buff+60;


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeObsh: ver=" << ver;

	if      (strstr(label,"MARKER NAME"         ))
	{
		if (sta)
		{
			sta->id				.assign(buff,		60);
		}
	}
	else if (strstr(label,"MARKER NUMBER"       ))
	{
		if (sta) 
		{
			sta->marker			.assign(buff,		20);
		}
	}
//     else if (strstr(label,"MARKER TYPE"         )) ; // ver.3
//     else if (strstr(label,"OBSERVER / AGENCY"   )) ;
	else if (strstr(label,"REC # / TYPE / VERS" ))
	{
		if (sta)
		{
			sta->recSerial		.assign(buff,		20);
			sta->recType		.assign(buff+20,	20);
			sta->recFWVersion	.assign(buff+40,	20);
		}
	}
	else if (strstr(label,"ANT # / TYPE"        ))
	{
		if (sta)
		{
			sta->antSerial		.assign(buff,		20);
			sta->antDesc		.assign(buff+20,	20);
		}
	}
	else if (strstr(label,"APPROX POSITION XYZ" ))
	{
		if (sta)
		{
			for (int i = 0, j = 0; i<3 ; i++, j += 14)
				sta->pos[i] = str2num(buff,j,14);
		}
	}
	else if (strstr(label,"ANTENNA: DELTA H/E/N"))
	{
		if (sta)
		{
			for (int i = 0, j = 0; i < 3; i++, j += 14)
				del[i] = str2num(buff,j,14);

			sta->del[2] = del[0]; // h
			sta->del[0] = del[1]; // e
			sta->del[1] = del[2]; // n
		}
	}
//     else if (strstr(label,"ANTENNA: DELTA X/Y/Z")) ; // opt ver.3
//     else if (strstr(label,"ANTENNA: PHASECENTER")) ; // opt ver.3
//     else if (strstr(label,"ANTENNA: B.SIGHT XYZ")) ; // opt ver.3
//     else if (strstr(label,"ANTENNA: ZERODIR AZI")) ; // opt ver.3
//     else if (strstr(label,"ANTENNA: ZERODIR XYZ")) ; // opt ver.3
//     else if (strstr(label,"CENTER OF MASS: XYZ" )) ; // opt ver.3
	else if (strstr(label,"SYS / # / OBS TYPES" ))
	{
		// ver.3
		//get system from code letter
		char code[]	= "x00";
		code[0]		= buff[0];

		SatSys Sat(code);

		if (Sat.sys == +E_Sys::NONE)
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "invalid system code: sys=" << code[0];

			return;
		}

		n = (int) str2num(buff, 3, 3);

		for (int j = 0, k = 7; j < n; j++, k += 4)
		{
			if (k > 58)
			{
				//more on the next line

				if (!std::getline(inputStream, line))
					break;

				buff = &line[0];
				k = 7;
			}

			CodeType codeType;
			codeType.type = buff[k];

			char code[] = "Lxx";
			code[1] = buff[k+1];
			code[2] = buff[k+2];
			if	( (Sat.sys == +E_Sys::BDS)
				&&(code[1] == '2'))
			{
				// change beidou B1 code: 3.02 draft -> 3.02
				code[1] = '1';
			}
			try
			{
				codeType.code = E_ObsCode::_from_string(code);
			}
			catch (...)
			{
				BOOST_LOG_TRIVIAL(debug)
				<< "invalid obs code: " << code;
			}

			sysCodeTypes[Sat.sys].push_back(codeType);
		}

		// if unknown code in ver.3, set default code
// 		for (auto& codeType : sysCodeTypes[Sat.sys])
// 		{
//             if (tobs[i][j][2])
// 				continue;
//
//             if (!(p = strchr(frqcodes, tobs[i][j][1])))
// 				continue;
//
				// default codes for unknown code
//     			const char *defcodes[] =
//    			{
//         			"CWX   ",   // GPS: L125___
//         			"CC    ",   // GLO: L12____
//         			"X XXXX",   // GAL: L1_5678
//         			"CXXX  ",   // QZS: L1256__
//         			"C X   ",   // SBS: L1_5___
//         			"X  XX "	// BDS: L1__67_
//     			};
//             	tobs[i][j][2] = defcodes[i][(int)(p - frqcodes)];
//
//             	BOOST_LOG_TRIVIAL(debug)
// 				<< "set default for unknown code: sys=" << buff[0]
// 				<< " code=" << tobs[i][j];
//         }
	}
//     else if (strstr(label,"WAVELENGTH FACT L1/2")) ; // opt ver.2
//     else if (strstr(label,"# / TYPES OF OBSERV" ))
// 	{ // ver.2
//         n=(int)str2num(buff,0,6);
//         for (i=nt=0,j=10;i<n;i++,j+=6)
// 		{
//             if (j>58)
// 			{
//                 if (!fgets(buff,MAXRNXLEN,fp))
// 					break;
//                 j=10;
//             }
//
//             if (nt>=MAXOBSTYPE-1)
// 				continue;
//
//             if (ver<=2.99)
// 			{
//                 setstr(str,buff+j,2);
//                 convcode(ver,E_Sys::GPS,str,tobs[0][nt]);
//                 convcode(ver,E_Sys::GLO,str,tobs[1][nt]);
//                 convcode(ver,E_Sys::GAL,str,tobs[2][nt]);
//                 convcode(ver,E_Sys::QZS,str,tobs[3][nt]);
//                 convcode(ver,E_Sys::SBS,str,tobs[4][nt]);
//                 convcode(ver,E_Sys::BDS,str,tobs[5][nt]);
//             }
//             nt++;
//         }
//         *tobs[0][nt]='\0';
//     }
//     else if (strstr(label,"SIGNAL STRENGTH UNIT")) ; // opt ver.3
//     else if (strstr(label,"INTERVAL"            )) ; // opt
	else if (strstr(label,"TIME OF FIRST OBS"   ))
	{
		if      (!strncmp(buff+48,"GPS",3)) tsys=TSYS_GPS;
		else if (!strncmp(buff+48,"GLO",3)) tsys=TSYS_UTC;
		else if (!strncmp(buff+48,"GAL",3)) tsys=TSYS_GAL;
		else if (!strncmp(buff+48,"QZS",3)) tsys=TSYS_QZS; // ver.3.02
		else if (!strncmp(buff+48,"BDT",3)) tsys=TSYS_CMP; // ver.3.02
	}
//     else if (strstr(label,"TIME OF LAST OBS"    )) ; // opt
//     else if (strstr(label,"RCV CLOCK OFFS APPL" )) ; // opt
//     else if (strstr(label,"SYS / DCBS APPLIED"  )) ; // opt ver.3
//     else if (strstr(label,"SYS / PCVS APPLIED"  )) ; // opt ver.3
//     else if (strstr(label,"SYS / SCALE FACTOR"  )) ; // opt ver.3
//     else if (strstr(label,"SYS / PHASE SHIFTS"  )) ; // ver.3.01
	else if (strstr(label,"GLONASS SLOT / FRQ #"))
	{
		// ver.3.02
		p = buff + 4;
		for (int i = 0; i < 8; i++, p += 8)
		{
			if (sscanf(p,"R%2d %2d",&prn,&fcn)<2)
				continue;

			if (1 <= prn
				&&prn <= MAXPRNGLO)
			{
				nav.glo_fcn[prn-1] = fcn+8;
			}

		}
	}
	else if (strstr(label,"GLONASS COD/PHS/BIS" ))
	{
		// ver.3.02
		p = buff;
		for (int i = 0; i < 4; i++, p += 13)
		{
			if      (strncmp(p+1,"C1C",3)) nav.glo_cpbias[0]=str2num(p,5,8);
			else if (strncmp(p+1,"C1P",3)) nav.glo_cpbias[1]=str2num(p,5,8);
			else if (strncmp(p+1,"C2C",3)) nav.glo_cpbias[2]=str2num(p,5,8);
			else if (strncmp(p+1,"C2P",3)) nav.glo_cpbias[3]=str2num(p,5,8);
		}
	}
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		// opt
		nav.leaps=(int)str2num(buff,0,6);
	}
//     else if (strstr(label,"# OF SALTELLITES"    )) ; // opt
//     else if (strstr(label,"PRN / # OF OBS"      )) ; // opt
}

/** Decode nav header
*/
void decodeNavh(
	string&		line,	///< Line to decode
	E_Sys		sys,	///< GNSS system
	Navigation&	nav)	///< Navigation data
{
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decodeNavh:";

	if      (strstr(label,"ION ALPHA"           ))
	{
		// opt ver.2
		E_NavMsgType type	= defNavMsgType[sys];
		GTime time 			= {};

		ION& ionEntry		= nav.ionMap[sys][type][time];

		ionEntry.type		= type;
		ionEntry.Sat.sys	= sys;
		ionEntry.ttm		= time;

		for (int i = 0, j = 2; i < 4; i++, j += 12)
			ionEntry.vals[i]	= str2num(buff,j,12);
	}
	else if (strstr(label,"ION BETA"            ))
	{
		// opt ver.2
		E_NavMsgType type	= defNavMsgType[sys];
		GTime time 			= {};

		ION& ionEntry		= nav.ionMap[sys][type][time];

		ionEntry.type		= type;
		ionEntry.Sat.sys	= sys;
		ionEntry.ttm		= time;

		for (int i = 0, j = 2; i < 4; i++, j += 12)
			ionEntry.vals[i+4]	= str2num(buff,j,12);
	}
	else if (strstr(label,"DELTA-UTC: A0,A1,T,W"))
	{
		// opt ver.2
		E_NavMsgType type	= defNavMsgType[sys];
		E_StoCode code		= E_StoCode::NONE;
		switch (sys)
		{
			case E_Sys::GPS:	code = E_StoCode::GPUT;	break;
			case E_Sys::QZS:	code = E_StoCode::QZUT;	break;
			case E_Sys::GAL:	code = E_StoCode::GAUT;	break;
		}

		double sec			= str2num(buff,31,9);
		double week			= str2num(buff,40,9);
		GTime time			= gpst2time(week, sec);

		STO& stoEntry		= nav.stoMap[code][type][time];

		stoEntry.type		= type;
		stoEntry.Sat.sys	= sys;
		stoEntry.tot		= time;
		stoEntry.code		= code;

		stoEntry.A0			= str2num(buff, 3,19);
		stoEntry.A1			= str2num(buff,22,19);
		stoEntry.A2			= 0;
	}
	else if (strstr(label,"IONOSPHERIC CORR"    ))
	{
		// opt ver.3
		char sysStr[4]		= "";
		strncpy(sysStr,buff,3);
		sys					= E_Sys::_from_string(sysStr);
		E_NavMsgType type	= defNavMsgType[sys];
		GTime time			= {};

		ION& ionEntry		= nav.ionMap[sys][type][time];

		ionEntry.type		= type;
		ionEntry.Sat.sys	= sys;
		ionEntry.Sat.prn	= str2num(buff,55,3);
		ionEntry.ttm		= time;

		if		( buff[3] == 'A'
				||buff[3] == ' ')
		{
			for (int i = 0, j = 5; i < 4; i++, j += 12)
				ionEntry.vals[i]	= str2num(buff,j,12);
		}
		else if	( buff[3] == 'B')
		{
			for (int i = 0, j = 5; i < 4; i++, j += 12)
				ionEntry.vals[i+4]	= str2num(buff,j,12);
		}
	}
	else if (strstr(label,"TIME SYSTEM CORR"    ))
	{
		// opt ver.3
		char codeStr[5] 	= "";
		strncpy(codeStr,buff,4);
		E_StoCode code		= E_StoCode::_from_string(codeStr);

		char id[8] 			= "";
		strncpy(id,buff+51,5);
		SatSys Sat			= SatSys(id);

		if (Sat.sys == +E_Sys::NONE)
		{
			switch (code)
			{
				case E_StoCode::GPUT :	Sat.sys = E_Sys::GPS;	break;
				case E_StoCode::GLUT :	Sat.sys = E_Sys::GLO;	break;
				case E_StoCode::GAUT :	Sat.sys = E_Sys::GAL;	break;
				case E_StoCode::BDUT :	Sat.sys = E_Sys::BDS;	break;
				case E_StoCode::QZUT :	Sat.sys = E_Sys::QZS;	break;
				case E_StoCode::SBUT :	Sat.sys = E_Sys::SBS;	break;
				case E_StoCode::GAGP :	Sat.sys = E_Sys::GAL;	break;
				case E_StoCode::QZGP :	Sat.sys = E_Sys::QZS;	break;
			}
		}
		// UTC ID skipped

		E_NavMsgType type	= defNavMsgType[Sat.sys];

		double sec			= str2num(buff,38, 7);
		double week			= str2num(buff,45, 5);
		GTime time			= {};
		if (Sat.sys == +E_Sys::BDS)	{	time = bdt2gpst( bdt2time(week, sec));	}
		else						{	time = 			gpst2time(week, sec);	}

		STO& stoEntry		= nav.stoMap[code][type][time];

		stoEntry.type		= type;
		stoEntry.Sat		= Sat;
		stoEntry.tot		= time;
		stoEntry.ttm		= time;
		stoEntry.code		= code;

		stoEntry.A0			= str2num(buff, 5,17);
		stoEntry.A1			= str2num(buff,22,16);
		stoEntry.A2			= 0.0;
	}
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		// opt
		nav.leaps=(int)str2num(buff,0,6);
	}
}
/** Decode gnav header
*/
void decodeGnavh(
	string&		line,
	Navigation&	nav)
{
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decodeGnavh:";

	if      (strstr(label,"CORR TO SYTEM TIME"  )) ; // opt
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		// opt
		nav.leaps=(int)str2num(buff,0,6);
	}
}

/** Decode geo nav header
*/
void decodeHnavh(
	string&		line,
	Navigation&	nav)
{
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decodeHnavh:";

	if      (strstr(label, "CORR TO SYTEM TIME"  )) ; // opt
	else if (strstr(label, "D-UTC A0,A1,T,W,S,U" )) ; // opt
	else if (strstr(label, "LEAP SECONDS"        ))
	{
		// opt
		nav.leaps= (int)str2num(buff,0,6);
	}
}

/** Read rinex header
*/
int readrnxh(
	std::istream& 					inputStream,
	double&							ver,
	char&							type,
	E_Sys&							sys,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	Navigation&						nav,
	RinexStation*					sta)
{
	string	line;
	int 	i		= 0;
	int 	block	= 0;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxh:";

	ver		= 2.10;
	type	= ' ';
	sys		= E_Sys::GPS;
	tsys	= TSYS_GPS;

	char sysChar = '\0';
	int typeOffset = 20;
	int sysCharOffset = 40;

	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];
		char*	label	= buff + 60;

		if (line.length() <= 60)
			continue;
		else if (strstr(label,"RINEX VERSION / TYPE"))
		{
			ver		= str2num(buff,0,9);

			type	= buff[typeOffset];

			sysChar = buff[sysCharOffset];

			// possible error in generation by one manufacturer. This hack gets around it
			if(ver==3.04 && type == ' ')
			{
				typeOffset		+= 1;
				sysCharOffset	+= 2;
				type	= buff[typeOffset];

				sysChar = buff[sysCharOffset];

			}

			// satellite system
			switch (sysChar)
			{
				case ' ':
				case 'G': sys = E_Sys::GPS;  tsys = TSYS_GPS; break;
				case 'R': sys = E_Sys::GLO;  tsys = TSYS_UTC; break;
				case 'E': sys = E_Sys::GAL;  tsys = TSYS_GAL; break; // v.2.12
				case 'S': sys = E_Sys::SBS;  tsys = TSYS_GPS; break;
				case 'J': sys = E_Sys::QZS;  tsys = TSYS_QZS; break; // v.3.02
				case 'C': sys = E_Sys::BDS;  tsys = TSYS_CMP; break; // v.2.12
				case 'M': sys = E_Sys::NONE; tsys = TSYS_GPS; break; // mixed
				default :
					BOOST_LOG_TRIVIAL(debug)
					<< "unsupported satellite system: " << sysChar;

					break;
			}
			continue;
		}
		else if (strstr(label,"PGM / RUN BY / DATE"))
			continue;
		else if (strstr(label,"COMMENT"))
		{ // opt

			// read cnes wl satellite fractional bias
			if	( strstr(buff,"WIDELANE SATELLITE FRACTIONAL BIASES")
				||strstr(buff,"WIDELANE SATELLITE FRACTIONNAL BIASES"))
			{
				block=1;
			}
			else if (block)
			{
				double bias;
				SatSys Sat;

				// cnes/cls grg clock
				if (!strncmp(buff,"WL",2)&&(Sat=SatSys(buff+3), Sat)&&
					sscanf(buff+40,"%lf",&bias)==1)
				{
					nav.satNavMap[Sat].wlbias = bias;
				}
				// cnes ppp-wizard clock
				else if ((Sat=SatSys(buff+1), Sat)&&sscanf(buff+6,"%lf",&bias)==1)
				{
					nav.satNavMap[Sat].wlbias = bias;
				}
			}
			continue;
		}
		// file type
		switch (type)
		{            
			case 'O': decodeObsh(inputStream, line, ver, tsys, sysCodeTypes, nav, sta); break;
			case 'N': decodeNavh			(  line, sys,        nav); break; // GPS (ver.2) or mixed (ver.3)
			case 'G': decodeGnavh			(  line,             nav); break;
			case 'H': decodeHnavh			(  line,             nav); break;
			case 'J': decodeNavh 			(  line, E_Sys::QZS, nav); break; // extension
			case 'E':
			case 'L': decodeNavh 			(  line, E_Sys::GAL, nav); break; // extension
		}
		if (strstr(label,"END OF HEADER"))
			return 1;

		if (++i>=MAXPOSHEAD
			&&type==' ')
		{
			break; // no rinex file
		}
	}
	return 0;
}
/** Decode obs epoch
*/
int decodeObsepoch(
	std::istream& 		inputStream,
	string&				line,
	double				ver,
	GTime&				time,
	int&				flag,
	vector<SatSys>&		sats)

{
	int n;
	char*	buff	= &line[0]; 


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeObsepoch: ver=" << ver;

	if	(ver <= 2.99)
	{
		// ver.2
		n		= (int) str2num(buff, 29, 3);
		if (n <= 0)
			return 0;

		// epoch flag: 3:new site,4:header info,5:external event
		flag	= (int) str2num(buff, 28, 1);

		if	( flag >= 3
			&&flag <= 5)
		{
			return n;
		}

		if (str2time(buff, 0, 26, time))
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "rinex obs invalid epoch: epoch=" << buff;
			return 0;
		}

		for (int i = 0, j = 32; i < n; i++, j += 3)
		{
			if (j >= 68)
			{
				//more on the next line
				if (!std::getline(inputStream, line))
					break;

				buff = &line[0];

				j = 32;
			}

			sats.push_back(SatSys(buff + j));
		}
	}
	else
	{
		// ver.3
		n		= (int) str2num(buff, 32, 3);
		if (n <= 0)
		{
			return 0;
		}

		flag	= (int) str2num(buff, 31, 1);

		if	( flag >= 3
			&&flag <= 5)
			return n;

		if	( buff[0] != '>'
			||str2time(buff, 1, 28, time))
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "rinex obs invalid epoch: epoch=" << buff;
			return 0;
		}
	}

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeObsepoch: time=" << time.to_string(3)
// 	<< " flag=" << flag;

	return n;
}

/** Decode obs data
*/
int decodeObsdata(
	std::istream& 					inputStream,
	string&							line,
	double							ver,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	RawObs&							obs,
	SatSys&							v2SatSys_ptr)
{
	char		satid[8]	= "";
	int			stat		= 1;
	char*		buff		= &line[0];


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeObsdata: ver=" << ver;

	if (ver > 2.99)
	{
		// ver.3
		strncpy(satid, buff, 3);
		obs.Sat = SatSys(satid);
	}
	else
	{
		obs.Sat = v2SatSys_ptr;
	}

	if (!obs.Sat)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "decodeObsdata: unsupported sat sat=" << satid;

		stat = 0;
	}

	vector<CodeType>& codeTypes = sysCodeTypes[obs.Sat.sys];

	int j;
	if (ver <= 2.99)	j = 0;
	else				j = 3;

	if (!stat)
		return 0;

	for (auto& codeType : codeTypes)
	{
//         if	( ver	<= 2.99
// 			&&j		>= 80)
// 		{
// 			// ver.2
//             if (!fgets(buff,MAXRNXLEN,fp))
// 				break;
//             j = 0;
//         }

		E_FType ft		= ftypes[codeType.code];

		RawSig* rawSig = nullptr;
		list<RawSig>& sigList = obs.SigsLists[ft];

		for (auto& sig : sigList)
		{
			if (sig.code == codeType.code)
			{
				rawSig = &sig;
				break;
			}
		}
		if (rawSig == nullptr)
		{
			RawSig raw;
			raw.code = codeType.code;

			sigList.push_back(raw);
			rawSig = &sigList.back();
		}


		double val = str2num(buff, j,		14);
		double lli = str2num(buff, j+14,	1);
		lli = (unsigned char) lli & 0x03;

// 		val += shift;// todo aaron, phase shift needed
		RawSig& sig = *rawSig;
		switch (codeType.type)
		{
			case 'C': sig.P		= val; 									break;
			case 'L': sig.L		= val; 				sig.LLI = lli;  	break;
			case 'D': sig.D		= val;                        			break;
			case 'S': sig.snr	= val * 4 + 0.5;   						break;
		}

		j += 16;
	}

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeObsdata: time=" << obs.time.to_string(0)
// 	<< " sat=" << obs.Sat.id();

	return 1;
}

/** Read rinex obs data body
*/
int readrnxobsb(
	std::istream& 					inputStream,
	double							ver,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	int&							flag,
	ObsList&						obsList)
{
	GTime			time = {};
	string			line;
	int				i		= 0;
	vector<SatSys>	sats;
	int				nSats = 0;	//cant replace with sats.size()

	// read record
	while (std::getline(inputStream, line))
	{
		// decode obs epoch
		if (i == 0)
		{
			nSats = decodeObsepoch(inputStream, line, ver, time, flag, sats);
			if (nSats <= 0)
			{
				continue;
			} 
		}
		else if ( flag <= 2
				||flag == 6)
		{
			RawObs rawObs = {};

			rawObs.time	= time;

			// decode obs data
			bool pass = decodeObsdata(inputStream, line, ver, sysCodeTypes, rawObs, sats[i]);
			if	(pass)
			{
				// save obs data
				obsList.push_back(rawObs);
			}
		}

		if (++i > nSats)
			return obsList.size();
	}

	return -1;
}

/** Read rinex obs
*/
int readrnxobs(
	std::istream& 					inputStream,
	double							ver,
	int								tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	ObsList&						obsList)
{
	int flag = 0;
	int stat = 0;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxobs: ver=" << ver << " tsys=" << tsys;

	// read rinex obs data body
	int n = readrnxobsb(inputStream, ver, sysCodeTypes, flag, obsList);

	if	( n >= 0
		&&stat >= 0)
	{
		for (auto& obs : obsList)
		{
			// utc -> gpst
			if (tsys == TSYS_UTC)
				obs.time = utc2gpst(obs.time);
		}

		stat = 1;
	}

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxobs: nobs=" << obsList.size()
// 	<< " stat=" << stat;

	return stat;
}

/** Decode ephemeris
*/
int decodeEph(
	double		ver,
	SatSys		Sat,
	GTime		toc,
	double*		data,
	Eph&		eph)
{
//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeEph: ver=" << ver << " sat=" << Sat.id();

	int sys = Sat.sys;

	if	( sys != +E_Sys::GPS
		&&sys != +E_Sys::GAL
		&&sys != +E_Sys::QZS
		&&sys != +E_Sys::BDS)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}
	eph.type=defNavMsgType[Sat.sys];
	eph.Sat=Sat;
	eph.toc=toc;

	eph.f0		= data[0];
	eph.f1		= data[1];
	eph.f2		= data[2];
	eph.crs		= data[ 4];
	eph.deln	= data[ 5];
	eph.M0		= data[ 6];
	eph.cuc		= data[ 7];
	eph.e		= data[ 8];
	eph.cus		= data[ 9];
	eph.A		= SQR(data[10]);
	eph.cic		= data[12];
	eph.OMG0	= data[13];
	eph.cis		= data[14];
	eph.i0		= data[15];
	eph.crc		= data[16];
	eph.omg		= data[17];
	eph.OMGd	= data[18];
	eph.idot	= data[19];

	if	( sys == +E_Sys::GPS
		||sys == +E_Sys::QZS)
	{
		eph.iode=(int)data[ 3];  	// IODE
		eph.iodc=(int)data[26];  	// IODC
		eph.toes=     data[11];  	// toe (s) in gps week
		eph.week=(int)data[21];  	// gps week
		eph.toe=adjweek(gpst2time(eph.week,data[11]),eph.toc);
		eph.ttm=adjweek(gpst2time(eph.week,data[27]),eph.toc);

		eph.code=(int)data[20];  	// GPS: codes on L2 ch
		eph.svh =(E_Svh)data[24];  	// sv health
		eph.sva=uraindex(data[23]);  // ura (m->index)
		eph.flag=(int)data[22];  	// GPS: L2 P data flag

		eph.tgd[0]=   data[25];  	// TGD
		eph.fit   =   data[28];  	// fit interval

		decomposeTGDBias(Sat, eph.tgd[0]);
	}
	else if (sys==+E_Sys::GAL)
	{
		// GAL ver.3
		eph.iode=(int)data[ 3];  	// IODnav
		eph.toes=     data[11];  	// toe (s) in galileo week
		eph.week=(int)data[21];  	// gal week = gps week
		eph.toe=adjweek(gpst2time(eph.week,data[11]),eph.toc);
		eph.ttm=adjweek(gpst2time(eph.week,data[27]),eph.toc);

		eph.code=(int)data[20];  	// data sources
									// bit 0 set: I/NAV E1-B
									// bit 1 set: F/NAV E5a-I
									// bit 2 set: I/NAV E5b-I
									// bit 8 set: af0-af2 toc are for E5a.E1
									// bit 9 set: af0-af2 toc are for E5b.E1
		unsigned short iNavMask = 0x0005;
		unsigned short fNavMask = 0x0002;
		if		(eph.code & iNavMask)	eph.type=E_NavMsgType::INAV;
		else if	(eph.code & fNavMask)	eph.type=E_NavMsgType::FNAV;

		eph.svh =(E_Svh)data[24];  	// sv health
									// bit     0: E1B DVS
									// bit   1-2: E1B HS
									// bit     3: E5a DVS
									// bit   4-5: E5a HS
									// bit     6: E5b DVS
									// bit   7-8: E5b HS
		eph.sva = sisaToSva(data[23]);
		//eph.sva =uraindex(data[23]); // ura (m->index)

		eph.tgd[0]=   data[25];  	// BGD E5a/E1
		eph.tgd[1]=   data[26];  	// BGD E5b/E1

		decomposeBGDBias(Sat, eph.tgd[0], eph.tgd[1]);
	}
	else if (sys==+E_Sys::BDS)
	{
		// BeiDou v.3.02
		if (Sat.prn>5&&Sat.prn<59)	eph.type=E_NavMsgType::D1; // MEO/IGSO
		else						eph.type=E_NavMsgType::D2; // GEO, prn range may change in the future*/

		eph.toc=bdt2gpst(eph.toc);  // bdt -> gpst
		eph.iode=(int)data[ 3];  	// AODE
		eph.iodc=(int)data[28];  	// AODC
		eph.toes=     data[11];  	// toe (s) in bdt week
		eph.week=(int)data[21];  	// bdt week
		eph.toe=bdt2gpst(bdt2time(eph.week,data[11])); // bdt -> gpst
		eph.ttm=bdt2gpst(bdt2time(eph.week,data[27])); // bdt -> gpst
		eph.toe=adjweek(eph.toe,eph.toc);
		eph.ttm=adjweek(eph.ttm,eph.toc);

		eph.svh =(E_Svh)data[24];	// satH1
		eph.sva=uraindex(data[23]);  // ura (m->index)

		eph.tgd[0]=   data[25];  	// TGD1 B1/B3
		eph.tgd[1]=   data[26];  	// TGD2 B2/B3
	}

	if (eph.iode<0||1023<eph.iode)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iode=" << eph.iode;
	}

	if (eph.iodc<0||1023<eph.iodc)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iodc=" << eph.iodc;
	}
	return 1;
}

/** Decode glonass ephemeris
*/
int decodeGeph(
	double	ver,	///< RINEX version
	SatSys	Sat,	///< Satellite ID
	GTime	toc,	///< Time of clock
	double*	data,	///< Data to decode
	Geph&	geph)	///< Glonass ephemeris
{
	GTime tof;
	double tow,tod;
	int week,dow;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeGeph: ver=" << ver << " sat=" << Sat.id();

	if (Sat.sys!=+E_Sys::GLO)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "glonass ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}

	geph.type=defNavMsgType[Sat.sys];
	geph.Sat=Sat;

	// toc rounded by 15 min in utc
	tow=time2gpst(toc,&week);
	toc=gpst2time(week,floor((tow+450.0)/900.0)*900);
	dow=(int)floor(tow/86400.0);

	// time of frame in utc
	tod=ver<=2.99?data[2]:fmod(data[2],86400.0); // tod (v.2), tow (v.3) in utc
	tof=gpst2time(week,tod+dow*86400.0);
	tof=adjday(tof,toc);

	geph.toe=utc2gpst(toc);   // toc (gpst)
	geph.tof=utc2gpst(tof);   // tof (gpst)

	// iode = tb (7bit), tb =index of UTC+3H within current day
	geph.iode=(int)(fmod(tow+10800.0,86400.0)/900.0+0.5);

	geph.taun= data[0];   	// taun
	geph.gamn= data[1];   	// +gamman

	geph.pos[0]=data[3]*1E3; geph.pos[1]=data[7]*1E3; geph.pos[2]=data[11]*1E3;
	geph.vel[0]=data[4]*1E3; geph.vel[1]=data[8]*1E3; geph.vel[2]=data[12]*1E3;
	geph.acc[0]=data[5]*1E3; geph.acc[1]=data[9]*1E3; geph.acc[2]=data[13]*1E3;

	geph.svh=(E_Svh)data[ 6];
	geph.frq=(int)data[10];
	geph.age=(int)data[14];

	// some receiver output >128 for minus frequency number
	if (geph.frq >  128)
		geph.frq -= 256;

	if (geph.frq<MINFREQ_GLO||MAXFREQ_GLO<geph.frq)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex gnav invalid freq: sat=" << Sat << " fn=" << geph.frq;
	}
	return 1;
}

/** Decode geo ephemeris
*/
int decodeSeph(
	double		ver,
	SatSys		Sat,
	GTime		toc,
	double*		data,
	Seph&		seph)
{
	int week;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeSeph: ver=" << ver << " sat=" << Sat.id();

	if (Sat.sys!=+E_Sys::SBS)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "geo ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}

	seph.type=defNavMsgType[Sat.sys];
	seph.Sat=Sat;
	seph.t0 =toc;

	time2gpst(seph.t0,&week);
	seph.tof=adjweek(gpst2time(week,data[2]),seph.t0);

	seph.af0=data[0];
	seph.af1=data[1];

	seph.pos[0]=data[3]*1E3; seph.pos[1]=data[7]*1E3; seph.pos[2]=data[11]*1E3;
	seph.vel[0]=data[4]*1E3; seph.vel[1]=data[8]*1E3; seph.vel[2]=data[12]*1E3;
	seph.acc[0]=data[5]*1E3; seph.acc[1]=data[9]*1E3; seph.acc[2]=data[13]*1E3;

	seph.svh=(E_Svh)data[6];
	seph.sva=uraindex(data[10]);

	return 1;
}

/** Decode CNVX ephemeris
*/
int decodeCeph(
	double			ver,	///< RINEX version
	SatSys			Sat,	///< Satellite ID
	E_NavMsgType	type,	///< Navigation message type
	GTime			toc,	///< Time of clock
	double*			data,	///< Data to decode
	Ceph&			ceph)	///< CNVX ephemeris
{
//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decodeCeph: ver=" << ver << " sat=" << Sat.id();

	if (ver < 4.0)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid RINEX version=" << ver;

		return -1;
	}

	if  ( type != +E_NavMsgType::CNAV
		&&type != +E_NavMsgType::CNV1
		&&type != +E_NavMsgType::CNV2
		&&type != +E_NavMsgType::CNV3)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid message type=" << type._to_string();

		return 0;
	}

	int sys = Sat.sys;

	if	( sys != +E_Sys::GPS
		&&sys != +E_Sys::QZS
		&&sys != +E_Sys::BDS)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}

	ceph.Sat	= Sat;
	ceph.type	= type;
	ceph.toc	= toc;

	ceph.f0	= data[0];
	ceph.f1	= data[1];
	ceph.f2	= data[2];
	ceph.Adot	= data[3];
	ceph.crs	= data[4];
	ceph.deln	= data[5];
	ceph.M0	= data[6];
	ceph.cuc	= data[7];
	ceph.e		= data[8];
	ceph.cus	= data[9];
	ceph.A		= SQR(data[10]);
	ceph.cic	= data[12];
	ceph.OMG0	= data[13];
	ceph.cis	= data[14];
	ceph.i0	= data[15];
	ceph.crc	= data[16];
	ceph.omg	= data[17];
	ceph.OMGd	= data[18];
	ceph.idot	= data[19];
	ceph.dn0d	= data[20];

	if	(sys==+E_Sys::GPS||sys==+E_Sys::QZS)
	{
		int week;
		ceph.toe = ceph.toc;
		ceph.toes = time2gpst(ceph.toe,&week);

		ceph.ura[0]	= data[21];  
		ceph.ura[1]	= data[22];  
		ceph.ura[2]	= data[26];  
		ceph.ura[3]	= data[23];  

		ceph.svh		= (E_Svh)data[24];	// sv health

		ceph.tgd[0]	= data[25];  	// TGD

		ceph.isc[0]	= data[27];  
		ceph.isc[1]	= data[28];  
		ceph.isc[2]	= data[29];  
		ceph.isc[3]	= data[30];  

		if (type==+E_NavMsgType::CNAV)
		{
			ceph.ttm = adjweek(gpst2time(week,data[31]),ceph.toc);
			ceph.wnop = (int)data[32];
		}
		else if (type==+E_NavMsgType::CNV2)
		{
			ceph.isc[4] = data[31];  
			ceph.isc[5] = data[32];  

			ceph.ttm = adjweek(gpst2time(week,data[35]),ceph.toc);
			ceph.wnop = (int)data[36];
		}

		ceph.tops = data[11];  	// top (s) in seconds
		ceph.top = adjweek(gpst2time(ceph.wnop,data[11]),ceph.toc);
	}
	else if (sys==+E_Sys::BDS)
	{
		// BeiDou v.4.00
		int week;
		time2bdt(ceph.toc,&week);

		ceph.toc = bdt2gpst(ceph.toc); // bdt -> gpst

		ceph.orb = E_SatType::_from_integral(data[21]);

		ceph.sis[0]	= data[23];  
		ceph.sis[1]	= data[24];  
		ceph.sis[2]	= data[25];  
		ceph.sis[3]	= data[26];  

		if  ( type==+E_NavMsgType::CNV1
			||type==+E_NavMsgType::CNV2)
		{
			ceph.isc[0]	= data[27];  
			ceph.isc[1]	= data[28];  

			ceph.tgd[0]	= data[29];  	// TGD_B1Cp
			ceph.tgd[1]	= data[30];  	// TGD_B2ap

			ceph.sis[4]	= data[31];  

			ceph.svh 	= (E_Svh)data[32];	// sv health
			ceph.flag	= (int)data[33];  	// integrity flag
			ceph.iodc	= (int)data[34];  	// IODC
			ceph.iode	= (int)data[37];  	// IODE

			ceph.ttm = bdt2gpst(bdt2time(week,data[35])); // bdt -> gpst
			ceph.ttm = adjweek(ceph.ttm,ceph.toc);
		}
		else if (type==+E_NavMsgType::CNV3)
		{
			ceph.sis[4]	= data[27];  
			ceph.svh 		= (E_Svh)data[28];	// sv health
			ceph.flag		= (int)data[29];  	// integrity flag
			ceph.tgd[2]	= data[30];  	// TGD_B2ap

			ceph.ttm = bdt2gpst(bdt2time(week,data[31])); // bdt -> gpst
			ceph.ttm = adjweek(ceph.ttm,ceph.toc);
		}

		ceph.toes	= data[11];  	// top (s) in seconds
		ceph.tops	= data[22];  	// top (s) in seconds
		ceph.toe	= bdt2gpst(bdt2time(week,data[11])); // bdt -> gpst
		ceph.top	= bdt2gpst(bdt2time(week,data[22])); // bdt -> gpst
		ceph.toe	= adjweek(ceph.toe,ceph.toc);
		ceph.top	= adjweek(ceph.top,ceph.toc);
	}

	if (ceph.iode<0||1023<ceph.iode)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iode=" << ceph.iode;
	}

	if (ceph.iodc<0||1023<ceph.iodc)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iodc=" << ceph.iodc;
	}

	return 1;
}

/** Decode STO message
*/
int decodeSto(
	double			ver,
	SatSys			Sat,
	E_NavMsgType	type,
	GTime			toc,
	double*			data,
	STO&			sto)
{
	if (ver < 4.0)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid RINEX version=" << ver;

		return -1;
	}

	int sys = Sat.sys;

	sto.Sat=Sat;
	sto.type=type;
	sto.tot=toc;

	sto.code=E_StoCode::_from_integral(data[0]);
	sto.sid =E_SbasId::_from_integral(data[1]);
	sto.uid =E_UtcId::_from_integral(data[2]);

	sto.A0=data[4];
	sto.A1=data[5];
	sto.A2=data[6];

	int week;
	if (sys != +E_Sys::BDS)	{time2gpst(sto.tot,&week);	sto.ttm=		  gpst2time(week,data[3]);									}
	else					{time2bdt (sto.tot,&week);	sto.ttm=bdt2gpst( bdt2time(week,data[3]));	sto.tot=bdt2gpst(sto.tot);	}

	return 1;
}

/** Decode EOP message
*/
int decodeEop(
	double			ver,
	SatSys			Sat,
	E_NavMsgType	type,
	GTime			toc,
	double*			data,
	EOP&			eop)
{
	if (ver < 4.0)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid RINEX version=" << ver;

		return -1;
	}

	int sys = Sat.sys;

	eop.Sat=Sat;
	eop.type=type;
	eop.teop=toc;

	eop.xp  =data[0] * AS2R;
	eop.xpr =data[1] * AS2R;
	eop.xprr=data[2] * AS2R;
	eop.yp  =data[4] * AS2R;
	eop.ypr =data[5] * AS2R;
	eop.yprr=data[6] * AS2R;
	eop.dut1=data[8];
	eop.dur =data[9];
	eop.durr=data[10];

	int week;
	if (sys != +E_Sys::BDS)	{time2gpst(eop.teop,&week);	eop.ttm=		  gpst2time(week,data[7]);									}
	else					{time2bdt (eop.teop,&week);	eop.ttm=bdt2gpst( bdt2time(week,data[7]));	eop.teop=bdt2gpst(eop.teop);	}

	return 1;
}

/** Decode ION message
*/
int decodeIon(
	double			ver,
	SatSys			Sat,
	E_NavMsgType	type,
	GTime			toc,
	double*			data,
	ION&			ion)
{
	if (ver < 4.0)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid RINEX version=" << ver;

		return -1;
	}

	int sys = Sat.sys;

	ion.Sat=Sat;
	ion.type=type;
	ion.ttm=toc;
	if (sys == +E_Sys::BDS)
		ion.ttm=bdt2gpst(ion.ttm);

	if	( sys==+E_Sys::GAL
		&&type==+E_NavMsgType::IFNV)
	{
		ion.ai0=data[0];
		ion.ai1=data[1];
		ion.ai2=data[2];

		ion.flag=(int)data[3];
	}
	else if	( sys==+E_Sys::BDS
			&&type==+E_NavMsgType::CNVX)
	{
		ion.alpha1=data[0];
		ion.alpha2=data[1];
		ion.alpha3=data[2];
		ion.alpha4=data[3];
		ion.alpha5=data[4];
		ion.alpha6=data[5];
		ion.alpha7=data[6];
		ion.alpha8=data[7];
		ion.alpha9=data[8];
	}
	else if	( type==+E_NavMsgType::LNAV
			||type==+E_NavMsgType::D1D2
			||type==+E_NavMsgType::CNVX)
	{
		ion.a0=data[0];
		ion.a1=data[1];
		ion.a2=data[2];
		ion.a3=data[3];
		ion.b0=data[4];
		ion.b1=data[5];
		ion.b2=data[6];
		ion.b3=data[7];

		ion.code=(int)data[8];

		if (ion.code == 1)	// QZS Japan area coefficients are currently skipped
			return 0;
	}

	return 1;
}

/** Read rinex navigation data body
*/
int readrnxnavb(
	std::istream& 	inputStream,	///< Input stream to read
	double		ver,				///< RINEX version
	E_Sys		sys,				///< Satellite system
	E_EphType&	type,				///< Ephemeris type (output)
	Eph&		eph,				///< GPS Ephemeris
	Geph&		geph,				///< Glonass ephemeris
	Seph&		seph,				///< Geo ephemeris
	Ceph&		ceph,				///< CNVX ephemeris
	STO&		sto,				///< System time offset data
	EOP&		eop,				///< EOP data
	ION&		ion)				///< Ionosphere data
{
	GTime toc;
	double data[64];
	int i=0,prn,sp=3;
	string	line;
	char id[8]="";
	char *p;


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxnavb: ver=" << ver << " sys=" << sys;

	SatSys Sat = {};
	E_NavRecType	recType	= E_NavRecType::NONE;
	E_NavMsgType	msgType	= E_NavMsgType::NONE;

	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];

		if (i == 0)
		{
			// decode message type field
			if	( ver >= 4.0
				&&buff[0] == '>')
			{
				// ver.4
				char typeStr[5] = "";
				strncpy(typeStr,buff+2,3);
				recType	= E_NavRecType::_from_string(typeStr);

				strncpy(id,buff+6,3);
				Sat=SatSys(id);
				sys=Sat.sys;

				strncpy(typeStr,buff+10,4);
				std::replace(typeStr, typeStr+4, ' ', '\0');
				msgType	= E_NavMsgType::_from_string(typeStr);

				continue;
			}

			// decode satellite field
			if	( ver >= 3.0
				||sys == +E_Sys::GAL
				||sys == +E_Sys::QZS)
			{
				// ver.3 or GAL/QZS
				strncpy(id,buff,3);
				sp = 4;
				if (ver < 4.0)	// satellite id included in message type field in ver.4
				{
					Sat=SatSys(id);
					if (ver >= 3.0)
						sys=Sat.sys;
				}
			}
			else
			{
				prn = (int)str2num(buff,0,2);
				Sat.sys = sys;
				Sat.prn = prn;
			}

			// decode toc field
			if (str2time(buff+sp,0,19,toc))
			{
//                 BOOST_LOG_TRIVIAL(debug)
// 				<< "rinex nav toc error: " << buff;

				return 0;
			}

			if (recType == +E_NavRecType::STO)
			{
				// decode STO code, SBAS ID & UTC ID for STO message
				char code[19] = "";
				strncpy(code,buff+24,18);
				std::replace(code, code+18, ' ', '\0');
				data[i++] = +E_StoCode::_from_string(code);

				strncpy(code,buff+43,18);
				std::replace(code, code+18, '-', '_' );
				std::replace(code, code+18, ' ', '\0');
				data[i++] = *(E_SbasId::_from_string_nothrow(code));	//code may be empty

				strncpy(code,buff+62,18);
				std::replace(code, code+18, '(', '_' );
				std::replace(code, code+18, ')', '\0');
				std::replace(code, code+18, ' ', '\0');
				data[i++] = *(E_UtcId::_from_string_nothrow(code));		//code may be empty
			}
			else
			{
				// decode data fields
				p = buff+sp+19;
				for (int j = 0; j < 3; j++, p += 19)
				{
					data[i++]=str2num(p,0,19);
				}
			}

			if (recType == +E_NavRecType::NONE)	recType = E_NavRecType::EPH;
			if (msgType == +E_NavMsgType::NONE)	msgType = defNavMsgType[sys];
		}
		else
		{
			// decode data fields
			p = buff+sp;
			for (int j = 0; j < 4; j++, p += 19)
			{
				data[i++]=str2num(p,0,19);
			}
			// decode ephemeris
			if		(recType == +E_NavRecType::EPH)
			{
				switch (msgType)
				{
					case E_NavMsgType::CNAV	: 
					case E_NavMsgType::CNV3	: if (i>=35)	{ type = E_EphType::CEPH;	return decodeCeph(ver,Sat,msgType,toc,data,ceph); }	break;
					case E_NavMsgType::CNV1	: 
					case E_NavMsgType::CNV2	: if (i>=39)	{ type = E_EphType::CEPH;	return decodeCeph(ver,Sat,msgType,toc,data,ceph); }	break;
					case E_NavMsgType::FDMA	: if (i>=15)	{ type = E_EphType::GEPH;	return decodeGeph(ver,Sat,        toc,data,geph); }	break;
					case E_NavMsgType::SBAS	: if (i>=15)	{ type = E_EphType::SEPH;	return decodeSeph(ver,Sat,        toc,data,seph); }	break;
					default					: if (i>=31)	{ type = E_EphType:: EPH;	return decodeEph (ver,Sat,        toc,data, eph); }	break;
				}
			}
			else if (recType == +E_NavRecType::STO)
			{
				if (i>= 7)									{ type = E_EphType:: STO;	return decodeSto (ver,Sat,msgType,toc,data, sto); }
			}
			else if (recType == +E_NavRecType::EOP)
			{
				if (i>=11)									{ type = E_EphType:: EOP;	return decodeEop (ver,Sat,msgType,toc,data, eop); }
			}
			else if (recType == +E_NavRecType::ION)
			{
				switch (sys)
				{
					case E_Sys::GAL	: if (i>= 7)			{ type = E_EphType:: ION;	return decodeIon (ver,Sat,msgType,toc,data, ion); }	break;
					default			: if (i>=11)			{ type = E_EphType:: ION;	return decodeIon (ver,Sat,msgType,toc,data, ion); }	break;
				}
			}
			else
				return -1;
		}
	}
	return -1;
}

/** Read rinex nav/gnav/geo nav
*/
int readrnxnav(
	std::istream& 	inputStream,	///< Input stream to read
	double			ver,			///< RINEX version
	E_Sys			sys,			///< Satellite system
	Navigation&		nav)			///< Navigation object
{
	Eph			eph		= {};
	Geph		geph	= {};
	Seph		seph	= {};
	Ceph		ceph	= {};
	STO			sto		= {};
	EOP			eop		= {};
	ION			ion		= {};
	int			stat	= {};
	E_EphType	type	= {};

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxnav: ver=" << ver << " sys=" << sys;

	// read rinex navigation data body
	while ((stat = readrnxnavb(inputStream, ver, sys, type, eph, geph, seph, ceph, sto, eop, ion)) >= 0)
	{
		// add ephemeris to navigation data
		if (stat)
		{
			switch (type)
			{
				case E_EphType::EPH:	nav.ephMap	[eph.Sat]					[eph.toe]	= eph;	break;
				case E_EphType::GEPH:	nav.gephMap	[geph.Sat]					[geph.toe]	= geph;	break;
				case E_EphType::SEPH:	nav.sephMap	[seph.Sat]					[seph.t0]	= seph;	break;
				case E_EphType::CEPH:	nav.cephMap	[ceph.Sat]		[ceph.type]	[ceph.toe]	= ceph;	break;
				case E_EphType::STO:	nav.stoMap	[sto.code]		[sto.type]	[sto.tot ]	= sto;	break;
				case E_EphType::EOP:	nav.eopMap	[eop.Sat.sys]	[eop.type]	[eop.teop]	= eop;	break;
				case E_EphType::ION:	nav.ionMap	[ion.Sat.sys]	[ion.type]	[ion.ttm ]	= ion;	break;
				default: continue;
			}
		}
	}
	return	( nav. ephMap.size() > 0
			||nav.gephMap.size() > 0
			||nav.sephMap.size() > 0
			||nav.cephMap.size() > 0
			||nav. stoMap.size() > 0
			||nav. eopMap.size() > 0
			||nav. ionMap.size() > 0);
}

/** Read rinex clock
*/
int readrnxclk(
	std::istream& 	inputStream,
	double			ver,
	Navigation&		nav)
{
//     trace(3,"readrnxclk: index=%d\n", index);

	static int index = 0;
	index++;
	string line;

	typedef struct 
	{
		short offset;
		short length;
	} ClkStruct;


	ClkStruct typ = {0,2};
	ClkStruct as  = {3,3};
	ClkStruct ar  = {3,4};
	ClkStruct tim = {8,26};
	ClkStruct clk = {40,19};
	ClkStruct std = {60,19};

	// special case for 3.04 rnx with 9 char AR names
	if (ver == 3.04)
	{
		ar.length  += 5;
		tim.offset += 5;
		clk.offset += 5;
		std.offset += 5;
	}

	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];

		GTime time;
		if (str2time(buff, tim.offset, tim.length, time))
		{
//             trace(2,"rinex clk invalid epoch: %34.34s\n", buff);
			continue;
		}

		string type(buff + typ.offset,typ.length);

		string idString;
		if		(type == "AS")	{	idString.assign(buff + as.offset, as.length);	}
		else if	(type == "AR")	{	idString.assign(buff + ar.offset, ar.length);	}
		else 						continue;

		Pclk preciseClock = {};

		preciseClock.clk	= str2num(buff, clk.offset, clk.length);
		preciseClock.std	= str2num(buff, std.offset, std.length);
		preciseClock.time	= time;
		preciseClock.index	= index;

		nav.pclkMap[idString].push_back(preciseClock);
	}

	return nav.pclkMap.size() > 0;
}

/** Read rinex file
*/
int readrnx(
	std::istream& 					inputStream,
	char&							type,
	ObsList&						obsList,
	Navigation&						nav,
	RinexStation*					sta,
	double&							ver,
	E_Sys&							sys,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes)
{
//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxfp: flag=" << flag
// 	<< " index=" << index;

	// read rinex header if at beginning of file
	if (inputStream.tellg() == 0)
	{
		bool pass = readrnxh(inputStream, ver, type, sys, tsys, sysCodeTypes, nav, sta);

		return pass;
	}

	// read rinex body
	switch (type)
	{
		case 'O': return readrnxobs(inputStream, ver, tsys, sysCodeTypes, obsList);
		case 'N': return readrnxnav(inputStream, ver, sys       ,	nav);
		case 'G': return readrnxnav(inputStream, ver, E_Sys::GLO, 	nav);
		case 'H': return readrnxnav(inputStream, ver, E_Sys::SBS, 	nav);
		case 'J': return readrnxnav(inputStream, ver, E_Sys::QZS, 	nav); // extension
		case 'L': return readrnxnav(inputStream, ver, E_Sys::GAL, 	nav); // extension
		case 'C': return readrnxclk(inputStream, ver,				nav);
	}

	BOOST_LOG_TRIVIAL(debug)
	<< "unsupported rinex type ver=" << ver << " type=" << type;

	return 0;
}

