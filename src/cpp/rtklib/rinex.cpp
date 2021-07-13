
#include <boost/log/trivial.hpp>


#include <string>

using std::string;



#include "streamTrace.hpp"
#include "navigation.hpp"
#include "station.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "rinex.hpp"

/* constants/macros ----------------------------------------------------------*/

#define MAXRNXLEN   (16*MAXOBSTYPE+4)   /* max rinex record length */
#define MAXPOSHEAD  1024                /* max head line position */
#define MINFREQ_GLO -7                  /* min frequency number glonass */
#define MAXFREQ_GLO 13                  /* max frequency number glonass */

const double ura_eph[]=
{
	/* ura values (ref [3] 20.3.3.3.1.1) */
	2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24, 48, 96, 192, 384, 768, 1536, 3072, 6144, 0
};

/* set string without tail space ---------------------------------------------*/
void setstr(char *dst, const char *src, int n)
{
	char *p=dst;
	const char *q=src;
	while (*q&&q<src+n) *p++=*q++;
	*p--='\0';
	while (p>=dst&&*p==' ') *p--='\0';
}

/* adjust time considering week handover -------------------------------------*/
GTime adjweek(GTime t, GTime t0)
{
	double tt = timediff(t, t0);
	if (tt < -302400)		return timeadd(t, +604800);
	if (tt > +302400)		return timeadd(t, -604800);
							return t;
}

/* adjust time considering week handover -------------------------------------*/
GTime adjday(GTime t, GTime t0)
{
	double tt = timediff(t, t0);
	if (tt < -43200.0)		return timeadd(t, +86400.0);
	if (tt > +43200.0)		return timeadd(t, -86400.0);
							return t;
}

/* ura value (m) to ura index ------------------------------------------------*/
int uraindex(double value)
{
	int i;
	for (i = 0; i < 15; i++)
		if (ura_eph[i] >= value)
			break;
	return i;
}

/*------------------------------------------------------------------------------
* input rinex functions
*-----------------------------------------------------------------------------*/

/* convert rinex obs type ver.2 -> ver.3 -------------------------------------*/
void convcode(
	double		ver,
	int			sys,
	const char*	str, 
	char*		type)
{
	strcpy(type,"   ");

	if      (!strcmp(str,"P1"))
	{
		/* ver.2.11 GPS L1PY,GLO L2P */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1W",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1P",'C');
	}
	else if (!strcmp(str,"P2"))
	{
		/* ver.2.11 GPS L2PY,GLO L2P */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2W",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2P",'C');
	}
	else if (!strcmp(str,"C1"))
	{
		/* ver.2.11 GPS L1C,GLO L1C/A */
		if      (ver>=2.12) ; /* reject C1 for 2.12 */
		else if (sys==+E_Sys::GPS) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",'C'); /* ver.2.12 */
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",'C');
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",'C');
	}
	else if (!strcmp(str,"C2"))
	{
		if (sys==+E_Sys::GPS)
		{
			if (ver>=2.12) sprintf(type,"%c2W",'C'); /* L2P(Y) */
			else           sprintf(type,"%c2X",'C'); /* L2C */
		}
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2C",'C');
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",'C');
		else if (sys==+E_Sys::CMP) sprintf(type,"%c1X",'C'); /* ver.2.12 B1 */
	}
	else if (ver>=2.12&&str[1]=='A')
	{
		/* ver.2.12 L1C/A */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",str[0]);
	}
	else if (ver>=2.12&&str[1]=='B')
	{
		/* ver.2.12 GPS L1C */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1X",str[0]);
	}
	else if (ver>=2.12&&str[1]=='C')
	{
		/* ver.2.12 GPS L2C */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2X",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",str[0]);
	}
	else if (ver>=2.12&&str[1]=='D')
	{
		/* ver.2.12 GLO L2C/A */
		if      (sys==+E_Sys::GLO) sprintf(type,"%c2C",str[0]);
	}
	else if (ver>=2.12&&str[1]=='1')
	{
		/* ver.2.12 GPS L1PY,GLO L1P */
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1W",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1P",str[0]);
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",str[0]); /* tentative */
		else if (sys==+E_Sys::CMP) sprintf(type,"%c1X",str[0]); /* extension */
	}
	else if (ver<2.12&&str[1]=='1')
	{
		if      (sys==+E_Sys::GPS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::GAL) sprintf(type,"%c1X",str[0]); /* tentative */
		else if (sys==+E_Sys::QZS) sprintf(type,"%c1C",str[0]);
		else if (sys==+E_Sys::SBS) sprintf(type,"%c1C",str[0]);
	}
	else if (str[1]=='2')
	{
		if      (sys==+E_Sys::GPS) sprintf(type,"%c2W",str[0]);
		else if (sys==+E_Sys::GLO) sprintf(type,"%c2P",str[0]);
		else if (sys==+E_Sys::QZS) sprintf(type,"%c2X",str[0]);
		else if (sys==+E_Sys::CMP) sprintf(type,"%c1X",str[0]); /* ver.2.12 B1 */
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
		else if (sys==+E_Sys::CMP) sprintf(type,"%c6X",str[0]); /* ver.2.12 B3 */
	}
	else if (str[1]=='7')
	{
		if      (sys==+E_Sys::GAL) sprintf(type,"%c7X",str[0]);
		else if (sys==+E_Sys::CMP) sprintf(type,"%c7X",str[0]); /* ver.2.12 B2 */
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


/* decode obs header ---------------------------------------------------------*/
void decode_obsh(
	std::istream& 					inputStream,
	string&							line,
	double							ver,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	nav_t&							nav,
	RinexStation*					sta)
{
	double del[3];
	int i,j,n,prn,fcn;
	const char *p;	
	char* buff	= &line[0];
	char* label	= buff+60;


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_obsh: ver=" << ver;

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
//     else if (strstr(label,"MARKER TYPE"         )) ; /* ver.3 */
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
			for (i=0,j=0;i<3;i++,j+=14)
				sta->pos[i] = str2num(buff,j,14);
		}
	}
	else if (strstr(label,"ANTENNA: DELTA H/E/N"))
	{
		if (sta)
		{
			for (i=0,j=0;i<3;i++,j+=14)
				del[i] = str2num(buff,j,14);

			sta->del[2] = del[0]; /* h */
			sta->del[0] = del[1]; /* e */
			sta->del[1] = del[2]; /* n */
		}
	}
//     else if (strstr(label,"ANTENNA: DELTA X/Y/Z")) ; /* opt ver.3 */
//     else if (strstr(label,"ANTENNA: PHASECENTER")) ; /* opt ver.3 */
//     else if (strstr(label,"ANTENNA: B.SIGHT XYZ")) ; /* opt ver.3 */
//     else if (strstr(label,"ANTENNA: ZERODIR AZI")) ; /* opt ver.3 */
//     else if (strstr(label,"ANTENNA: ZERODIR XYZ")) ; /* opt ver.3 */
//     else if (strstr(label,"CENTER OF MASS: XYZ" )) ; /* opt ver.3 */
	else if (strstr(label,"SYS / # / OBS TYPES" ))
	{
		/* ver.3 */
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
			if	( (Sat.sys == +E_Sys::CMP)
				&&(code[1] == '2'))
			{
				/* change beidou B1 code: 3.02 draft -> 3.02 */
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

		/* if unknown code in ver.3, set default code */
// 		for (auto& codeType : sysCodeTypes[Sat.sys])
// 		{
//             if (tobs[i][j][2])
// 				continue;
//
//             if (!(p = strchr(frqcodes, tobs[i][j][1])))
// 				continue;
//
				/* default codes for unknown code */
//     			const char *defcodes[] =
//    			{
//         			"CWX   ",   /* GPS: L125___ */
//         			"CC    ",   /* GLO: L12____ */
//         			"X XXXX",   /* GAL: L1_5678 */
//         			"CXXX  ",   /* QZS: L1256__ */
//         			"C X   ",   /* SBS: L1_5___ */
//         			"X  XX "    /* BDS: L1__67_ */
//     			};
//             	tobs[i][j][2] = defcodes[i][(int)(p - frqcodes)];
//
//             	BOOST_LOG_TRIVIAL(debug)
// 				<< "set default for unknown code: sys=" << buff[0]
// 				<< " code=" << tobs[i][j];
//         }
	}
//     else if (strstr(label,"WAVELENGTH FACT L1/2")) ; /* opt ver.2 */
//     else if (strstr(label,"# / TYPES OF OBSERV" ))
// 	{ /* ver.2 */
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
//                 convcode(ver,E_Sys::CMP,str,tobs[5][nt]);
//             }
//             nt++;
//         }
//         *tobs[0][nt]='\0';
//     }
//     else if (strstr(label,"SIGNAL STRENGTH UNIT")) ; /* opt ver.3 */
//     else if (strstr(label,"INTERVAL"            )) ; /* opt */
	else if (strstr(label,"TIME OF FIRST OBS"   ))
	{
		if      (!strncmp(buff+48,"GPS",3)) tsys=TSYS_GPS;
		else if (!strncmp(buff+48,"GLO",3)) tsys=TSYS_UTC;
		else if (!strncmp(buff+48,"GAL",3)) tsys=TSYS_GAL;
		else if (!strncmp(buff+48,"QZS",3)) tsys=TSYS_QZS; /* ver.3.02 */
		else if (!strncmp(buff+48,"BDT",3)) tsys=TSYS_CMP; /* ver.3.02 */
	}
//     else if (strstr(label,"TIME OF LAST OBS"    )) ; /* opt */
//     else if (strstr(label,"RCV CLOCK OFFS APPL" )) ; /* opt */
//     else if (strstr(label,"SYS / DCBS APPLIED"  )) ; /* opt ver.3 */
//     else if (strstr(label,"SYS / PCVS APPLIED"  )) ; /* opt ver.3 */
//     else if (strstr(label,"SYS / SCALE FACTOR"  )) ; /* opt ver.3 */
//     else if (strstr(label,"SYS / PHASE SHIFTS"  )) ; /* ver.3.01 */
	else if (strstr(label,"GLONASS SLOT / FRQ #"))
	{
		/* ver.3.02 */
		for (i=0,p=buff+4;i<8;i++,p+=8)
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
		/* ver.3.02 */
		for (i=0,p=buff;i<4;i++,p+=13)
		{
			if      (strncmp(p+1,"C1C",3)) nav.glo_cpbias[0]=str2num(p,5,8);
			else if (strncmp(p+1,"C1P",3)) nav.glo_cpbias[1]=str2num(p,5,8);
			else if (strncmp(p+1,"C2C",3)) nav.glo_cpbias[2]=str2num(p,5,8);
			else if (strncmp(p+1,"C2P",3)) nav.glo_cpbias[3]=str2num(p,5,8);
		}
	}
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		/* opt */
		nav.leaps=(int)str2num(buff,0,6);
	}
//     else if (strstr(label,"# OF SALTELLITES"    )) ; /* opt */
//     else if (strstr(label,"PRN / # OF OBS"      )) ; /* opt */
}

/* decode nav header ---------------------------------------------------------*/
void decode_navh(
	string&	line,
	nav_t&	nav)
{
	int i,j;
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decode_navh:";

	if      (strstr(label,"ION ALPHA"           ))	{ /* opt ver.2 */  for (i=0,j=2;i<4;i++,j+=12) nav.ion_gps[i]	= str2num(buff,j,12);  }
	else if (strstr(label,"ION BETA"            ))	{ /* opt ver.2 */  for (i=0,j=2;i<4;i++,j+=12) nav.ion_gps[i+4]	= str2num(buff,j,12);  }
	else if (strstr(label,"DELTA-UTC: A0,A1,T,W"))
	{
		/* opt ver.2 */
		for (i=0,j=3;i<2;i++,j+=19)	nav.utc_gps[i]=str2num(buff,j,19);
		for (;i<4;i++,j+=9)			nav.utc_gps[i]=str2num(buff,j,9);
	}
	else if (strstr(label,"IONOSPHERIC CORR"    ))
	{
		/* opt ver.3 */
		if		(!strncmp(buff,"GPSA",4))		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_gps[i]		= str2num(buff,j,12); }
		else if (!strncmp(buff,"GPSB",4)) 		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_gps[i+4]	= str2num(buff,j,12); }
		else if (!strncmp(buff,"GAL" ,3))		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_gal[i]		= str2num(buff,j,12); }
		else if (!strncmp(buff,"QZSA",4)) 		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_qzs[i]		= str2num(buff,j,12); }
		else if (!strncmp(buff,"QZSB",4))		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_qzs[i+4]	= str2num(buff,j,12); }
		else if (!strncmp(buff,"BDSA",4)) 		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_cmp[i]		= str2num(buff,j,12); }
		else if (!strncmp(buff,"BDSB",4))		{ for (i=0,j=5; i<4; i++,j+=12) nav.ion_cmp[i+4]	= str2num(buff,j,12); }
	}
	else if (strstr(label,"TIME SYSTEM CORR"    ))
	{
		/* opt ver.3 */
		if (!strncmp(buff,"GPUT",4))
		{
			nav.utc_gps[0] = str2num(buff, 5,17);
			nav.utc_gps[1] = str2num(buff,22,16);
			nav.utc_gps[2] = str2num(buff,38, 7);
			nav.utc_gps[3] = str2num(buff,45, 5);
		}
		else if (!strncmp(buff,"GLUT",4))
		{
			nav.utc_glo[0] = str2num(buff, 5,17);
			nav.utc_glo[1] = str2num(buff,22,16);
		}
		else if (!strncmp(buff,"GAUT",4))
		{
			/* v.3.02 */
			nav.utc_gal[0] = str2num(buff, 5,17);
			nav.utc_gal[1] = str2num(buff,22,16);
			nav.utc_gal[2] = str2num(buff,38, 7);
			nav.utc_gal[3] = str2num(buff,45, 5);
		}
		else if (!strncmp(buff,"QZUT",4))
		{
			/* v.3.02 */
			nav.utc_qzs[0] = str2num(buff, 5,17);
			nav.utc_qzs[1] = str2num(buff,22,16);
			nav.utc_qzs[2] = str2num(buff,38, 7);
			nav.utc_qzs[3] = str2num(buff,45, 5);
		}
		else if (!strncmp(buff,"BDUT",4))
		{
			/* v.3.02 */
			nav.utc_cmp[0] = str2num(buff, 5,17);
			nav.utc_cmp[1] = str2num(buff,22,16);
			nav.utc_cmp[2] = str2num(buff,38, 7);
			nav.utc_cmp[3] = str2num(buff,45, 5);
		}
		else if (!strncmp(buff,"SBUT",4))
		{
			/* v.3.02 */
			nav.utc_cmp[0] = str2num(buff, 5,17);
			nav.utc_cmp[1] = str2num(buff,22,16);
			nav.utc_cmp[2] = str2num(buff,38, 7);
			nav.utc_cmp[3] = str2num(buff,45, 5);
		}
	}
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		/* opt */
		nav.leaps=(int)str2num(buff,0,6);
	}
}
/* decode gnav header --------------------------------------------------------*/
void decode_gnavh(
	string&	line,
	nav_t&	nav)
{	
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decode_gnavh:";

	if      (strstr(label,"CORR TO SYTEM TIME"  )) ; /* opt */
	else if (strstr(label,"LEAP SECONDS"        ))
	{
		/* opt */
		nav.leaps=(int)str2num(buff,0,6);
	}
}

/* decode geo nav header -----------------------------------------------------*/
void decode_hnavh(
	string&	line,
	nav_t&	nav)
{
	char*	buff	= &line[0];
	char*	label	= buff + 60;

	BOOST_LOG_TRIVIAL(debug)
	<< "decode_hnavh:";

	if      (strstr(label, "CORR TO SYTEM TIME"  )) ; /* opt */
	else if (strstr(label, "D-UTC A0,A1,T,W,S,U" )) ; /* opt */
	else if (strstr(label, "LEAP SECONDS"        ))
	{
		/* opt */
		nav.leaps= (int)str2num(buff,0,6);
	}
}

/* read rinex header ---------------------------------------------------------*/
int readrnxh(
	std::istream& 					inputStream,
	double&							ver,
	char&							type,
	E_Sys&							sys,
	int&							tsys,
	map<E_Sys, vector<CodeType>>&	sysCodeTypes,
	nav_t&							nav,
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

	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];
		char*	label	= buff + 60;

		if (line.length() <= 60)
			continue;
		else if (strstr(label,"RINEX VERSION / TYPE"))
		{
			ver		= str2num(buff,0,9);
			type	= *(buff+20);

			char sysChar = buff[40];

			/* satellite system */
			switch (sysChar)
			{
				case ' ':
				case 'G': sys = E_Sys::GPS;  tsys = TSYS_GPS; break;
				case 'R': sys = E_Sys::GLO;  tsys = TSYS_UTC; break;
				case 'E': sys = E_Sys::GAL;  tsys = TSYS_GAL; break; /* v.2.12 */
				case 'S': sys = E_Sys::SBS;  tsys = TSYS_GPS; break;
				case 'J': sys = E_Sys::QZS;  tsys = TSYS_QZS; break; /* v.3.02 */
				case 'C': sys = E_Sys::CMP;  tsys = TSYS_CMP; break; /* v.2.12 */
				case 'M': sys = E_Sys::NONE; tsys = TSYS_GPS; break; /* mixed */
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
		{ /* opt */

			/* read cnes wl satellite fractional bias */
			if	( strstr(buff,"WIDELANE SATELLITE FRACTIONAL BIASES")
				||strstr(buff,"WIDELANE SATELLITE FRACTIONNAL BIASES"))
			{
				block=1;
			}
			else if (block)
			{
				double bias;
				SatSys Sat;

				/* cnes/cls grg clock */
				if (!strncmp(buff,"WL",2)&&(Sat=SatSys(buff+3), Sat)&&
					sscanf(buff+40,"%lf",&bias)==1)
				{
					nav.satNavMap[Sat].wlbias = bias;
				}
				/* cnes ppp-wizard clock */
				else if ((Sat=SatSys(buff+1), Sat)&&sscanf(buff+6,"%lf",&bias)==1)
				{
					nav.satNavMap[Sat].wlbias = bias;
				}
			}
			continue;
		}
		/* file type */
		switch (type)
		{            
			case 'O': decode_obsh(inputStream, line, ver, tsys, sysCodeTypes, nav, sta); break;
			case 'N': decode_navh			(  line,nav); break;
			case 'G': decode_gnavh			(  line,nav); break;
			case 'H': decode_hnavh			(  line,nav); break;
			case 'J': decode_navh 			(  line,nav); break; /* extension */
			case 'L': decode_navh 			(  line,nav); break; /* extension */
		}
		if (strstr(label,"END OF HEADER"))
			return 1;

		if (++i>=MAXPOSHEAD
			&&type==' ')
		{
			break; /* no rinex file */
		}
	}
	return 0;
}
/* decode obs epoch ----------------------------------------------------------*/
int decode_obsepoch(
	std::istream& 		inputStream,
	string&				line,
	double				ver,
	GTime*				time,
	int&				flag,
	vector<SatSys>&		sats)

{
	int n;
	char*	buff	= &line[0]; 


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_obsepoch: ver=" << ver;

	if	(ver <= 2.99)
	{
		/* ver.2 */
		n		= (int) str2num(buff, 29, 3);
		if (n <= 0)
			return 0;

		/* epoch flag: 3:new site,4:header info,5:external event */
		flag	= (int) str2num(buff, 28, 1);

		if	( flag >= 3
			&&flag <= 5)
		{
			return n;
		}

		if (str2time(buff, 0, 26, *time))
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "rinex obs invalid epoch: epoch=" << buff;
			return 0;
		}

		int j = 32;
		for (int i = 0; i < n; i++, j+=3)
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
		/* ver.3 */
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
			||str2time(buff, 1, 28, *time))
		{
			BOOST_LOG_TRIVIAL(debug)
			<< "rinex obs invalid epoch: epoch=" << buff;
			return 0;
		}
	}

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_obsepoch: time=" << time->to_string(3)
// 	<< " flag=" << flag;

	return n;
}

/* decode obs data -----------------------------------------------------------*/
int decode_obsdata(
	std::istream& 					inputStream,
	string&		line,
	double		ver,
	map<E_Sys, vector<CodeType>>& sysCodeTypes,
	RawObs&		obs,
	SatSys*		v2SatSys_ptr)
{
	char		satid[8]	= "";
	int			stat		= 1;
	char*		buff		= &line[0];


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_obsdata: ver=" << ver;

	if (ver > 2.99)
	{
		/* ver.3 */
		strncpy(satid, buff, 3);
		obs.Sat = SatSys(satid);
	}
	else
	{
		obs.Sat = *v2SatSys_ptr;
	}

	if (!obs.Sat)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "decode_obsdata: unsupported sat sat=" << satid;

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
// 			/* ver.2 */
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
// 	<< "decode_obsdata: time=" << obs.time.to_string(0)
// 	<< " sat=" << obs.Sat.id();

	return 1;
}

/* read rinex obs data body --------------------------------------------------*/
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

	/* read record */
	while (std::getline(inputStream, line))
	{
		/* decode obs epoch */
		if (i == 0)
		{
			nSats = decode_obsepoch(inputStream, line, ver, &time, flag, sats);
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

			/* decode obs data */
			bool pass = decode_obsdata(inputStream, line, ver, sysCodeTypes, rawObs, &sats[i]);
			if	(pass)
			{
				/* save obs data */
				obsList.push_back(rawObs);
			}
		}
		
		if (++i > nSats)
			return obsList.size();
	}
	
	return -1;
}

/* read rinex obs ------------------------------------------------------------*/
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

	/* read rinex obs data body */
	int n = readrnxobsb(inputStream, ver, sysCodeTypes, flag, obsList);

	if	( n >= 0
		&&stat >= 0)
	{
		for (auto& obs : obsList)
		{
			/* utc -> gpst */
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

/* decode ephemeris ----------------------------------------------------------*/
int decode_eph(
	double ver,
	SatSys Sat,
	GTime toc,
	double *data,
	Eph *eph)
{
	Eph eph0 = {};

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_eph: ver=" << ver << " sat=" << Sat.id();

	int sys = Sat.sys;

	if	( sys != +E_Sys::GPS
		&&sys != +E_Sys::GAL
		&&sys != +E_Sys::QZS
		&&sys != +E_Sys::CMP)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}
	*eph=eph0;

	eph->Sat=Sat;
	eph->toc=toc;

	eph->f0=data[0];
	eph->f1=data[1];
	eph->f2=data[2];

	eph->A=SQR(data[10]); eph->e=data[ 8]; eph->i0  =data[15]; eph->OMG0=data[13];
	eph->omg =data[17]; eph->M0 =data[ 6]; eph->deln=data[ 5]; eph->OMGd=data[18];
	eph->idot=data[19]; eph->crc=data[16]; eph->crs =data[ 4]; eph->cuc =data[ 7];
	eph->cus =data[ 9]; eph->cic=data[12]; eph->cis =data[14];

	if (sys==+E_Sys::GPS||sys==+E_Sys::QZS)
	{
		eph->iode=(int)data[ 3];      /* IODE */
		eph->iodc=(int)data[26];      /* IODC */
		eph->toes=     data[11];      /* toe (s) in gps week */
		eph->week=(int)data[21];      /* gps week */
		eph->toe=adjweek(gpst2time(eph->week,data[11]),toc);
		eph->ttr=adjweek(gpst2time(eph->week,data[27]),toc);

		eph->code=(int)data[20];      /* GPS: codes on L2 ch */
		eph->svh =(int)data[24];      /* sv health */
		eph->sva=uraindex(data[23]);  /* ura (m->index) */
		eph->flag=(int)data[22];      /* GPS: L2 P data flag */

		eph->tgd[0]=   data[25];      /* TGD */
		eph->fit   =   data[28];      /* fit interval */
	}
	else if (sys==+E_Sys::GAL)
	{
		/* GAL ver.3 */
		eph->iode=(int)data[ 3];      /* IODnav */
		eph->toes=     data[11];      /* toe (s) in galileo week */
		eph->week=(int)data[21];      /* gal week = gps week */
		eph->toe=adjweek(gpst2time(eph->week,data[11]),toc);
		eph->ttr=adjweek(gpst2time(eph->week,data[27]),toc);

		eph->code=(int)data[20];      /* data sources */
									/* bit 0 set: I/NAV E1-B */
									/* bit 1 set: F/NAV E5a-I */
									/* bit 2 set: F/NAV E5b-I */
									/* bit 8 set: af0-af2 toc are for E5a.E1 */
									/* bit 9 set: af0-af2 toc are for E5b.E1 */
		eph->svh =(int)data[24];      /* sv health */
									/* bit     0: E1B DVS */
									/* bit   1-2: E1B HS */
									/* bit     3: E5a DVS */
									/* bit   4-5: E5a HS */
									/* bit     6: E5b DVS */
									/* bit   7-8: E5b HS */
		eph->sva =uraindex(data[23]); /* ura (m->index) */

		eph->tgd[0]=   data[25];      /* BGD E5a/E1 */
		eph->tgd[1]=   data[26];      /* BGD E5b/E1 */
	}
	else if (sys==+E_Sys::CMP)
	{
		/* BeiDou v.3.02 */
		eph->toc=bdt2gpst(eph->toc);  /* bdt -> gpst */
		eph->iode=(int)data[ 3];      /* AODE */
		eph->iodc=(int)data[28];      /* AODC */
		eph->toes=     data[11];      /* toe (s) in bdt week */
		eph->week=(int)data[21];      /* bdt week */
		eph->toe=bdt2gpst(bdt2time(eph->week,data[11])); /* bdt -> gpst */
		eph->ttr=bdt2gpst(bdt2time(eph->week,data[27])); /* bdt -> gpst */
		eph->toe=adjweek(eph->toe,toc);
		eph->ttr=adjweek(eph->ttr,toc);

		eph->svh =(int)data[24];      /* satH1 */
		eph->sva=uraindex(data[23]);  /* ura (m->index) */

		eph->tgd[0]=   data[25];      /* TGD1 B1/B3 */
		eph->tgd[1]=   data[26];      /* TGD2 B2/B3 */
	}

	if (eph->iode<0||1023<eph->iode)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iode=" << eph->iode;
	}

	if (eph->iodc<0||1023<eph->iodc)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex nav invalid: sat=" << Sat.id() << " iodc=" << eph->iodc;
	}
	return 1;
}

/* decode glonass ephemeris --------------------------------------------------*/
int decode_geph(double ver, SatSys Sat, GTime toc, double *data,
					Geph *geph)
{
	Geph geph0={};
	GTime tof;
	double tow,tod;
	int week,dow;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_geph: ver=" << ver << " sat=" << Sat.id();

	if (Sat.sys!=+E_Sys::GLO)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "glonass ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}
	*geph=geph0;

	geph->Sat=Sat;

	/* toc rounded by 15 min in utc */
	tow=time2gpst(toc,&week);
	toc=gpst2time(week,floor((tow+450.0)/900.0)*900);
	dow=(int)floor(tow/86400.0);

	/* time of frame in utc */
	tod=ver<=2.99?data[2]:fmod(data[2],86400.0); /* tod (v.2), tow (v.3) in utc */
	tof=gpst2time(week,tod+dow*86400.0);
	tof=adjday(tof,toc);

	geph->toe=utc2gpst(toc);   /* toc (gpst) */
	geph->tof=utc2gpst(tof);   /* tof (gpst) */

	/* iode = tb (7bit), tb =index of UTC+3H within current day */
	geph->iode=(int)(fmod(tow+10800.0,86400.0)/900.0+0.5);

	geph->taun=-data[0];       /* -taun */
	geph->gamn= data[1];       /* +gamman */

	geph->pos[0]=data[3]*1E3; geph->pos[1]=data[7]*1E3; geph->pos[2]=data[11]*1E3;
	geph->vel[0]=data[4]*1E3; geph->vel[1]=data[8]*1E3; geph->vel[2]=data[12]*1E3;
	geph->acc[0]=data[5]*1E3; geph->acc[1]=data[9]*1E3; geph->acc[2]=data[13]*1E3;

	geph->svh=(int)data[ 6];
	geph->frq=(int)data[10];
	geph->age=(int)data[14];

	/* some receiver output >128 for minus frequency number */
	if (geph->frq >  128)
		geph->frq -= 256;

	if (geph->frq<MINFREQ_GLO||MAXFREQ_GLO<geph->frq)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "rinex gnav invalid freq: sat=" << Sat << " fn=" << geph->frq;
	}
	return 1;
}
/* decode geo ephemeris ------------------------------------------------------*/
int decode_seph(double ver, SatSys Sat, GTime toc, double *data,
					Seph *seph)
{
	Seph seph0 = {};
	int week;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "decode_seph: ver=" << ver << " sat=" << Sat.id();

	if (Sat.sys!=+E_Sys::SBS)
	{
		BOOST_LOG_TRIVIAL(debug)
		<< "geo ephemeris error: invalid satellite sat=" << Sat.id();

		return 0;
	}
	*seph=seph0;

	seph->Sat=Sat;
	seph->t0 =toc;

	time2gpst(toc,&week);
	seph->tof=adjweek(gpst2time(week,data[2]),toc);

	seph->af0=data[0];
	seph->af1=data[1];

	seph->pos[0]=data[3]*1E3; seph->pos[1]=data[7]*1E3; seph->pos[2]=data[11]*1E3;
	seph->vel[0]=data[4]*1E3; seph->vel[1]=data[8]*1E3; seph->vel[2]=data[12]*1E3;
	seph->acc[0]=data[5]*1E3; seph->acc[1]=data[9]*1E3; seph->acc[2]=data[13]*1E3;

	seph->svh=(int)data[6];
	seph->sva=uraindex(data[10]);

	return 1;
}

/* read rinex navigation data body -------------------------------------------*/
int readrnxnavb(
	std::istream& 	inputStream,
	double		ver,
	E_Sys		sys,
	int&		type,
	Eph*		eph,
	Geph*		geph,
	Seph*		seph)
{
	GTime toc;
	double data[64];
	int i=0,j,prn,sp=3;
	string	line;
	char id[8]="";
	char *p;


//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxnavb: ver=" << ver << " sys=" << sys;

	SatSys Sat = {};
	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];

		if (i==0)
		{
			/* decode satellite field */
			if (ver>=3.0||sys==+E_Sys::GAL||sys==+E_Sys::QZS)
			{
				/* ver.3 or GAL/QZS */
				strncpy(id,buff,3);
				Sat=SatSys(id);
				sp=4;
				if (ver>=3.0) sys=Sat.sys;
			}
			else
			{
				prn=(int)str2num(buff,0,2);
				Sat.sys = sys;
				Sat.prn = prn;
			}
			/* decode toc field */
			if (str2time(buff+sp,0,19,toc))
			{
//                 BOOST_LOG_TRIVIAL(debug)
// 				<< "rinex nav toc error: " << buff;

				return 0;
			}
			/* decode data fields */
			for (j=0,p=buff+sp+19;j<3;j++,p+=19)
			{
				data[i++]=str2num(p,0,19);
			}
		}
		else
		{
			/* decode data fields */
			for (j=0,p=buff+sp;j<4;j++,p+=19)
			{
				data[i++]=str2num(p,0,19);
			}
			/* decode ephemeris */
			if		(sys==+E_Sys::GLO&&i>=15)	{ type = 1;	return decode_geph(ver,Sat,toc,data,geph);  }
			else if	(sys==+E_Sys::SBS&&i>=15) 	{ type = 2;	return decode_seph(ver,Sat,toc,data,seph);  }
			else if	(i>=31)						{ type = 0;	return decode_eph(ver,Sat,toc,data,eph);    }
		}
	}
	return -1;
}

/* read rinex nav/gnav/geo nav -----------------------------------------------*/
int readrnxnav(
	std::istream& 	inputStream,
	double			ver,
	E_Sys			sys,
	nav_t&			nav)
{
	Eph		eph;
	Geph	geph;
	Seph	seph;
	int stat;
	int type;

//     BOOST_LOG_TRIVIAL(debug)
// 	<< "readrnxnav: ver=" << ver << " sys=" << sys;

	/* read rinex navigation data body */
	while ((stat = readrnxnavb(inputStream, ver, sys, type, &eph, &geph, &seph)) >= 0)
	{
		/* add ephemeris to navigation data */
		if (stat)
		{
			switch (type)
			{
				case 1 : nav.gephMap[geph.Sat].push_back(geph);	break;
				case 2 : nav.sephMap[seph.Sat].push_back(seph);	break;
				default: nav.ephMap [eph.Sat] .push_back(eph);	break;
			}
		}
	}
	return	( nav.ephMap .size() > 0
			||nav.gephMap.size() > 0
			||nav.sephMap.size() > 0);
}

/* read rinex clock ----------------------------------------------------------*/
int readrnxclk(
	std::istream& 	inputStream,
	nav_t&			nav)
{
//     trace(3,"readrnxclk: index=%d\n", index);

	static int index = 0;
	index++;
	string line;

	while (std::getline(inputStream, line))
	{
		char*	buff	= &line[0];

		GTime time;
		if (str2time(buff,8,26,time))
		{
//             trace(2,"rinex clk invalid epoch: %34.34s\n", buff);
			continue;
		}

		string type(buff,2);

		string idString;
		if		(type == "AS")	{	idString.assign(buff + 3, 3);	}
		else if	(type == "AR")	{	idString.assign(buff + 3, 4);	}
		else 						continue;

		Pclk preciseClock = {};

		preciseClock.clk	= str2num(buff, 40, 19);
		preciseClock.std	= str2num(buff, 60, 19);
		preciseClock.time	= time;
		preciseClock.index	= index;

		nav.pclkMap[idString].push_back(preciseClock);
	}

	return nav.pclkMap.size() > 0;
}

/* read rinex file -----------------------------------------------------------*/
int readrnx(
	std::istream& 					inputStream,
	char&							type,
	ObsList&						obsList,
	nav_t&							nav,
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

	/* read rinex body */
	switch (type)
	{
		case 'O': return readrnxobs(inputStream, ver, tsys, sysCodeTypes, obsList);
		case 'N': return readrnxnav(inputStream, ver, sys      , 	nav);
		case 'G': return readrnxnav(inputStream, ver, E_Sys::GLO, 	nav);
		case 'H': return readrnxnav(inputStream, ver, E_Sys::SBS, 	nav);
		case 'J': return readrnxnav(inputStream, ver, E_Sys::QZS, 	nav); /* extension */
		case 'L': return readrnxnav(inputStream, ver, E_Sys::GAL, 	nav); /* extension */
		case 'C': return readrnxclk(inputStream,					nav);
	}

	BOOST_LOG_TRIVIAL(debug)
	<< "unsupported rinex type ver=" << ver << " type=" << type;

	return 0;
}

