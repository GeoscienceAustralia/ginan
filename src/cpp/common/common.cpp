
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include "common.hpp"
#include "constants.hpp"

double svaToUra(int sva)
{
	/*
		GLOBAL POSITIONING SYSTEM
		STANDARD POSITIONING SERVICE
		SIGNAL SPECIFICATION
		2nd Ed, June 2,1995
		see section - 2.5.3 User Range Accuracy
	*/
	double ura = 0;
	if (sva <= 6)
	{
		ura = 10 * pow(2, 1 + ((double)sva / 2.0));
		ura = round(ura) / 10.0;
	}
	else if (sva != 15)
		ura = pow(2, (double)sva - 2.0);
	else
		ura = -1;
	return ura;
}



double svaToSisa(int sva)
{
	/*
		EUROPEAN GNSS (GALILEO) OPEN SERVICE
		SIGNAL-IN-SPACE
		INTERFACE CONTROL
		DOCUMENT
		Issue 2.0, January 2021
		See Section, 5.1.12. Signal In Space Accuracy (SISA)
	*/

	double sisa;
	if (sva <= 49)			sisa = 0.0 + (sva - 0) * 0.01;
	else if (sva <= 74)		sisa = 0.5 + (sva - 50) * 0.02;
	else if (sva <= 99)		sisa = 1.0 + (sva - 75) * 0.04;
	else if (sva <= 125)	sisa = 2.0 + (sva - 100) * 0.16;
	else					sisa = -1;
	return sisa;
}

int sisaToSva(double sisa)
{
	if (sisa < 0)
	{
		BOOST_LOG_TRIVIAL(error) << "Error converting SISA to SVA, value is less than zero.";
		return -1;
	}

	if (sisa <= 0.49)	    return (int)((((sisa - 0.0) / 0.01) + 0) + 0.5);
	else if (sisa <= 0.98)  return (int)((((sisa - 0.5) / 0.02) + 50) + 0.5);
	else if (sisa <= 1.96)  return (int)((((sisa - 1.0) / 0.04) + 75) + 0.5);
	else if (sisa <= 6.00)  return (int)((((sisa - 2.0) / 0.16) + 100) + 0.5);
	else
	{
		BOOST_LOG_TRIVIAL(warning) << "SISA is too large SVA undefined.";
		return -1;
	}
}

/* crc-24q parity --------------------------------------------------------------
* compute crc-24q parity for sbas, rtcm3
* args   : unsigned char *buff I data
*          int    len    I      data length (bytes)
* return : crc-24Q parity
* notes  : see reference [2] A.4.3.3 Parity
*-----------------------------------------------------------------------------*/
unsigned int crc24q(
	const unsigned char *buff, 
	int len)
{
//	trace(4,"%s: len=%d\n",__FUNCTION__, len);
	
	unsigned int crc = 0;

	for (int i=0;i<len;i++) 
		crc = ((crc<<8) & 0xFFFFFF) ^ tbl_CRC24Q[(crc >> 16) ^ buff[i]];
	
	return crc;
}

void setbitu(
	unsigned char*	buff,
	int 			pos,
	int				len,
	unsigned int	data)
{
	unsigned int mask=1u<<(len-1);
	
	if	( len<=0
		||len>32)
	{
		return;
	}
	
	unsigned long int invalid = (1ul<<len);
	
	if (data >= invalid)
	{
		std::cout << "Warning: " << __FUNCTION__ << " has data outside range\n";
	}
	
	for (int i = pos; i < pos + len; i++, mask >>= 1) 
	{
		if (data&mask)	buff[i/8] |=  (1u<<(7-i%8));
		else			buff[i/8] &= ~(1u<<(7-i%8));
	}
}

void setbits(
	unsigned char*	buff, 
	int				pos, 
	int				len, 
	int				data)
{
	unsigned int mask=1u<<(len-1);
	
	if	( len<=0
		||len>32)
	{
		return;
	}
	
	long int invalid = (1ul<<(len-1));
	
	if	( +data >= invalid
		||-data >= invalid)
	{
		std::cout << "Warning: " << __FUNCTION__ << " has data outside range, setting invalid\n";
		data = -invalid;
	}
	
	for (int i = pos; i < pos + len; i++, mask >>= 1) 
	{
		if (data&mask)	buff[i/8] |=  (1u<<(7-i%8));
		else			buff[i/8] &= ~(1u<<(7-i%8));
	}
}

int setbituInc(
	unsigned char*	buff,
	int				pos,
	int				len,
	unsigned int	var)
{   
	setbitu(buff, pos, len, var);
	return pos + len;
}

int setbitsInc(
	unsigned char*	buff,
	int				pos,
	int				len,
	int				var)
{
	setbits(buff, pos, len, var);
	return pos + len;
}

/* extract unsigned/signed bits ------------------------------------------------
* extract unsigned/signed bits from byte data
* args   : unsigned char *buff I byte data
*          int    pos    I      bit position from start of data (bits)
*          int    len    I      bit length (bits) (len<=32)
* return : extracted unsigned/signed bits
*-----------------------------------------------------------------------------*/
unsigned int getbitu(
	const unsigned char*	buff,
	int						pos,
	int						len)
{
	unsigned int bits = 0;
	for (int i=pos;i<pos+len;i++)
		bits=(bits<<1)+((buff[i/8]>>(7-i%8))&1u);
	
	return bits;
}

int getbits(
	const unsigned char*	buff,
	int						pos,
	int						len)
{
	unsigned int bits = getbitu(buff, pos, len);
	
	
	long int invalid = (1ul<<(len-1));
	
	if (bits == -invalid)
	{
		std::cout << "warning: invalid number received on " << __FUNCTION__ << " " << invalid << " " << len << std::endl;
	}
	
	if	( len<=0
		||len>=32
		||!(bits&(1u<<(len-1)))) 
	{
		return (int)bits;
	}
	return (int)(bits|(~0u<<len)); /* extend sign */
}

unsigned int getbituInc(
	const unsigned char*	buff,
	int&					pos,
	int						len)
{
	unsigned int ans = getbitu(buff, pos, len);
	pos += len;
	return ans;
}

int getbitsInc(
	const unsigned char*	buff,
	int&					pos,
	int						len)
{
	int ans = getbits(buff, pos, len);
	pos += len;
	return ans;
}
