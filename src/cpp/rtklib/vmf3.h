
#pragma once

struct Vmf3GridPoint
{
	union
	{
		double data[10] = {};
		struct
		{
			double lat;		// lat grid (degree) 
			double lon;		// lon grid (degree) 
			double ah;		// hydrostatic mapping coefficient 
			double aw;		// wet mapping coefficient 
			double zhd;		// zenith hydrastatic delay (m) 
			double zwd;		// zenith wet delay 		
			double mfh;		// hydrostatic mapping function
			double mfw;		// wet mapping function
			double orog;	
			double index;	// index in the file, for use against orography files
		};
	};
	
	Vmf3GridPoint operator * (double d)
	{
		Vmf3GridPoint output = *this;
		for (int i = 0; i < 9; i++)	//skip index
		{
			output.data[i] *= d;
		}
		
		return output;
	}
	
	Vmf3GridPoint& operator += (const Vmf3GridPoint& d)
	{
		for (int i = 0; i < 9; i++)	//skip index
		{
			data[i] += d.data[i];
		}
		
		return *this;
	}
};

struct Vmf3 : map<GTime, map<double, map<double, Vmf3GridPoint>>>
{
	vector<double>	orography;
};

int readvmf3(
	string			file,
	Vmf3&			vmf3);

int readorog(
	string			file,
	vector<double>& orog);

int tropvmf3(
	const Vmf3&			vmf3,	
	GTime				time,	
	const VectorPos&	pos,
	const double		el,		
	double&				zhd,	
	double&				zwd,	
	double				mf[2]);

