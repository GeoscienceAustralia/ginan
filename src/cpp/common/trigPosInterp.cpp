// /*
// 
// Original codes information:
// 
// A Brief Review of Basic GPS Orbit Interpolation Strategies
// 
// Mark S. Schenewerk
// give-em-an-inch.com
// Roeland Park, KS 66205
// published in: GPS Solutions, Volume 6, Number 4.
// 
// C code for comparing different techniques for interpolating GPS precise
// ephemerides in SP3 format. Example input and output files are included
// for the atest.c program.
// 
// */
// 
// 
// 
// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>
// #include <ctype.h>
// #include "atest.hpp"
// 
// Vector3d trigPosInterp(
// 	char*           source_eph,     ///< sp3 ephemeris file
// 	double          epoch,          ///< time epoch for the interpolation point
// 	int             num_data,       ///< the number of data points to use in when computing interpolation terms
// 	int             num_terms,      ///< the number of interpolation terms
// 	int             prn,            ///< PRN number of satellite to use in interpolation
// 	char            which_strategy) ///< selects an interpolation strategy
// {
// 
// 	/*
// 	*   pgm            = pointer to program name character string
// 	*/
// 	char pgm[] = "TrigInterp";
// 	/*
// 	*  Program variables
// 	*    delta_x        = interpolated - control X coordinate value
// 	*    delta_y        = interpolated - control Y coordinate value
// 	*    delta_z        = interpolated - control Z coordinate value
// 	*    err            = command line interpretation error flag
// 	*    i              = loop counter
// 	*    j              = loop counter
// 	*    index          = index of nearest entry in data epoch storage vector
// 	*    index_min      = starting index of data to use in interpolation
// 	*    last_index_min = last index_min
// 	*    ptr            = scratch character string pointer
// 	*    sig_x_new      = interpolated X value uncertainty
// 	*    sig_y_new      = interpolated Y value uncertainty
// 	*    sig_z_new      = interpolated Z value uncertainty
// 	*    t_new          = requested epoch for interpolation
// 	*    x_new          = interpolated X coordinate value
// 	*    y_new          = interpolated Y coordinate value
// 	*    z_new          = interpolated Z coordinate value
// 	*/
// 	char* ptr = NULL;
// 
// 	int err;
// 	int i;
// 	int j;
// 	int index;
// 	int index_min;
// 	int last_index_min;
// 
// 	double delta_3d;
// 	double delta_x;
// 	double delta_y;
// 	double delta_z;
// 	double sig_x_new;
// 	double sig_y_new;
// 	double sig_z_new;
// 	double t_new;
// 	double x_new;
// 	double y_new;
// 	double z_new;
// 
// 	/*
// 	*  Ephemeris data storage
// 	*    data_ntrpl = interpolation ephemeris data
// 	*/
// #   define max_data ((int)1000)
// 	int count_ntrpl;
// 	EpochDatum* data_ntrpl[max_data];
// 
// 	/*
// 	*  Interpolation data storage
// 	*    t              = independent (epoch) values
// 	*    x              = dependent (coordinate) values
// 	*    y              = dependent (coordinate) values
// 	*    z              = dependent (coordinate) values
// 	*    sig            = dependent (coordinate) value sigmas
// 	*/
// 	double* t = NULL;
// 	double* x = NULL;
// 	double* y = NULL;
// 	double* z = NULL;
// 	double* sig = NULL;
// 
// 	/*
// 	*  Singular value decomposition storage.
// 	*    OrbFunc        = routine which computes basis function values
// 	*    afunc          = evaluated basis function values
// 	*    x_coef         = X interpolation coefficients
// 	*    y_coef         = Y interpolation coefficients
// 	*    z_coef         = Z interpolation coefficients
// 	*    u              = singular value decomposition workspace
// 	*    v              = singular value decomposition workspace
// 	*    w              = singular value decomposition workspace
// 	*    cvm            = singular value decomposition covariance matrix
// 	*    mp             = number of data points
// 	*    ncvm           = size of covariance matrix
// 	*    np             = number of coefficients
// 	*/
// 	void (*OrbFunc)(double, double*, unsigned int);
// 	void (*OrbFunc)();
// 
// 	double x_chisq;
// 	double y_chisq;
// 	double z_chisq;
// 	double* x_coef = NULL;
// 	double* y_coef = NULL;
// 	double* z_coef = NULL;
// 	double* afunc = NULL;
// 	double** cvm = NULL;
// 	double** u = NULL;
// 	double** v = NULL;
// 	double* w = NULL;
// 
// 	unsigned int mp;
// 	unsigned int ncvm;
// 	unsigned int np;
// 
// 	/*
// 	*  Cubic spline interpolator storage.
// 	*    x2             = array of X coordinate second derivatives
// 	*    y2             = array of Y coordinate second derivatives
// 	*    z2             = array of Z coordinate second derivatives
// 	*    xp1            = first epoch X first derivative
// 	*    xpn            = last epoch X first derivative
// 	*    yp1            = first epoch Y first derivative
// 	*    ypn            = last epoch Y first derivative
// 	*    zp1            = first epoch Z first derivative
// 	*    zpn            = last epoch Z first derivative
// 	*/
// 	double* x2;
// 	double* y2;
// 	double* z2;
// 	double xp1 = 1.0e30;
// 	double xpn = 1.0e30;
// 	double yp1 = 1.0e30;
// 	double ypn = 1.0e30;
// 	double zp1 = 1.0e30;
// 	double zpn = 1.0e30;
// 
// 	/*
// 	*  Initialization
// 	*/
// 	for ( i = 0; i < max_data; ++i )
// 		data_ntrpl[i] = NULL;
// 
// 
// 	count_ntrpl = ScanFile( data_ntrpl, max_data, source_eph, prn );
// 
// 	err = 0;
// 
// 	if ( count_ntrpl == 0 )
// 	{
// 		fprintf(
// 			stderr, "%s error: reading interp_eph = \"%s\"\n",
// 			pgm, source_eph
// 		);
// 		++err;
// 	}
// 
// 	if ( num_data < 0 )
// 	{
// 		fprintf(
// 			stderr, "%s error: num_data (%d) < zero\n",
// 			pgm, num_data
// 		);
// 		++err;
// 	}
// 	else if ( num_data > count_ntrpl )
// 	{
// 		fprintf(
// 			stderr, "%s error: num_data (%d) > number of data points (%d)\n",
// 			pgm, num_data, count_ntrpl
// 		);
// 		++err;
// 	}
// 
// 	if ( num_terms < 0 )
// 	{
// 		fprintf(
// 			stderr, "%s error: num_terms (%d) < zero\n",
// 			pgm, num_terms
// 		);
// 		++err;
// 	}
// 	else if ( num_terms > num_data )
// 	{
// 		fprintf(
// 			stderr, "%s error: num_terms (%d) > num_data (%d)\n",
// 			pgm, num_terms, num_data
// 		);
// 		++err;
// 	}
// 
// 	if ( which_strategy >= 'a' && which_strategy <= 'z' )
// 		which_strategy = which_strategy - 'a' + 'A';
// 	if ( which_strategy != 'N' &&
// 			which_strategy != 'T' &&
// 			which_strategy != '2' )
// 	{
// 		fprintf(
// 			stderr, "%s error: interpolation strategy = %c\n",
// 			pgm, which_strategy
// 		);
// 		++err;
// 	}
// 
// 	if ( err > 0 )
// 	{
// 		exit( 2 );
// 	}
// 
// 	/*
// 	*  Allocate storage space for interpolation.
// 	*/
// 
// 	if ( (t = (double*)malloc( num_data * sizeof( double ) )) == NULL )
// 	{
// 		fprintf( stderr, "%s", "error: allocating \"t\" data storage\n" );
// 		exit( 3 );
// 	}
// 
// 	if ( (x = (double*)malloc( num_data * sizeof( double ) )) == NULL )
// 	{
// 		fprintf( stderr, "%s", "error: allocating \"x\" data storage\n" );
// 		exit( 3 );
// 	}
// 	if ( (y = (double*)malloc( num_data * sizeof( double ) )) == NULL )
// 	{
// 		fprintf( stderr, "%s", "error: allocating \"y\" data storage\n" );
// 		exit( 3 );
// 	}
// 	if ( (z = (double*)malloc( num_data * sizeof( double ) )) == NULL )
// 	{
// 		fprintf( stderr, "%s", "error: allocating \"z\" data storage\n" );
// 		exit( 3 );
// 	}
// 	if ( (sig = (double*)malloc( num_data * sizeof( double ) )) == NULL )
// 	{
// 		fprintf( stderr, "%s", "error: allocating \"sig\" data storage\n" );
// 		exit( 3 );
// 	}
// 
// 	if ( which_strategy == 'T' || which_strategy == '2' )
// 	{
// 
// 		if ( which_strategy == '2' )
// 			OrbFunc = &TrigExt;
// 		else
// 			OrbFunc = &TrigFunc;
// 
// 		if ( num_data >= num_terms )
// 			mp = num_data;
// 		else
// 			mp = num_terms;
// 		np = num_terms;
// 		ncvm = np;
// 
// 		/*
// 		*  Allocate singular value decomposition storage space.
// 		*
// 		*  Interpolation coefficient storage.
// 		*/
// 		if ( (x_coef = (double*)malloc( num_terms * sizeof( double ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"x_coef\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < num_terms; ++i )
// 			x_coef[i] = 1.0;
// 
// 		if ( (y_coef = (double*)malloc( num_terms * sizeof( double ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"y_coef\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < num_terms; ++i )
// 			y_coef[i] = 1.0;
// 
// 		if ( (z_coef = (double*)malloc( num_terms * sizeof( double ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"z_coef\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < num_terms; ++i )
// 			z_coef[i] = 1.0;
// 
// 		/*
// 		*  Basis function values
// 		*/
// 		if ( (afunc = (double*)malloc( num_terms * sizeof( double ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"afunc\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < num_terms; ++i )
// 			afunc[i] = 0.0;
// 
// 
// 		/*
// 		*  Singular value decomposition workspace storage.
// 		*/
// 		if ( (u = (double**)malloc( mp * sizeof( double* ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"u\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < mp; ++i )
// 		{
// 			u[i] = NULL;
// 			if ( (u[i] = (double*)malloc( np * sizeof( double ) )) == NULL )
// 			{
// 				fprintf( stderr, "%s", "error: allocating \"u\" storage\n" );
// 				exit( 3 );
// 			}
// 			for ( j = 0; j < np; ++j )
// 			{
// 				u[i][j] = 0.0;
// 			}
// 		}
// 
// 		if ( (v = (double**)malloc( np * sizeof( double* ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"v\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < np; ++i )
// 		{
// 			v[i] = NULL;
// 			if ( (v[i] = (double*)malloc( np * sizeof( double ) )) == NULL )
// 			{
// 				fprintf( stderr, "%s", "error: allocating \"v\" storage\n" );
// 				exit( 3 );
// 			}
// 			for ( j = 0; j < np; ++j )
// 			{
// 				v[i][j] = 0.0;
// 			}
// 		}
// 
// 		if ( (w = (double*)malloc( np * sizeof( double ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"w\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < np; ++i )
// 		{
// 			w[i] = 0.0;
// 		}
// 
// 		/*
// 		*  Singular value decomposition covariance matrix storage
// 		*/
// 		ncvm = np;
// 		if ( (cvm = (double**)malloc( ncvm * sizeof( double* ) )) == NULL )
// 		{
// 			fprintf( stderr, "%s", "error: allocating \"cvm\" storage\n" );
// 			exit( 3 );
// 		}
// 		for ( i = 0; i < ncvm; ++i )
// 		{
// 			if ( (cvm[i] = (double*)malloc( ncvm * sizeof( double ) )) == NULL )
// 			{
// 				fprintf( stderr, "%s", "error: allocating \"cvm\" storage\n" );
// 				exit( 3 );
// 			}
// 			for ( j = 0; j < ncvm; ++j )
// 			{
// 				cvm[i][j] = 0.0;
// 			}
// 		}
// 	}
// 
// 
// 
// 	/*
// 	*  Loop over all entries in the control ephemeris
// 	*/
// 	last_index_min = -1;
// 
// 	/*
// 		*  Determine the upper and lower indices of the data to use
// 		*  in computing the interpolation parameters.
// 		*/
// 	index = search( data_ntrpl, count_ntrpl, epoch, index );
// 	index_min = index - num_data / 2;
// 	if ( index_min < 0 )
// 		index_min = 0;
// 	else if ( (index_min + num_data) > count_ntrpl )
// 		index_min = count_ntrpl - num_data;
// 
// 	/*
// 		*  Compute new interpolation terms as necessary.
// 		*/
// 	if ( last_index_min != index_min )
// 	{
// 		/*
// 			*  Load interpolation data storage arrays.
// 			*/
// 		for ( j = 0; j < num_data; ++j )
// 		{
// 			t[j] = data_ntrpl[index_min + j]->epoch
// 				   - data_ntrpl[index_min]->epoch;
// 			x[j] = data_ntrpl[index_min + j]->xyz[0];
// 			y[j] = data_ntrpl[index_min + j]->xyz[1];
// 			z[j] = data_ntrpl[index_min + j]->xyz[2];
// 			sig[j] = data_ntrpl[index_min + j]->sig[0];
// 		}
// 
// 		if ( which_strategy == 'T' || which_strategy == '2' )
// 		{
// 			/*
// 				*  Compute the X, Y, and Z interpolation terms.
// 				*/
// 			x_chisq = 0.0;
// 			svdfit( t, x, sig, num_data,
// 					x_coef, num_terms,
// 					u, v, w, mp, np,
// 					&x_chisq, OrbFunc );
// 
// 			/* svdvar(v, num_terms, np, w, cvm, ncvm );*/
// 
// 			y_chisq = 0.0;
// 			svdfit( t, y, sig, num_data,
// 					y_coef, num_terms,
// 					u, v, w, mp, np,
// 					&y_chisq, OrbFunc );
// 
// 			/* svdvar(v, num_terms, np, w, cvm, ncvm );*/
// 
// 			z_chisq = 0.0;
// 			svdfit( t, z, sig, num_data,
// 					z_coef, num_terms,
// 					u, v, w, mp, np,
// 					&z_chisq, OrbFunc );
// 
// 			/* svdvar(v, num_terms, np, w, cvm, ncvm );*/
// 		}
// 
// 		last_index_min = index_min;
// 	}
// 
// 	/*
// 		*  Compute interpolated values at this epoch
// 		*/
// 	t_new = epoch - data_ntrpl[index_min]->epoch;
// 
// 	if ( which_strategy == 'T' || which_strategy == '2' )
// 	{
// 		(*OrbFunc)( t_new, afunc, num_terms );
// 
// 		/*
// 			*  Compute interpolated X, Y and Z values.
// 			*/
// 		x_new = 0.0;
// 		for ( j = 0; j < num_terms; ++j )
// 		{
// 			x_new = x_new + x_coef[j] * afunc[j];
// 		}
// 
// 		y_new = 0.0;
// 		for ( j = 0; j < num_terms; ++j )
// 			y_new = y_new + y_coef[j] * afunc[j];
// 
// 		z_new = 0.0;
// 		for ( j = 0; j < num_terms; ++j )
// 			z_new = z_new + z_coef[j] * afunc[j];
// 	}
// 	else
// 	{
// 		polint( t, x, num_data, t_new, &x_new, &sig_x_new );
// 		polint( t, y, num_data, t_new, &y_new, &sig_y_new );
// 		polint( t, z, num_data, t_new, &z_new, &sig_z_new );
// 	}
// 
// 	return Vector3d(x_new, y_new, z_new);
// }
