#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "crd.h"

/*-------------------------------------------------------------------------
* Subroutines: read CRD data records from an input string.
*
* Author: Randall Ricklefs / Univ of Texas / Center for Space Research
*
* History:
*   July 06, 2007 - Initial version
*   June 24, 2008 - v1.00
*   July 02, 2019 - v2.00 plus NA. rlr.
*   Sept 09, 2020 - v2.01 - Fixed string copy length in header records and 
*                   returns from readi, readd, and readld, and 
*                   writing a substitute strcasecpy.
*                   Thanks to Mark Yeo, Industrial Sciences Group, for catching
*                   these errors! rlr.
*   Dec  15, 2020 - V2.01b - Removed the test for "na" for the detail type 
*                   in the C3 and C6 records. "NA" should never be allowed for 
*                   detail type. rlr.
*
**-----------------------------------------------------------------------*/

char stro[256];
static int format_version;
void remove_blanks (char *, int);
int tokenize (char *, char *, int, int, char *);
int readi (char *, int *);
int readd (char *, double *);
int readld (char *, long double *);
int isna (char *);

/* Ranging data header/footer records */
/* H1 - format header */
int
read_h1 (char * str, struct CrdH1 *header)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

strncpy (header->crd_literal, &tokens[i*ncols], 3); // fixed 09/09/2020
header->crd_literal[3]= '\0';											i++;
if (readi (&tokens[i*ncols], &header->format_version)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->prod_year)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->prod_mon)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->prod_day)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->prod_hour)		< 0) nstat--;


// make sure later records know the format version
//   version 0 was for beta-version data.
if (header->format_version == 0) header->format_version= 1;
format_version= header->format_version;

return (nstat+1);
}

/* H2 - station header */
int
read_h2 (char * str, struct CrdH2 *header)
{
int nstat;


if (format_version == 1)
	{
	nstat= sscanf (&str[14],
		"%d %d %d %d",
		&header->cdp_pad_id, &header->cdp_sys_num, &header->cdp_occ_num, 
		&header->stn_timescale);

	strncpy (header->stn_name, &str[3], 10);	/* to preserve spaces */
	header->stn_name[10]= '\0';
	remove_blanks (header->stn_name, strlen (header->stn_name));
	nstat+= 2;
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];
	
	nstat= tokenize (str, " \n", 100, 256, tokens);
	
	strncpy (header->stn_name, &tokens[i*ncols], 10); // fixed 09/09/2020
	header->stn_name[10]= '\0';												i++;
	if (readi (&tokens[i*ncols], &header->cdp_pad_id)		< 0)	nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->cdp_sys_num)	< 0)	nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->cdp_occ_num)	< 0)	nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->stn_timescale)	< 0)	nstat--;	i++;
	strncpy (header->stn_network, &tokens[i*ncols], 10); // fixed 09/09/2020
	header->stn_network[10]= '\0';

	}

return (nstat);
}

/* H3 - spacecraft header */
int
read_h3 (char * str, struct CrdH3 *header)
{
int nstat;

if (format_version == 1)
	{
	nstat= sscanf (&str[14],
					"%d %d %d %d %d", 
				&header->ilrs_id, &header->sic, 
				&header->norad, &header->SC_timescale,
					&header->target_type);

	strncpy (header->target_name, &str[3], 10);	/* to preserve spaces */
	header->target_name[10]= '\0';
	remove_blanks (header->target_name, strlen (header->target_name));

	// Convert to v2 target class and location
	// incomplete: Could include LRO, ELT, etc.
	header->target_class= header->target_type;
	header->target_loc= 1;
	if (header->target_type == 2)       // old lunar reflector entry
		{
		header->target_class= 1;
		header->target_loc= 3;
		}
	nstat+= 2;
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];
	
	nstat= tokenize (str, " \n", 100, 256, tokens);
	
	strncpy (header->target_name, &tokens[i*ncols], 10); // fixed 09/09/2020
	header->target_name[10]= '\0';										i++;
	if (readi (&tokens[i*ncols], &header->ilrs_id)		< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->sic)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->norad)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->SC_timescale)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->target_class)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &header->target_loc)		< 0) nstat--;

	// Convert to v1 target type
	header->target_type= header->target_class;
	if (header->target_class == 1 && header->target_loc == 3)
		{
		header->target_type= 2;
		}
	nstat++;
	}
return (nstat);
}

/* H4 - Session header */
int
read_h4 (char * str, struct CrdH4 *header)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &header->data_type)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_year)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_mon)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_day)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_hour)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_min)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->start_sec)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_year)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_mon)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_day)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_hour)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_min)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->end_sec)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->data_release)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->refraction_app_ind)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->CofM_app_ind)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->xcv_amp_app_ind)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->stn_sysdelay_app_ind)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->SC_sysdelay_app_ind)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->range_type_ind)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->data_qual_alert_ind)	< 0) nstat--;	

return (nstat+1);
}

/* H5 - Prediction header */
// New in v2.
int
read_h5 (char * str, struct CrdH5 *header)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &header->prediction_type) < 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &header->year_of_century) < 0) nstat--;	i++;
strncpy (header->date_and_time, &tokens[i*ncols], 12); // fixed 09/09/2020
header->date_and_time[12]= '\0';										i++;
strncpy (header->prediction_provider, &tokens[i*ncols], 10); // fixed 09/09/2020
header->prediction_provider[10]= '\0';								i++;
if (readi (&tokens[i*ncols], &header->sequence_number) < 0) nstat--;


return (nstat+1);
}

/* Need indicators that these have been read? */
/* H8 - End of Session footer */
int
read_h8 (char * str)
{
sscanf (str, "H8");
}

/* H9 - End of File footer */
int
read_h9 (char * str)
{
sscanf (str, "H9");
}

/* Ranging data configuration records (1 of n) */
	/* C0 - System Configuration Record */
int
read_c0 (char * str, struct CrdC0 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
int ii;
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->xmit_wavelength)	< 0) nstat--;	i++;

// Zero-initialise config_ids - added 09/09/2020
//   10 -> from "char config_ids[10][41];" in crd.h
for (int j = 0; j < 10; j++) { 
	config->config_ids[j][0] = '\0';
}

// Write only those config_ids that exist, hence nstat below
for (ii=i; ii<nstat; ii++)
	{
	strncpy (config->config_ids[ii-i], &tokens[ii*ncols], 40);
	config->config_ids[ii-i][40]= '\0';
	}


return (nstat+1);
}

	/* C1 - Laser Configuration Record */
int
read_c1 (char * str, struct CrdC1 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)		< 0) nstat--;	i++;
strncpy (config->laser_config_id, &tokens[i*ncols], 40);
config->laser_config_id[40]= '\0';										i++;
strncpy (config->laser_type, &tokens[i*ncols], 40);
config->laser_type[40]= '\0';												i++;
if (readd (&tokens[i*ncols], &config->prim_wavelength)	< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->nom_fire_rate)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->pulse_energy)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->pulse_width)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->beam_div)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &config->pulses_in_semitrain)< 0) nstat--;


return (nstat+1);
}

	/* C2 - Detector Configuration Record */
int
read_c2 (char * str, struct CrdC2 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)				< 0) nstat--;	i++;
strncpy (config->detector_config_id, &tokens[i*ncols], 40);
config->detector_config_id[40]= '\0';												i++;
strncpy (config->detector_type, &tokens[i*ncols], 40);
config->detector_type[40]= '\0';													i++;
if (readd (&tokens[i*ncols], &config->app_wavelength)				< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->qe)							< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->voltage)					< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->dark_count)					< 0) nstat--;	i++;
strncpy (config->output_pulse_type, &tokens[i*ncols], 40);
config->output_pulse_type[40]= '\0';												i++;
if (readd (&tokens[i*ncols], &config->output_pulse_width)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->spectral_filter)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->spectral_filter_xmission)	< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->spatial_filter)				< 0) nstat--;	i++;
strncpy (config->signal_proc, &tokens[i*ncols], 40);
config->signal_proc[40]= '\0';

if (format_version == 2)
	{
	i++;
	if (readd (&tokens[i*ncols], &config->amp_gain)		< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &config->amp_bandwidth)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &config->amp_in_use)		< 0) nstat--;
	}


return (nstat+1);
}

	/* C3 - Timing Configuration Record */
int
read_c3 (char * str, struct CrdC3 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type) < 0) nstat--;									i++;
strncpy (config->timing_config_id,	&tokens[i*ncols], 40);	config->timing_config_id[40]= '\0';	i++;
strncpy (config->time_source,			&tokens[i*ncols], 40);	config->time_source		[40]= '\0';	i++;
strncpy (config->freq_source,			&tokens[i*ncols], 40);	config->freq_source		[40]= '\0';	i++;
strncpy (config->timer,				&tokens[i*ncols], 40);	config->timer			[40]= '\0';	i++;
strncpy (config->timer_serial_num,	&tokens[i*ncols], 40);	config->timer_serial_num[40]= '\0';	i++;

if (readd (&tokens[i*ncols], &config->epoch_delay_corr) < 0) nstat--;

return (nstat+1);
}

	/* C4 - Transponder Configuration Record */
int
read_c4 (char * str, struct CrdC4 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)			< 0) nstat--;	i++;
strncpy (config->xponder_config_id, &tokens[i*ncols], 40);
config->xponder_config_id[40]= '\0';											i++;
if (readld(&tokens[i*ncols], &config->est_stn_utc_offset)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->est_stn_osc_drift)		< 0) nstat--;	i++;
if (readld(&tokens[i*ncols], &config->est_xponder_utc_offset)	< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->est_xponder_osc_drift)	< 0) nstat--;	i++;
if (readld(&tokens[i*ncols], &config->xponder_clock_ref_time)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &config->stn_off_drift_app_ind)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &config->SC_off_drift_app_ind)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &config->SC_time_simplified_ind)	< 0) nstat--;	

return (nstat+1);
}

	/* C5 - Software Configuration Record */
	// new in v2.
int
read_c5 (char * str, struct CrdC5 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)	< 0) nstat--;															i++;
strncpy (config->software_config_id,				&tokens[i*ncols], 40);	config->software_config_id				[40]= '\0';	i++;
strncpy (config->tracking_software,				&tokens[i*ncols], 40);	config->tracking_software				[40]= '\0';	i++;
strncpy (config->tracking_software_versions,		&tokens[i*ncols], 40);	config->tracking_software_versions		[40]= '\0';	i++;
strncpy (config->processing_software,				&tokens[i*ncols], 40);	config->processing_software				[40]= '\0';	i++;
strncpy (config->processing_software_versions,	&tokens[i*ncols], 40);	config->processing_software_versions	[40]= '\0';	

return (nstat+1);
}

	/* C6 - Meteorology Configuration Record */
	// new in v2.
int
read_c6 (char * str, struct CrdC6 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)				< 0) nstat--;	i++;
strncpy (config->met_config_id, 					&tokens[i*ncols], 40);  config->met_config_id					[40]= '\0';	i++;
strncpy (config->pressure_sensor_manufacturer, 	&tokens[i*ncols], 40);  config->pressure_sensor_manufacturer	[40]= '\0';	i++;
strncpy (config->pressure_sensor_model, 			&tokens[i*ncols], 40);  config->pressure_sensor_model			[40]= '\0';	i++;
strncpy (config->pressure_sensor_serial_num, 		&tokens[i*ncols], 40);  config->pressure_sensor_serial_num		[40]= '\0';	i++;
strncpy (config->temperature_sensor_manufacturer, &tokens[i*ncols], 40);  config->temperature_sensor_manufacturer	[40]= '\0';	i++;
strncpy (config->temperature_sensor_model, 		&tokens[i*ncols], 40);  config->temperature_sensor_model		[40]= '\0';	i++;
strncpy (config->temperature_sensor_serial_num, 	&tokens[i*ncols], 40);  config->temperature_sensor_serial_num	[40]= '\0';	i++;
strncpy (config->humidity_sensor_manufacturer, 	&tokens[i*ncols], 40);  config->humidity_sensor_manufacturer	[40]= '\0';	i++;
strncpy (config->humidity_sensor_model, 			&tokens[i*ncols], 40);  config->humidity_sensor_model			[40]= '\0';	i++;
strncpy (config->humidity_sensor_serial_num, 		&tokens[i*ncols], 40);  config->humidity_sensor_serial_num		[40]= '\0';	

return (nstat+1);
}

	/* C7 - Meteorology Configuration Record */
	// new in v2.
int
read_c7 (char * str, struct CrdC7 *config)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readi (&tokens[i*ncols], &config->detail_type)				< 0) nstat--;	i++;
strncpy (config->calconfig_id, &tokens[i*ncols], 40);
config->calconfig_id[40]= '\0';													i++;
strncpy (config->target_name, &tokens[i*ncols], 40);
config->target_name[40]= '\0';													i++;
if (readd (&tokens[i*ncols], &config->surveyed_target_dist)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->survey_error)				< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->other_fixed_delays)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &config->pulse_energy)				< 0) nstat--;	i++;
strncpy (config->processing_software, &tokens[i*ncols], 40);
config->processing_software[40]= '\0';											i++;
strncpy (config->processing_software_version, &tokens[i*ncols], 40);
config->processing_software_version[40]= '\0';

return (nstat+1);
}

/* Ranging data records */
/* Secofday: need int sec and int psec? */
	/* 10 - Range Record */
int
read_10 (char * str, struct CrdD10 *data_recd)
{
int nstat;

if (format_version == 1)
	{
	char temp_sysconfig_id[256];
	nstat= sscanf (str,
		"%*s %Lf %Lf %255s %d %d %d %d %d",
		&data_recd->sec_of_day, &data_recd->time_of_flight, 
	temp_sysconfig_id, &data_recd->epoch_event, 
	&data_recd->filter_flag, &data_recd->detector_channel, 
	&data_recd->stop_number, &data_recd->xcv_amp);
	strncpy (data_recd->sysconfig_id, temp_sysconfig_id, 40);
	data_recd->sysconfig_id[40]= '\0';
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];
	nstat= tokenize (str, " \n", 100, 256, tokens);

	if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
	if (readld(&tokens[i*ncols], &data_recd->time_of_flight)		< 0) nstat--;	i++;
	strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
	data_recd->sysconfig_id[40]= '\0';											i++;
	if (readi (&tokens[i*ncols], &data_recd->epoch_event)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->filter_flag)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->detector_channel)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->stop_number)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->xcv_amp)				< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->xmt_amp)				< 0) nstat--;
	}

return (nstat+1);
}

	/* 11 - Normal Point Record */
int
read_11 (char * str, struct CrdD11 *data_recd)
{
char temp_sysconfig_id[256];
int nstat;

if (format_version == 1)
	{
	nstat= sscanf (str,
		"%*s %Lf %Lf %255s %d %lf %d %lf %lf %lf %lf %lf %d",
		&data_recd->sec_of_day, &data_recd->time_of_flight, 
	temp_sysconfig_id, &data_recd->epoch_event, 
	&data_recd->np_window_length, &data_recd->num_ranges, 
	&data_recd->bin_rms, &data_recd->bin_skew, &data_recd->bin_kurtosis, 
	&data_recd->bin_PmM, &data_recd->return_rate, 
		&data_recd->detector_channel);
	strncpy (data_recd->sysconfig_id, temp_sysconfig_id, 40);
	data_recd->sysconfig_id[40]= '\0';
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];
	nstat= tokenize (str, " \n", 100, 256, tokens);

	if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
	if (readld(&tokens[i*ncols], &data_recd->time_of_flight)		< 0) nstat--;	i++;
	strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
	data_recd->sysconfig_id[40]= '\0';	i++;
	if (readi (&tokens[i*ncols], &data_recd->epoch_event)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->np_window_length)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->num_ranges)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->bin_rms)				< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->bin_skew)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->bin_kurtosis)		< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->bin_PmM)				< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->return_rate)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->detector_channel)	< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->signal_to_noise)		< 0) nstat--;
	}

return (nstat+1);
}

	/* 12 - Range Supplement Record */
int
read_12 (char * str, struct CrdD12 *data_recd)
{
char temp_sysconfig_id[256];
int nstat;

if (format_version == 1)
	{
	nstat= sscanf (str,
		"%*s %Lf %255s %lf %lf %lf %lf",
		&data_recd->sec_of_day, temp_sysconfig_id,
	&data_recd->refraction_corr, &data_recd->target_CofM_corr, 
	&data_recd->nd_value, &data_recd->time_bias);
	strncpy (data_recd->sysconfig_id, temp_sysconfig_id, 40);
	data_recd->sysconfig_id[40]= '\0';
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];
	
	nstat= tokenize (str, " \n", 100, 256, tokens);
	if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
	
	strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
	data_recd->sysconfig_id[40]= '\0';											i++;

	if (readd (&tokens[i*ncols], &data_recd->refraction_corr)		< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->target_CofM_corr)	< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->nd_value)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->time_bias)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->range_rate)			< 0) nstat--;	
	}

return (nstat+1);
}

	/* 20 - Meteorological Record */
int
read_20 (char * str, struct CrdD20 *data_recd)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);
if (readld(&tokens[i*ncols], &data_recd->sec_of_day)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->pressure)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->temperature)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->humidity)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->value_origin)	< 0) nstat--;	i++;

return (nstat+1);
}

	/* 21 - Meteorological Supplement Record */
int
read_21 (char * str, struct CrdD21 *data_recd)
{
char temp_precip_type[256];
char temp_weather_conditions[256];
int nstat;

if (format_version == 1)
	{
	nstat= sscanf (str,
		"%*s %Lf %lf %lf %255s %d %lf %d %d",
		&data_recd->sec_of_day, &data_recd->wind_speed, 
	&data_recd->wind_direction, temp_precip_type, 
	&data_recd->visibility, &data_recd->sky_clarity,
	&data_recd->atmospheric_seeing, &data_recd->cloud_cover);
	strncpy (data_recd->weather_conditions, temp_precip_type, 40);
	data_recd->weather_conditions[40]= '\0';
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];

	nstat= tokenize (str, " \n", 100, 256, tokens);

	if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->wind_speed)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->wind_direction)		< 0) nstat--;	i++;

	strncpy (data_recd->weather_conditions, &tokens[i*ncols], 40);
	data_recd->weather_conditions[40]= '\0';										i++;
	
	if (readi (&tokens[i*ncols], &data_recd->visibility)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->sky_clarity)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->atmospheric_seeing)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->cloud_cover)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->sky_temperature)		< 0) nstat--;	
	}

return (nstat+1);
}

	/* 30 - Pointing Angles Record */
int
read_30 (char * str, struct CrdD30 *data_recd)
{
int nstat;

if (format_version == 1)
	{
	nstat= sscanf (str,
		"%*s %Lf %lf %lf %d %d %d",
		&data_recd->sec_of_day, &data_recd->azimuth, &data_recd->elevation, 
	&data_recd->direction_ind, &data_recd->angle_origin_ind, 
	&data_recd->refraction_corr_ind);
	}
else if (format_version == 2)
	{
	int nrows= 100, ncols= 256;
	int i= 1; // =1 to skip the record id.
	char tokens[100*256];

	nstat= tokenize (str, " \n", 100, 256, tokens);

	if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->azimuth)				< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->elevation)			< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->direction_ind)		< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->angle_origin_ind)	< 0) nstat--;	i++;
	if (readi (&tokens[i*ncols], &data_recd->refraction_corr_ind)	< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->azimuth_rate)		< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->elevation_rate)		< 0) nstat--;	
}

return (nstat+1);
}

	/* 40 - Calibration Record */
int
read_40 (char * str, struct CrdD40 *data_recd)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readld(&tokens[i*ncols], &data_recd->sec_of_day)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->type_of_data)			< 0) nstat--;	i++;

strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
data_recd->sysconfig_id[40]= '\0';												i++;

if (readi (&tokens[i*ncols], &data_recd->num_points_recorded)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->num_points_used)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->one_way_target_dist)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_sys_delay)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_delay_shift)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_rms)					< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_skew)				< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_kurtosis)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_PmM)					< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_type_ind)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_shift_type_ind)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->detector_channel)		< 0) nstat--;	
if (format_version == 2)
	{
	i++;
	if (readi (&tokens[i*ncols], &data_recd->cal_span)			< 0) nstat--;	i++;
	if (readd (&tokens[i*ncols], &data_recd->cal_return_rate)		< 0) nstat--;
	}


return (nstat+1);
}

	/* 41 - Calibration Detail Record */
int
read_41 (char * str, struct CrdD40 *data_recd)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readld(&tokens[i*ncols], &data_recd->sec_of_day)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->type_of_data)		< 0) nstat--;	i++;
strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
data_recd->sysconfig_id[40]= '\0';											i++;
if (readi (&tokens[i*ncols], &data_recd->num_points_recorded)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->num_points_used)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->one_way_target_dist)	< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_sys_delay)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_delay_shift)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_rms)				< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_skew)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_kurtosis)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_PmM)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_type_ind)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_shift_type_ind)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->detector_channel)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_span)			< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->cal_return_rate)		< 0) nstat--;	

return (nstat+1);
}

	/* 42 - Calibration "Shot" Record */
int
read_42 (char * str, struct CrdD42 *data_recd)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

if (readld(&tokens[i*ncols], &data_recd->sec_of_day)				< 0) nstat--;	i++;
if (readld(&tokens[i*ncols], &data_recd->time_of_flight)			< 0) nstat--;	i++;
strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
data_recd->sysconfig_id[40]= '\0';												i++;
strncpy (data_recd->calconfig_id, &tokens[i*ncols], 40);
data_recd->calconfig_id[40]= '\0';												i++;
if (readd (&tokens[i*ncols], &data_recd->other_variable_delays)	< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->type_of_data)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_type_ind)			< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->filter_flag)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->detector_channel)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->stop_number)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->cal_span)				< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->xcv_amp)					< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->xmt_amp)					< 0) nstat--;	

return (nstat+1);
}

	/* 50 - Session Statistics Record */
int
read_50 (char * str, struct CrdD50 *data_recd)
{
int nstat;
int nrows= 100, ncols= 256;
int i= 1; // =1 to skip the record id.
char tokens[100*256];

nstat= tokenize (str, " \n", 100, 256, tokens);

strncpy (data_recd->sysconfig_id, &tokens[i*ncols], 40);
data_recd->sysconfig_id[40]= '\0';										i++;
if (readd (&tokens[i*ncols], &data_recd->sess_rms)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->sess_skew)		< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->sess_kurtosis)	< 0) nstat--;	i++;
if (readd (&tokens[i*ncols], &data_recd->sess_PmM)		< 0) nstat--;	i++;
if (readi (&tokens[i*ncols], &data_recd->data_qual_ind)	< 0) nstat--;	

return (nstat+1);
}

	/* 60 - Compatibility Record */
int
read_60 (char * str, struct CrdD60 *data_recd)
{
char temp_sysconfig_id[256];
int nstat;

nstat= sscanf (str,
	"%*s %255s %d %d", 
	temp_sysconfig_id, &data_recd->sys_change_ind, 
	&data_recd->sys_config_ind); 

strncpy (data_recd->sysconfig_id, temp_sysconfig_id, 40);
data_recd->sysconfig_id[40]= '\0';

return (nstat+1);
}

	/* 9X - User Defined Records 90-99 */
int
read_9x (char * str, struct CrdD9x *data_recd)
{
int nstat= 0;
return (nstat);
}

	/* 00 - Comment Record */
int
read_00 (char * str, struct CrdD00 *data_recd)
{
int i;
int nstat= 1;

// search for and remove '\n' (and '\r' for MS Windows)
strncpy(data_recd->comment, &str[3], 80);
data_recd->comment[80]= '\0';
for (i=0; i<80; i++)
	{
	if (data_recd->comment[i] == '\n' ||
		data_recd->comment[i] == '\r')
		data_recd->comment[i]= '\0';
	}
return (nstat);
}

void
remove_blanks (char *str, int slen)
{
int i, j, k;

// Do we have a string to check?
if (slen == 0) return;

// Handle left- and right-justified fields.
// Assumes no blanks mid-field.
if (isalnum(str[0]))
	{
	// left-justified
	for (i=slen-1; i>0; i--)
		{
		if (str[i] == ' ') str[i]= '\0';
		}
	}
else
	{
	// right-justified
	for (i=0; i<slen; i++)
		{
		if (str[i] != ' ') break;
		}
	k= 0;
	for (j=i; j<slen; j++)
		{
		str[k++]= str[j];
		}
	str[k]= '\0';
	}
}

int tokenize (char *str, char *separator, int rows, int cols, char *tokens)
{
char* token = strtok(str, separator);
int nt= 0;

// Keep printing tokens while one of the delimiters present in str[].
while (token != NULL) {
	strncpy (&tokens[nt*cols], token, cols);
	tokens[nt*cols+ cols-1]= '\0';    // make sure the string is terminated
	nt++;
	if (nt == rows)
		{
		printf ("Too many tokens for array! Quitting!\n");
		return (-1);
		}
	token = strtok(NULL, separator);
}
return (nt);
}

int
readi (char *str, int *ival)
{
if (strcasecmp (str, "na") == 0)
	{
	*ival= NA_VALUE;
	return (0);
	}
else if (sscanf (str, "%d", ival) != 1)
	{
	return (-1);
	}
return (1);
}

int
readd (char *str, double *dval)
{
if (strcasecmp (str, "na") == 0)
	{
	*dval= NA_VALUEF;
	return (0);
	}
else if (sscanf (str, "%lf", dval) != 1)
	{
	return (-1);
	}
return (1);
}

int
readld (char *str, long double *ldval)
{
if (strcasecmp (str, "na") == 0)
	{
	*ldval= NA_VALUEF;
	return (0);
	}
else if (sscanf (str, "%Lf", ldval) != 1)
	{
	return (-1);
	}
return (1);
}

int
isna (char *str)
{
if (strcasecmp (str, "na") == 0) return (1);
return (0);
}

#ifdef NEEDED
// For those systems that don't have strcasecmp...
// Here's a version written by Mark Yeo, Industrial Sciences Group 
// Just comment out the #idfe and #endif or 
//     #define NEEDED (1)
//     
// NOTE: str1 & str2 must each be < 100 characters long and '/0'-terminated
int
strcasecmp(char *str1, char *str2) 
{
	// Make lower-case copies of str1 and str2, then compare them
	char str1_lower[100];
	int i = 0;

	while (*str1 != '\0') {
		str1_lower[i] = tolower(*str1);
		str1++;
		i++;
	}
	str1_lower[i] = '\0';

	char str2_lower[100];
	i = 0;
	while (*str2 != '\0') {
		str2_lower[i] = tolower(*str2);
		str2++;
		i++;
	}

	str2_lower[i] = '\0';

	return strcmp(str1_lower, str2_lower);
}
#endif
