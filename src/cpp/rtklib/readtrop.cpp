/*-
* Note: support one day troposphere solution of MAXIGSSTA stations, with
*       'SAMPLING TROP' interval larger than 30 minutes*/

#include "acsQC.hpp"

/* initialize troposphere sturct -----------------------------------------------
* args     :       mgex_trop *mtrop	I/O     tropsphere sturct
* return   :       0 fail, 1 success
* ---------------------------------------------------------------------------*/
extern int inittrop(mgex_trop* mtrop)
{
	int i;
	mgex_tropcoord tmp = {{0}};
	mtrop->ver = mtrop->el = mtrop->factor = 0;
	mtrop->inttrop = mtrop->intdata = mtrop->bstart = mtrop->bend = 0;
	mtrop->obscode = ' ';
	memset(mtrop->id, 0, sizeof(mtrop->id));
	memset(mtrop->agency, 0, sizeof(mtrop->agency));
	memset(mtrop->agencycode, 0, sizeof(mtrop->agencycode));
	memset(mtrop->solcon, 0, sizeof(mtrop->solcon));
	memset(mtrop->solfield, 0, sizeof(mtrop->solfield));
	memset(mtrop->tropmap, 0, sizeof(mtrop->tropmap));
	memset(mtrop->cfactor, 0, sizeof(mtrop->cfactor));

	for (i = 0; i < MAXIGSSTA; i++)
	{
		mtrop->tcoord[i] = tmp;
	}

	if (!(mtrop->tsol =
				(mgex_tropsol*)malloc(sizeof(mgex_tropsol) * MAXIGSSTA * NTROP)))
	{
		return 0;
	}

	return 1;
}

/* free troposphere sturct -----------------------------------------------------
* args     :       mgex_trop *mtrop	I/O     tropsphere sturct
* return   :       none
* ---------------------------------------------------------------------------*/
extern void freetrop(mgex_trop* mtrop)
{
	free(mtrop->tsol);
}

#if (0)
/* convert year, doy, sec to gps time ------------------------------------------
* args     :       int year	    I     year
*                  int doy         I	  day of year
*                  int sod         I 	  second of day
* return   :       gtime_t 	    O	  gps time
* ---------------------------------------------------------------------------*/
gtime_t doy2time(int year, int doy, int sod)
{
	const int dy[] = {1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335};
	int leap = 0, i, mon = 0, tmp, day;
	double ep[6];

	if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) leap = 1;

	for (i = 0; i < 12; i++)
	{
		if (leap == 1 && i >= 2)
			tmp = dy[i] + 1;
		else
			tmp = dy[i];

		if (doy >= tmp)
		{
			mon++;
			day = doy - tmp + 1;
		}
	}

	ep[0] = year;
	ep[1] = mon;
	ep[2] = day;
	ep[3] = (int)sod / 3600;
	ep[4] = (int)((sod - ep[3] * 3600) / 60);
	ep[5] = sod - ep[3] * 3600 - ep[4] * 60;

	return epoch2time(ep);
}

#endif
/* read tropsphere -------------------------------------------------------------
* args     :       FILE *fp	    	I/O     data source file
*                  mgex_trop *mtrop	I/O     tropsphere sturct
* return   :       0 fail, 1 success
* ---------------------------------------------------------------------------*/
extern int readtrop(FILE* fp, mgex_trop* mtrop)
{
	char buff[2048], *p, *q, t1[4], t2[22], t3[12], t4[2];
	int i, j, cindex = 0, sindex = 0, ci = 0, si = 0;

	/* read trop sinex file */
	while (fgets(buff, sizeof(buff), fp))
	{
		if (buff[0] == '*') continue;

		/* header block */
		if (strstr(buff, "%="))
		{
			if (strstr(buff, "TRO") && !strstr(buff, "%=ENDTRO"))
			{
				/* ID */
				strncpy(mtrop->id, buff, 5);
				/* version */
				strncpy(t1, buff + 6, 4);
				sscanf(t1, "%lf", &mtrop->ver);
				/* file agency code */
				strncpy(mtrop->agency, buff + 11, 3);
				/* creatation time YDS */
				mtrop->tbc[0] = (int)strtol(buff + 15, NULL, 10) + 2000;
				mtrop->tbc[1] = (int)strtol(buff + 18, NULL, 10);
				mtrop->tbc[2] = (int)strtol(buff + 22, NULL, 10);
				/* agency code */
				strncpy(mtrop->agencycode, buff + 28, 3);
				/* start time of solution */
				mtrop->tbs[0] = (int)strtol(buff + 32, NULL, 10) + 2000;
				mtrop->tbs[1] = (int)strtol(buff + 35, NULL, 10);
				mtrop->tbs[2] = (int)strtol(buff + 39, NULL, 10);
				/* end time of solution */
				mtrop->tbe[0] = (int)strtol(buff + 45, NULL, 10) + 2000;
				mtrop->tbe[1] = (int)strtol(buff + 48, NULL, 10);
				mtrop->tbe[2] = (int)strtol(buff + 52, NULL, 10);
				/* observation code */
				mtrop->obscode = buff[58];
				/* solution contents */
				strncpy(mtrop->solcon, buff + 60, 4);

				/* realloc memory if only one station (not supported) */
				if (!strstr(mtrop->solcon, "MIX"))
				{
				}
			}
			else if (strstr(buff, "%=ENDTRO"))
			{
				printf("End of trop\n");
				return 1;
			}
			else
				return 1;
		}
		/* description block */
		else if (strstr(buff, " SAMPLING INTERVAL"))
		{
			mtrop->intdata = (int)strtol(buff + 31, NULL, 10);
		}
		else if (strstr(buff, " SAMPLING TROP"))
		{
			mtrop->inttrop = (int)strtol(buff + 31, NULL, 10);
		}
		else if (strstr(buff, " TROP MAPPING FUNCTION"))
		{
			strncpy(mtrop->tropmap, buff + 31, 22);
		}
		else if (strstr(buff, " ELEVATION CUTOFF ANGLE"))
		{
			strncpy(t2, buff + 31, 22);
			sscanf(t2, "%lf", &mtrop->el);
		}
		else if (strstr(buff, " SOLUTION_FIELDS_1"))
		{
			j = 0;
			p = strtok(buff + 30, " ");

			for (i = 0; i < 7; i++)
			{
				if (p != NULL && strlen(p))
				{
					strcpy(mtrop->solfield[i], p);
					j += strlen(p) + 1;
				}
				else
					continue;

				p = strtok(buff + 30 + j + 1, " ");
			}
		}
		else if (strstr(buff, " SOLUTION_FIELDS_2"))
		{
			j = 0;
			p = strtok(buff + 30, " ");

			for (i = 0; i < 7; i++)
			{
				if (p != NULL && strlen(p))
				{
					strcpy(mtrop->solfield[i + 7], p);	//todo aaron, buffer overflows on strcpys in this file
					j += strlen(p) + 1;
				}
				else
					continue;

				p = strtok(buff + 30 + j + 1, " ");
			}
		}
		else if (strstr(buff, " BIAS FROM INTERVAL"))
		{

		}
		else if (strstr(buff, " DELETE FACTOR"))
		{

		}
		else if (strstr(buff, " CONVERSION FACTORS"))
		{

		}
		/* trop/sta coordinates block */
		else if (strstr(buff, "+TROP/STA_COORDINATES"))
		{
			cindex = 1;
		}
		else if (strstr(buff, "-TROP/STA_COORDINATES"))
		{
			cindex = 0;
		}
		else if (cindex == 1)
		{
			strncpy(mtrop->tcoord[ci].sitecode, buff + 1, 4);
			strncpy(mtrop->tcoord[ci].ptcode, buff + 6, 2);
			strncpy(mtrop->tcoord[ci].solid, buff + 9, 4);
			mtrop->tcoord[ci].obscode = buff[14];

			strncpy(t3, buff + 16, 12);
			sscanf(t3, "%lf", &mtrop->tcoord[ci].x[0]);
			strncpy(t3, buff + 29, 12);
			sscanf(t3, "%lf", &mtrop->tcoord[ci].x[1]);
			strncpy(t3, buff + 42, 12);
			sscanf(t3, "%lf", &mtrop->tcoord[ci].x[2]);

			strncpy(mtrop->tcoord[ci].sys, buff + 55, 6);
			strncpy(mtrop->tcoord[ci].remark, buff + 62, 5);

			strncpy(t4, buff + 69, 2);
			sscanf(t4, "%lf", &mtrop->tcoord[ci].std[0]);
			strncpy(t4, buff + 72, 2);
			sscanf(t4, "%lf", &mtrop->tcoord[ci].std[1]);
			strncpy(t4, buff + 75, 2);
			sscanf(t4, "%lf", &mtrop->tcoord[ci].std[2]);

			if (strstr(mtrop->solcon, "MIX"))
				ci++;

			if (ci >= MAXIGSSTA)
			{
				fprintf(stdout,
						"*** Overflow,increase MAXIGSSTA number ***\n");
				return 0;
			}
		}

		/* trop solution block */
		else if (strstr(buff, "+TROP/SOLUTION"))
		{
			sindex = 1;
		}
		else if (strstr(buff, "-TROP/SOLUTION"))
		{
			sindex = 0;
		}


		else if (sindex == 1)
		{
			q = NULL;
			strncpy(mtrop->tsol[si].marker, buff + 1, 4);

			mtrop->tsol[si].ts[0] = (int)strtol(buff + 6, NULL, 10) + 2000;
			mtrop->tsol[si].ts[1] = (int)strtol(buff + 9, NULL, 10);
			mtrop->tsol[si].ts[2] = (int)strtol(buff + 13, NULL, 10);

			p = strtok(buff + 19, " ");

			for (i = 0; i < 14; i++)
			{
				if (p != NULL && strlen(p))
				{
					sscanf(p, "%lf", &mtrop->tsol[si].x[i]);
				}
				else continue;

				p = strtok(buff + 19 + strlen(p) + 1, " ");
			}

			if (strstr(mtrop->solcon, "MIX"))
				si++;

			if (si >= MAXIGSSTA * NTROP)
			{
				fprintf(stdout,
						"*** Overflow,increase MAXIGSSTA number ***\n");
				return 0;
			}
		}
	}

	return 1;
}

#if (0)
int readtropmain()
{
	FILE* fptrop;
	mgex_trop* mtrop;

	mtrop = (mgex_trop*)malloc(sizeof(mgex_trop));

	if (!(fptrop = fopen("/opt/data/COD18540.TRO", "r")))
	{
		printf("No file/n");
	}

	if (!(inittrop(mtrop))) return 0;

	if (fptrop)
		readtrop(fptrop, mtrop);

	freetrop(mtrop);
	free(mtrop);
	fclose(fptrop);
	return 0;
}
#endif
