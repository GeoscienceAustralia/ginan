#include "minunit.hpp"
#include "../common/snx.hpp"
#include "navigation.hpp"

#include <iostream>
#include <unistd.h>

using std::cout;
//using std::cin;
using std::endl;
using std::string;

string snxfile;
// Check to see if we can get all of the right values for ALIC ALICE SPRINGS
MU_TEST(test_read_sinex_1)
{
	// const char snxfile[] = "/data/acs/pea/proc/exs/products/igs19P2062.snx";
	printf("\nTesting readsnx() for the station ALIC from file %s:\t", snxfile.c_str());
	snx_stn_snx_t station;
	int result = 0;

	if (access(snxfile.c_str(), F_OK) == 0)
	{
		result = read_sinex(snxfile, true);
		mu_assert_int_eq(0, result);
	}
    else
    {
        cout << "Cannot find " << snxfile.c_str() << "!!!" << endl;
		return;
	}

	// ----------------------------------------------------------
	// Does the SINEX file parse correctly?
	// -----------------------------------------------------------
// 	char *sta = new char [16];
	int yds[3];
	string line = "ALIC";
	yds[0] = 2019;
	yds[1] = 199;
	yds[2] = 0;
	int status = getstnsnx(line, yds, station);
	cout << "ALIC result" << result << endl;
// 	Will return 0 if all field sare found, returns 1 if there is a stuff up and > 1 if something is missing
	mu_assert_int_eq(E_SnxDataMissing::GPS_PHASE_CENTRE, status);
	mu_assert_string_eq("ALIC", 				station.sitecode.c_str());
	mu_assert_string_eq(" A", 					station.ptcode.c_str());
	mu_assert_string_eq("50137M001", 			station.monuid.c_str());

//	mu_assert_string_eq("LEICA GR25          ", station.rectype.c_str());
//	mu_assert_string_eq("18304", 				station.recsn.c_str());
//	mu_assert_string_eq("4.30.063/6.", 			station.recfirm.c_str());
// 	mu_assert_int_eq(17,snx[index]->recstart[0]);
// 	mu_assert_int_eq(54,snx[index]->recstart[1]);
// 	mu_assert_int_eq(0,snx[index]->recstart[2]);
// 	mu_assert_int_eq(0,snx[index]->recend[0]);
// 	mu_assert_int_eq(0,snx[index]->recend[1]);
// 	mu_assert_int_eq(0,snx[index]->recend[2]);

	mu_assert_string_eq("LEIAR25.R3      NONE", station.anttype.c_str());
	mu_assert_string_eq("09370", 				station.antsn.c_str());
// 	mu_assert_int_eq(11,snx[index]->antstart[0]);
// 	mu_assert_int_eq(201,snx[index]->antstart[1]);
// 	mu_assert_int_eq(0,snx[index]->antstart[2]);
// 	mu_assert_int_eq(0,snx[index]->antend[0]);
// 	mu_assert_int_eq(0,snx[index]->antend[1]);
// 	mu_assert_int_eq(0,snx[index]->antend[2]);
	char sta[] = "ALIC"; //ALIC  A 50137M001
// 	int index = 0;
// 	status = readsnx(snxfile, sta, snx[index].get());
// 	mu_assert_int_eq(1, status);
	mu_assert_string_eq("ALIC",sta);
// 	char    snxtype[MAXSNXSTR];    // SINEX file type
// 	double  ver;                // version
// 	int     np;                 // number of estimated parameters
// 	char    solcont[MAXSNXSTR];    // solution types S O E T C A
// 	mu_assert_string_eq("SNX",snx[index]->snxtype);
// 	mu_assert_double_eq("",snx[index]->ver);
// 	mu_assert_int_eq("",snx[index]->np);
// 	mu_assert_string_eq("",snx[index]->solcont);

// 	mu_assert_string_eq("ALIC",snx[index]->sitecode);
// 	needs to be ' A' mu_assert_string_eq("A",snx[index]->ptcode);
// 	mu_assert_string_eq("50137M001",snx[index]->monuid);
// 	mu_assert_string_eq("LEICA GR25          ",snx[index]->rectype);
// 	mu_assert_string_eq("18304",snx[index]->recsn);
// 	mu_assert_string_eq("4.11.606/6.",snx[index]->recfirm);
// 	mu_assert_int_eq(17,snx[index]->recstart[0]);
// 	mu_assert_int_eq(54,snx[index]->recstart[1]);
// 	mu_assert_int_eq(0,snx[index]->recstart[2]);
// 	mu_assert_int_eq(0,snx[index]->recend[0]);
// 	mu_assert_int_eq(0,snx[index]->recend[1]);
// 	mu_assert_int_eq(0,snx[index]->recend[2]);

// 	mu_assert_string_eq("LEIAR25.R3      NONE",snx[index]->anttype);
// 	mu_assert_string_eq("09370",snx[index]->antsn);
// 	mu_assert_int_eq(11,snx[index]->antstart[0]);
// 	mu_assert_int_eq(201,snx[index]->antstart[1]);
// 	mu_assert_int_eq(0,snx[index]->antstart[2]);
// 	mu_assert_int_eq(0,snx[index]->antend[0]);
// 	mu_assert_int_eq(0,snx[index]->antend[1]);
// 	mu_assert_int_eq(0,snx[index]->antend[2]);

	/* GPS phase_center block (no Galileo yet) */
	//double  gpsl1[3];           /* GPS L1 PCO UNE (m) */
	//double  gpsl2[3];           /* GPS L2 PCO UNE (m) */
	mu_assert_bool_eq(false, station.has_gps_pc);

	/* eccentricity block */
	//int     eccrs;              /* reference system UNE (0) or XYZ (1) */
	//double  ecc[3];             /* eccentricity UNE or XYZ (m) */
	mu_assert_bool_eq(true, station.has_ecc);

	/* solution/estimate block */
	//mu_assert_bool_eq(true, station.has_estimates);

	//int     solvalid[3];        /* solution valid time */
	//char    unit[MAXSNXSTR];       /* parameter units */
	//double  pos[3];             /* real position (ecef) (m)*/
	Vector3d std;
	//station.getPosEstimates(station.pos, std);
	//if (!station.estimates.empty())
//	{
//		mu_assert_double_eq_6dp(-4052052.7169461, station.pos(0));
//		mu_assert_double_eq_6dp(+4212835.9809269, station.pos(1));
//		mu_assert_double_eq_6dp(-2545104.6079740, station.pos(2));
//		mu_assert_double_eq_6dp(0.0002855, std(0));
//		mu_assert_double_eq_6dp(0.0002902, std(1));
//		mu_assert_double_eq_6dp(0.0002026, std(2));
//	}

}

MU_TEST(test_get_snx_siteid)
{
	//int status;
	//std::vector<std::unique_ptr<snx_t>> snx;
	//initialise the snx data
	//snx.push_back(std::make_unique<snx_t>());
	//char *sta = new char [16];
	//char sta[] = "ALIC"; //ALIC  A 50137M001
	//int index = 0;
	printf("\nTesting snxGetSites() from site id block in file %s:\t", snxfile.c_str());
	snx_stn_snx_t station;
	int result = 0;
	int yds[3];

	/*
	 * just re-using same file as previous test - don't reload
	sinex_reset();

	if (access(snxfile, F_OK) == 0){
	    //int result;
		result = read_sinex(snxfile);

	    mu_assert_int_eq(0, result);
	}
	else {
		cout << "Cannot find " << snxfile << "!!!" << endl;
		return;
	}
	 */
	//mu_assert_int_eq(1, status);

	//snx_siteid_t snxid;
	//snxid = {};
	//std::vector<std::unique_ptr<snx_siteid_t>> vec_snxid;
	//vec_snxid.push_back(std::make_unique<snx_siteid_t>());

	//int nSTA = sinex_site_count();
	//mu_assert_int_eq(542, nSTA);

	yds[0] = 2019;
	yds[1] = 199;
	yds[2] = 0;

	string s;

	getstnsnx("ALBH", yds, station);
	mu_assert_string_eq		("ALBH", 					station.sitecode.c_str());
	mu_assert_string_eq		(" A", 						station.ptcode.c_str());
	mu_assert_string_eq		("40129M003", 				station.monuid.c_str());
	s += station.typecode;
	mu_assert_string_eq		("P", 						s.c_str());
	mu_assert_string_eq		("VICTORIA/SIDNEY, CANAD", 	station.desc.c_str());
	mu_assert_int_eq		(236, 						station.long_deg);
	mu_assert_int_eq		(30, 						station.long_min);
	mu_assert_double_eq_1dp	(45.1, 						station.long_sec);
	mu_assert_int_eq		(48, 						station.lat_deg);
	mu_assert_int_eq		(23, 						station.lat_min);
	mu_assert_double_eq_1dp	(23.2, 						station.lat_sec);
	mu_assert_double_eq_1dp	(31.7, 						station.height);
}

MU_TEST(test_read_sinex_all)
{
	// const char snxfile[] = "/data/acs/pea/proc/exs/products/AUS20127.SNX";

	printf("\nTesting sinex merge with file %s", snxfile.c_str());
	snx_stn_snx_t station;
	int result = 0;
	int yds[3];
	int yds2[3];

	yds[0] = 2019;
	yds[1] = 199;
	yds[2] = 0;
	yds2[0] = 2020;
	yds2[1] = 120;
	yds2[2] = 0;

	result =	getstnsnx("AUCK", yds, station);
	mu_assert_int_eq		(E_SnxDataMissing::GPS_PHASE_CENTRE, result);
	mu_assert_string_eq		("AUCK", 	station.sitecode.c_str());
	mu_assert_int_eq		(174, 		station.long_deg);
	mu_assert_int_eq		(50, 		station.long_min);
	mu_assert_double_eq_1dp	(3.80, 		station.long_sec);
	mu_assert_int_eq		(-36, 		station.lat_deg);
	mu_assert_int_eq		(36, 		station.lat_min);
	mu_assert_double_eq_1dp	(10.20, 	station.lat_sec);
	mu_assert_double_eq_1dp	(132.7, 	station.height);
//	mu_assert_bool_eq		(true, 		!station.estimates.empty());

//	if (!station.estimates.empty())
//	{
//		Vector3d std;
//		station.getPosEstimates(station.pos, std);
//		mu_assert_double_eq_6dp(-5105681.5224144, 	station.pos(0));
//		mu_assert_double_eq_6dp(+461563.9940242, 	station.pos(1));
//		mu_assert_double_eq_6dp(-3782181.0162940, 	station.pos(2));
//	}

//	mu_assert_string_eq("TRIMBLE NETR9       ", station.rectype.c_str());
//	mu_assert_string_eq("5035K", 				station.recsn.c_str());
//	mu_assert_string_eq("5.22", 				station.recfirm.c_str());
	mu_assert_string_eq("TRM57971.00     NONE", station.anttype.c_str());
	mu_assert_string_eq("14410", 				station.antsn.c_str());

	result = getstnsnx("AUCK", yds2, station);
	mu_assert_int_eq		(E_SnxDataMissing::GPS_PHASE_CENTRE, 		result);
	mu_assert_string_eq		("AUCK", 	station.sitecode.c_str());
	mu_assert_int_eq		(174, 		station.long_deg);
	mu_assert_int_eq		(50, 		station.long_min);
	mu_assert_double_eq_1dp	(3.80, 		station.long_sec);
	mu_assert_int_eq		(-36, 		station.lat_deg);
	mu_assert_int_eq		(36, 		station.lat_min);
	mu_assert_double_eq_1dp	(10.20, 	station.lat_sec);
	mu_assert_double_eq_1dp	(132.7, 	station.height);
//	mu_assert_bool_eq		(true, 		!station.estimates.empty());

//	if (!station.estimates.empty())
//	{
//		Vector3d std;
//		station.getPosEstimates(station.pos, std);
//		mu_assert_double_eq_6dp(-5105681.5224144, 	station.pos(0));
//		mu_assert_double_eq_6dp(461563.9940242, 	station.pos(1));
//		mu_assert_double_eq_6dp(-3782181.0162940, 	station.pos(2));
//	}

//	mu_assert_string_eq("TRIMBLE NETR9       ", station.rectype.c_str());
//	mu_assert_string_eq("5035K", 				station.recsn.c_str());
//	mu_assert_string_eq("5.22", 				station.recfirm.c_str());
	mu_assert_string_eq("TRM57971.00     NONE", station.anttype.c_str());
	mu_assert_string_eq("14410", 				station.antsn.c_str());

// 	check file exists
	if (access(snxfile.c_str(), F_OK) == 0)
	{
		//int result;
		result = read_sinex(snxfile, true);

		mu_assert_int_eq(0, result);
	}
	else
	{
		cout << "Cannot find " << snxfile.c_str() << "!!!" << endl;
		return;
	}

	result =	getstnsnx("AUCK", yds, station);
	mu_assert_int_eq		(E_SnxDataMissing::GPS_PHASE_CENTRE, 		result);
	mu_assert_string_eq		("AUCK", 	station.sitecode.c_str());
	mu_assert_int_eq		(174, 		station.long_deg);
	mu_assert_int_eq		(50, 		station.long_min);
	mu_assert_double_eq_1dp	(3.80, 		station.long_sec);
	mu_assert_int_eq		(-36, 		station.lat_deg);
	mu_assert_int_eq		(36, 		station.lat_min);
	mu_assert_double_eq_1dp	(10.20, 	station.lat_sec);
	mu_assert_double_eq_1dp	(132.7, 	station.height);
//	mu_assert_bool_eq		(true, 		!station.estimates.empty());

//	if (!station.estimates.empty())
//	{
//		Vector3d std;
//		station.getPosEstimates(station.pos, std);
//		mu_assert_double_eq_6dp(-5105681.5224144, station.pos(0));
//		mu_assert_double_eq_6dp(461563.9940242, station.pos(1));
//		mu_assert_double_eq_6dp(-3782181.0162940, station.pos(2));
//	}

//	mu_assert_string_eq("TRIMBLE NETR9       ",	station.rectype.c_str());
//	mu_assert_string_eq("5035K",				station.recsn.c_str());
//	mu_assert_string_eq("5.22",					station.recfirm.c_str());
	mu_assert_string_eq("TRM57971.00     NONE", station.anttype.c_str());
	mu_assert_string_eq("14410",				station.antsn.c_str());

	result = getstnsnx("AUCK", yds2, station);
	mu_assert_int_eq		(E_SnxDataMissing::GPS_PHASE_CENTRE, 		result);
	mu_assert_string_eq		("AUCK", 	station.sitecode.c_str());
	mu_assert_int_eq		(174, 		station.long_deg);
	mu_assert_int_eq		(50, 		station.long_min);
	mu_assert_double_eq_1dp	(3.80, 		station.long_sec);
	mu_assert_int_eq		(-36, 		station.lat_deg);
	mu_assert_int_eq		(36, 		station.lat_min);
	mu_assert_double_eq_1dp	(10.20, 	station.lat_sec);
	mu_assert_double_eq_1dp	(132.7, 	station.height);
//	mu_assert_bool_eq		(true, 		!station.estimates.empty());

//	if (!station.estimates.empty())
//	{
//		Vector3d std;
//		station.getPosEstimates(station.pos, std);
//		mu_assert_double_eq_6dp(-5105681.5224144, 	station.pos(0));
//		mu_assert_double_eq_6dp(461563.9940242, 	station.pos(1));
//		mu_assert_double_eq_6dp(-3782181.0162940, 	station.pos(2));
//	}

//	mu_assert_string_eq("TRIMBLE NETR9       ", station.rectype.c_str());
//	mu_assert_string_eq("5035K", 				station.recsn.c_str());
//	mu_assert_string_eq("5.22", 				station.recfirm.c_str());
	mu_assert_string_eq("TRM57971.00     NONE", station.anttype.c_str());
	mu_assert_string_eq("14410", 				station.antsn.c_str());


	printf("\n\tSite id block  : ");
// 	mu_assert_int_eq(1305,snx.vec_siteid.size());
// 	mu_assert_string_eq("ALBH",snx.vec_siteid[0].sitecode);
// 	mu_assert_string_eq("A ",snx.vec_siteid[0].ptcode);
// 	mu_assert_string_eq("40129M003",snx.vec_siteid[0].domes);
// 	mu_assert_string_eq("C",snx.vec_siteid[0].typecode);
// 	mu_assert_string_eq("VICTORIA/SIDNEY, CANAD",snx.vec_siteid[0].desc);
// 	mu_assert_string_eq("236",snx.vec_siteid[0].long_deg);
// 	mu_assert_string_eq("30",snx.vec_siteid[0].long_min);
// 	mu_assert_double_eq_1dp(45.1,snx.vec_siteid[0].long_sec);
// 	mu_assert_string_eq(" 48",snx.vec_siteid[0].lat_deg);
// 	mu_assert_string_eq("23",snx.vec_siteid[0].lat_min);
// 	mu_assert_double_eq_1dp(23.2,snx.vec_siteid[0].lat_sec);
// 	mu_assert_double_eq_1dp(31.7,snx.vec_siteid[0].height);

	printf("\n\tReceiver block : ");
// 	mu_assert_int_eq(9153,snx.vec_receivers.size());
// 	mu_assert_string_eq("ALBH",snx.vec_receivers[0].sitecode);
// 	mu_assert_string_eq(" A",snx.vec_receivers[0].ptcode);
// 	mu_assert_int_eq(0,snx.vec_receivers[0].solnnum);
// 	mu_assert_string_eq("C",snx.vec_receivers[0].typecode);
// 	mu_assert_int_eq(93,snx.vec_receivers[0].recstart[0]);
// 	mu_assert_int_eq(327,snx.vec_receivers[0].recstart[1]);
// 	mu_assert_int_eq(70260,snx.vec_receivers[0].recstart[2]);
// 	mu_assert_int_eq(94,snx.vec_receivers[0].recend[0]);
// 	mu_assert_int_eq(16,snx.vec_receivers[0].recend[1]);
// 	mu_assert_int_eq(15540,snx.vec_receivers[0].recend[2]);
// 	mu_assert_string_eq("ROGUE SNR-8C        ",snx.vec_receivers[0].rectype);
// 	mu_assert_string_eq("313  ",snx.vec_receivers[0].recsn);
// 	mu_assert_string_eq("Meenix 7.4 ",snx.vec_receivers[0].recfirm);

	printf("\n\tAntenna block  : ");
// 	mu_assert_int_eq(2860,snx.vec_antennas.size());
// 	mu_assert_string_eq("ALBH",snx.vec_antennas[0].sitecode);
// 	mu_assert_string_eq(" A",snx.vec_antennas[0].ptcode);
// 	mu_assert_int_eq(0,snx.vec_antennas[0].solnnum);
// 	mu_assert_string_eq("C",snx.vec_antennas[0].typecode);
// 	mu_assert_int_eq(92,snx.vec_antennas[0].antstart[0]);
// 	mu_assert_int_eq(125,snx.vec_antennas[0].antstart[1]);
// 	mu_assert_int_eq(0,snx.vec_antennas[0].antstart[2]);
// 	mu_assert_int_eq(94,snx.vec_antennas[0].antend[0]);
// 	mu_assert_int_eq(104,snx.vec_antennas[0].antend[1]);
// 	mu_assert_int_eq(74100,snx.vec_antennas[0].antend[2]);
// 	mu_assert_string_eq("AOAD/M_B        EMRA",snx.vec_antennas[0].anttype);
// 	mu_assert_string_eq("91119",snx.vec_antennas[0].antsn);

	printf("\n\tGPS_PHASE block: ");
	printf("\n\tECC block      : ");
// 	mu_assert_int_eq(1712,snx.vec_eccs.size());
// 	mu_assert_string_eq("ALBH",snx.vec_eccs[0].sitecode);
// 	mu_assert_string_eq(" A",snx.vec_eccs[0].ptcode);
// 	mu_assert_int_eq(0,snx.vec_eccs[0].solnnum);
// 	mu_assert_string_eq("C",snx.vec_eccs[0].typecode);
// 	mu_assert_int_eq(92,snx.vec_eccs[0].eccstart[0]);
// 	mu_assert_int_eq(146,snx.vec_eccs[0].eccstart[1]);
// 	mu_assert_int_eq(0,snx.vec_eccs[0].eccstart[2]);
// 	mu_assert_int_eq(94,snx.vec_eccs[0].eccend[0]);
// 	mu_assert_int_eq(104,snx.vec_eccs[0].eccend[1]);
// 	mu_assert_int_eq(74100,snx.vec_eccs[0].eccend[2]);
// 	mu_assert_string_eq("UNE",snx.vec_eccs[0].eccrs);
// 	mu_assert_double_eq_4dp(0.1260,snx.vec_eccs[0].ecc[0]);
// 	mu_assert_double_eq_4dp(0.0000,snx.vec_eccs[0].ecc[1]);
// 	mu_assert_double_eq_4dp(0.0000,snx.vec_eccs[0].ecc[2]);

	printf("\n\tSol/epoch block: ");
// 	mu_assert_int_eq(3826,snx.vec_solepochs.size());
// 	mu_assert_string_eq("ALBH",snx.vec_solepochs[0].sitecode);
// 	mu_assert_string_eq(" A",snx.vec_solepochs[0].ptcode);
// 	mu_assert_int_eq(1,snx.vec_solepochs[0].solnnum);
// 	mu_assert_string_eq("C",snx.vec_solepochs[0].typecode);
// 	mu_assert_int_eq(94,snx.vec_solepochs[0].start[0]);
// 	mu_assert_int_eq(2,snx.vec_solepochs[0].start[1]);
// 	mu_assert_int_eq(0,snx.vec_solepochs[0].start[2]);
// 	mu_assert_int_eq(94,snx.vec_solepochs[0].end[0]);
// 	mu_assert_int_eq(104,snx.vec_solepochs[0].end[1]);
// 	mu_assert_int_eq(0,snx.vec_solepochs[0].end[2]);
// 	mu_assert_int_eq(94,snx.vec_solepochs[0].mean[0]);
// 	mu_assert_int_eq(53,snx.vec_solepochs[0].mean[1]);
// 	mu_assert_int_eq(0,snx.vec_solepochs[0].mean[2]);

	printf("\n\tSolution block : ");
// 	mu_assert_int_eq(22956,snx.vec_sols.size());
// 	mu_assert_int_eq(1,snx.vec_sols[0].index);
// 	mu_assert_string_eq("STAX  ",snx.vec_sols[0].type);
// 	mu_assert_string_eq("ALBH",snx.vec_sols[0].sitecode);
// 	mu_assert_string_eq(" A",snx.vec_sols[0].ptcode);
// 	mu_assert_int_eq(10,snx.vec_sols[0].refepoch[0]);
// 	mu_assert_int_eq(1,snx.vec_sols[0].refepoch[1]);
// 	mu_assert_int_eq(0,snx.vec_sols[0].refepoch[2]);
// 	mu_assert_string_eq("m   ",snx.vec_sols[0].unit);
// 	mu_assert_string_eq("2",snx.vec_sols[0].S);


}

MU_TEST_SUITE(test_suite)
{
	snxfile = "/data/acs/pea/proc/exs/products/igs19P2062.snx";
	MU_RUN_TEST(test_read_sinex_1);
	MU_RUN_TEST(test_get_snx_siteid); // no snx reset
	MU_RUN_TEST(test_read_sinex_all); // no snx reset

    // snxfile = "/data/acs/pea/proc/exs/products/AUS20127.SNX";
	// MU_RUN_TEST(test_read_sinex_1);
	// MU_RUN_TEST(test_read_sinex_all); // no snx reset
}
nav_t nav = {};

int main(int argc, char* argv[])
{
	MU_RUN_SUITE(test_suite);
	MU_REPORT();
	return 0;
}

