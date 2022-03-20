#include "minunit.h"

#include "antenna.hpp"

MU_TEST(test_read_antenna_igs14) {

    int status;
    double t1[6] = {2000,01,01,0,0,0};
    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;

    /* ----------------------------------------------------------
    * Does the ANTEX file parse correctly?
    * -----------------------------------------------------------*/
    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
    mu_assert_int_eq(1, status);

    /*----------------------------------------------------------
    / Test the values for the TRM59800.00     NONE
    /----------------------------------------------------------*/
    pcv = findantenna("TRM59800.00     NONE",NULL,t1,&ds_pcvs,0);
    //extern pcvacs_t *findantenna(const char *type, const char *code, const double tc[6], const pcvacss_t *pcvs, const int id);
    /* Do we get the right type back */
    mu_assert_string_eq("TRM59800.00     NONE", pcv->type);
    /* Does it have a serial number */
    mu_assert_string_eq("                    ", pcv->code);
    /* When is this calibraion valid from - important for satellites */
    //mu_assert_string_eq("                    ", pcv->ts);
    // When is this calibraion valid until - important for satellites
    //mu_assert_string_eq("                    ", pcv->te);

    //--------------------------------
    // read_antex missing functions ..
    //--------------------------------
    // TODO read methodology, by and number of cals, date of cals
    //mu_assert_string_eq("ROBOT",pcv->cal_method);
    //mu_assert_string_eq("Geo++ GmbH",pcv->cal_by);
    //mu_assert_string_eq("28",pcv->cal_completed);
    //mu_assert_string_eq("29-JAN-17",pcv->cal_date);
    // add read of DAZI
    mu_assert_double_eq(5.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(90.0,pcv->zen[1]);
    mu_assert_double_eq(5.0,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(19, pcv->nz);
    // TODO read SINEX code
    //mu_assert_string_eq("IGS14_2000",pcv->sinex_code);

    // read valid from
    mu_assert_double_eq(0.0,pcv->tf[0]);
    mu_assert_double_eq(0.0,pcv->tf[1]);
    mu_assert_double_eq(0.0,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(0.0,pcv->tu[0]);
    mu_assert_double_eq(0.0,pcv->tu[1]);
    mu_assert_double_eq(0.0,pcv->tu[2]);
    mu_assert_double_eq(0.0,pcv->tu[3]);
    mu_assert_double_eq(0.0,pcv->tu[4]);
    mu_assert_double_eq(0.0,pcv->tu[5]);
    // Check the PCO offsets for each frequency
    // in antex it is neu, in this data structure it is enu
    //printf("\nTesting TRM59800.00 NONE PCO G01\n");
    mu_assert_double_eq(0.00104,pcv->pco[0][1]);
    mu_assert_double_eq(0.00070,pcv->pco[0][0]);
    mu_assert_double_eq(0.08951,pcv->pco[0][2]);
    // L2 offsets
    //printf("\nTesting TRM59800.00 NONE PCO G02\n");
    mu_assert_double_eq(0.000120,pcv->pco[1][1]);
    mu_assert_double_eq(0.0000,pcv->pco[1][0]);
    mu_assert_double_eq(0.11713,pcv->pco[1][2]);
    // R1 offsets
    //printf("\nTesting TRM59800.00 NONE PCO R01\n");
    mu_assert_double_eq(0.00104,pcv->pco[2][1]);
    mu_assert_double_eq(0.0007,pcv->pco[2][0]);
    mu_assert_double_eq(0.08951,pcv->pco[2][2]);
    // R1 offsets
    //printf("\nTesting TRM59800.00 NONE PCO R02\n");
    mu_assert_double_eq(0.00012,pcv->pco[3][1]);
    mu_assert_double_eq(0.0,pcv->pco[3][0]);
    mu_assert_double_eq(0.11713,pcv->pco[3][2]);

    // Check the PCV variaions are correct at azimuth = 0
    //printf("\nTesting TRM59800.00 NONE PCV G01\n");
    mu_assert_double_eq(0.0,pcv->recpcv[0][0][0]);
    mu_assert_double_eq(-0.00024,pcv->recpcv[0][1][0]);
    mu_assert_double_eq(-0.00090,pcv->recpcv[0][2][0]);
    mu_assert_double_eq(-0.00193,pcv->recpcv[0][3][0]);
    mu_assert_double_eq(-0.00323,pcv->recpcv[0][4][0]);
    mu_assert_double_eq(-0.00466,pcv->recpcv[0][5][0]);
    mu_assert_double_eq(-0.00604,pcv->recpcv[0][6][0]);
    mu_assert_double_eq(-0.00717,pcv->recpcv[0][7][0]);
    mu_assert_double_eq(-0.00790,pcv->recpcv[0][8][0]);
    mu_assert_double_eq(-0.00814,pcv->recpcv[0][9][0]);
    mu_assert_double_eq(-0.00789,pcv->recpcv[0][10][0]);
    mu_assert_double_eq(-0.00719,pcv->recpcv[0][11][0]);
    mu_assert_double_eq(-0.00611,pcv->recpcv[0][12][0]);
    mu_assert_double_eq(-0.00462,pcv->recpcv[0][13][0]);
    mu_assert_double_eq(-0.00259,pcv->recpcv[0][14][0]);
    mu_assert_double_eq(0.00024,pcv->recpcv[0][15][0]);
    mu_assert_double_eq(0.00411,pcv->recpcv[0][16][0]);
    mu_assert_double_eq(0.00917,pcv->recpcv[0][17][0]);
    mu_assert_double_eq(0.01528,pcv->recpcv[0][18][0]);

    // Check G02
    //printf("\nTesting TRM59800.00 NONE PCV G02\n");
    mu_assert_double_eq(0.00,pcv->recpcv[1][0][0]);
    mu_assert_double_eq(-0.00014,pcv->recpcv[1][1][0]);
    mu_assert_double_eq(-0.00053,pcv->recpcv[1][2][0]);
    mu_assert_double_eq(-0.00114,pcv->recpcv[1][3][0]);
    mu_assert_double_eq(-0.00189,pcv->recpcv[1][4][0]);
    mu_assert_double_eq(-0.00273,pcv->recpcv[1][5][0]);
    mu_assert_double_eq(-0.00358,pcv->recpcv[1][6][0]);
    mu_assert_double_eq(-0.00438,pcv->recpcv[1][7][0]);
    mu_assert_double_eq(-0.00502,pcv->recpcv[1][8][0]);
    mu_assert_double_eq(-0.00539,pcv->recpcv[1][9][0]);
    mu_assert_double_eq(-0.00537,pcv->recpcv[1][10][0]);
    mu_assert_double_eq(-0.00489,pcv->recpcv[1][11][0]);
    mu_assert_double_eq(-0.00398,pcv->recpcv[1][12][0]);
    mu_assert_double_eq(-0.00272,pcv->recpcv[1][13][0]);
    mu_assert_double_eq(-0.00122,pcv->recpcv[1][14][0]);
    mu_assert_double_eq(0.00055,pcv->recpcv[1][15][0]);
    mu_assert_double_eq(0.00282,pcv->recpcv[1][16][0]);
    mu_assert_double_eq(0.00604,pcv->recpcv[1][17][0]);
    mu_assert_double_eq(0.01076,pcv->recpcv[1][18][0]);

    // Check R01
    //printf("\nTesting TRM59800.00 NONE PCV R01\n");
    mu_assert_double_eq(0.00,pcv->recpcv[2][0][0]);
    mu_assert_double_eq(-0.00025,pcv->recpcv[2][1][0]);
    mu_assert_double_eq(-0.00094,pcv->recpcv[2][2][0]);
    mu_assert_double_eq(-0.00202,pcv->recpcv[2][3][0]);
    mu_assert_double_eq(-0.00338,pcv->recpcv[2][4][0]);
    mu_assert_double_eq(-0.00488,pcv->recpcv[2][5][0]);
    mu_assert_double_eq(-0.00636,pcv->recpcv[2][6][0]);
    mu_assert_double_eq(-0.00760,pcv->recpcv[2][7][0]);
    mu_assert_double_eq(-0.00844,pcv->recpcv[2][8][0]);
    mu_assert_double_eq(-0.00879,pcv->recpcv[2][9][0]);
    mu_assert_double_eq(-0.00862,pcv->recpcv[2][10][0]);
    mu_assert_double_eq(-0.00800,pcv->recpcv[2][11][0]);
    mu_assert_double_eq(-0.00699,pcv->recpcv[2][12][0]);
    mu_assert_double_eq(-0.00559,pcv->recpcv[2][13][0]);
    mu_assert_double_eq(-0.00370,pcv->recpcv[2][14][0]);
    mu_assert_double_eq(-0.00103,pcv->recpcv[2][15][0]);
    mu_assert_double_eq(0.00267,pcv->recpcv[2][16][0]);
    mu_assert_double_eq(0.00765,pcv->recpcv[2][17][0]);
    mu_assert_double_eq(0.01379,pcv->recpcv[2][18][0]);
    // Check R02
    //printf("\nTesting TRM59800.00 NONE PCV R01\n");
    mu_assert_double_eq(0.00,pcv->recpcv[3][0][0]);
    mu_assert_double_eq(-0.00015,pcv->recpcv[3][1][0]);
    mu_assert_double_eq(-0.00059,pcv->recpcv[3][2][0]);
    mu_assert_double_eq(-0.00128,pcv->recpcv[3][3][0]);
    mu_assert_double_eq(-0.00213,pcv->recpcv[3][4][0]);
    mu_assert_double_eq(-0.00308,pcv->recpcv[3][5][0]);
    mu_assert_double_eq(-0.00404,pcv->recpcv[3][6][0]);
    mu_assert_double_eq(-0.00496,pcv->recpcv[3][7][0]);
    mu_assert_double_eq(-0.00575,pcv->recpcv[3][8][0]);
    mu_assert_double_eq(-0.00629,pcv->recpcv[3][9][0]);
    mu_assert_double_eq(-0.00645,pcv->recpcv[3][10][0]);
    mu_assert_double_eq(-0.00616,pcv->recpcv[3][11][0]);
    mu_assert_double_eq(-0.00540,pcv->recpcv[3][12][0]);
    mu_assert_double_eq(-0.00424,pcv->recpcv[3][13][0]);
    mu_assert_double_eq(-0.00278,pcv->recpcv[3][14][0]);
    mu_assert_double_eq(-0.00103,pcv->recpcv[3][15][0]);
    mu_assert_double_eq(0.00116,pcv->recpcv[3][16][0]);
    mu_assert_double_eq(0.00410,pcv->recpcv[3][17][0]);
    mu_assert_double_eq(0.00823,pcv->recpcv[3][18][0]);

}

MU_TEST(igs14_satellite_G01) {

    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;
    int status;

    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
    mu_assert_int_eq(1, status);

    double t1[6] = {2000,01,01,0,0,0};
    /*----------------------------------------------------------
    // Test the values for the GPS01
    //----------------------------------------------------------*/
    pcv = findantenna("","G01",t1, &ds_pcvs,1);

    /* Do we get the right type back */
    mu_assert_string_eq("BLOCK IIA           ", pcv->type);
    // Does it have a serial number
    mu_assert_string_eq("G01                 ", pcv->code);

    // read valid from
    mu_assert_double_eq(1992,pcv->tf[0]);
    mu_assert_double_eq(11,pcv->tf[1]);
    mu_assert_double_eq(22,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(2008,pcv->tu[0]);
    mu_assert_double_eq(10,pcv->tu[1]);
    mu_assert_double_eq(16,pcv->tu[2]);
    mu_assert_double_eq(23,pcv->tu[3]);
    mu_assert_double_eq(59,pcv->tu[4]);
    mu_assert_double_eq(59,pcv->tu[5]);

    // add read of DAZI
    mu_assert_double_eq(0.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(17.0,pcv->zen[1]);
    mu_assert_double_eq(1.0,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(18, pcv->nz);
    // Test G01 PCO
    mu_assert_double_eq(0.27900,pcv->pco[0][1]);
    mu_assert_double_eq(0.00000,pcv->pco[0][0]);
    mu_assert_double_eq(2.3195,pcv->pco[0][2]);

    // TEST G02 PCO
    mu_assert_double_eq(0.27900,pcv->pco[1][1]);
    mu_assert_double_eq(0.00000,pcv->pco[1][0]);
    mu_assert_double_eq(2.3195,pcv->pco[1][2]);

    // TEST G01 PCV
    mu_assert_double_eq(-0.0008,pcv->satpcv[0][0]);
    mu_assert_double_eq(-0.0009,pcv->satpcv[0][1]);
    mu_assert_double_eq( 0.0013,pcv->satpcv[0][7]);

    //TEST G02 PCV
    mu_assert_double_eq(-0.0004,pcv->satpcv[0][4]);
    mu_assert_double_eq( 0.0002,pcv->satpcv[0][5]);
    mu_assert_double_eq( 0.0000,pcv->satpcv[0][11]);

}

MU_TEST(igs14_satellite_G01_t2) {

    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;
    int status;

    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
    //status = readantexf("data/antenna/igs14_2000.atx",&ds_pcvs);
    mu_assert_int_eq(1, status);
    /*----------------------------------------------------------
    // Test the values for the GPS01 at a different date
    //----------------------------------------------------------*/
    double t2[6] = {2010,11,04,3,9,59};
    pcv = findantenna("","G01",t2, &ds_pcvs,1);

    /* Do we get the right type back */
    mu_assert_string_eq("BLOCK IIR-M         ", pcv->type);
    // Does it have a serial number
    mu_assert_string_eq("G01                 ", pcv->code);

    // read valid from
    mu_assert_double_eq(2009,pcv->tf[0]);
    mu_assert_double_eq(3,pcv->tf[1]);
    mu_assert_double_eq(24,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(2011,pcv->tu[0]);
    mu_assert_double_eq(5,pcv->tu[1]);
    mu_assert_double_eq(6,pcv->tu[2]);
    mu_assert_double_eq(23,pcv->tu[3]);
    mu_assert_double_eq(59,pcv->tu[4]);
    mu_assert_double_eq(59,pcv->tu[5]);

    // add read of DAZI
    mu_assert_double_eq(0.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(17.0,pcv->zen[1]);
    mu_assert_double_eq(1.0,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(18, pcv->nz);
    // Test G01 PCO
    //mu_assert_double_eq(0.27900,pcv->pco[0][1]);
    //mu_assert_double_eq(0.00000,pcv->pco[0][0]);
    //mu_assert_double_eq(2.3195,pcv->pco[0][2]);

    // TEST G02 PCO
    //mu_assert_double_eq(0.27900,pcv->pco[1][1]);
    //mu_assert_double_eq(0.00000,pcv->pco[1][0]);
    //mu_assert_double_eq(2.3195,pcv->pco[1][2]);

    // TEST G01 PCV
    //mu_assert_double_eq(-0.0008,pcv->satpcv[0][0]);
    //mu_assert_double_eq(-0.0009,pcv->satpcv[0][1]);
    //mu_assert_double_eq( 0.0013,pcv->satpcv[0][7]);

    //TEST G02 PCV
    //mu_assert_double_eq(-0.0004,pcv->satpcv[0][4]);
    //mu_assert_double_eq( 0.0002,pcv->satpcv[0][5]);
    //mu_assert_double_eq( 0.0000,pcv->satpcv[0][11]);

}

MU_TEST(igs14_satellite_R01) {

    int status;
    double t1[6] = {2000,01,01,0,0,0};

    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;

    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
    //status = readantexf("data/antenna/igs14_2000.atx",&ds_pcvs);
    mu_assert_int_eq(1, status);

    /*----------------------------------------------------------
    // Test the values for the GPS01
    //----------------------------------------------------------*/
    pcv = findantenna("","R01",t1, &ds_pcvs,1);

    /* Do we get the right type back */
    mu_assert_string_eq("GLONASS             ", pcv->type);
    // Does it have a serial number
    mu_assert_string_eq("R01                 ", pcv->code);

    // read valid from
    mu_assert_double_eq(1998,pcv->tf[0]);
    mu_assert_double_eq(12,pcv->tf[1]);
    mu_assert_double_eq(30,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(2004,pcv->tu[0]);
    mu_assert_double_eq(12,pcv->tu[1]);
    mu_assert_double_eq(25,pcv->tu[2]);
    mu_assert_double_eq(23,pcv->tu[3]);
    mu_assert_double_eq(59,pcv->tu[4]);
    mu_assert_double_eq(59,pcv->tu[5]);

    // add read of DAZI
    mu_assert_double_eq(0.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(15.0,pcv->zen[1]);
    mu_assert_double_eq(1.0,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(16, pcv->nz);
    // Test G01 PCO
    mu_assert_double_eq(0.00000,pcv->pco[0][1]);
    mu_assert_double_eq(0.00000,pcv->pco[0][0]);
    mu_assert_double_eq(2.08410,pcv->pco[0][2]);

    // TEST G02 PCO
    mu_assert_double_eq(0.00000,pcv->pco[1][1]);
    mu_assert_double_eq(0.00000,pcv->pco[1][0]);
    mu_assert_double_eq(2.08410,pcv->pco[1][2]);

    // TEST G01 PCV
    mu_assert_double_eq( 0.0019,pcv->satpcv[0][0]);
    mu_assert_double_eq( 0.0015,pcv->satpcv[0][1]);
    mu_assert_double_eq(-0.0011,pcv->satpcv[0][7]);
    mu_assert_double_eq( 0.0022,pcv->satpcv[0][15]);

    //TEST G02 PCV
    mu_assert_double_eq( 0.0002,pcv->satpcv[1][4]);
    mu_assert_double_eq(-0.0002,pcv->satpcv[1][5]);
    mu_assert_double_eq(-0.0016,pcv->satpcv[1][11]);
    mu_assert_double_eq( 0.0022,pcv->satpcv[1][15]);

}

MU_TEST(igs14_satellite_R01_t2) {

    int status;
    double t1[6] = {2010,01,01,0,0,0};

    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;

    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
    //status = readantexf("data/antenna/igs14_2000.atx",&ds_pcvs);
    mu_assert_int_eq(1, status);

    /*----------------------------------------------------------
    // Test the values for the GPS01
    //----------------------------------------------------------*/
    pcv = findantenna("","R01",t1, &ds_pcvs,1);

    /* Do we get the right type back */
    mu_assert_string_eq("GLONASS-M           ", pcv->type);
    // Does it have a serial number
    mu_assert_string_eq("R01                 ", pcv->code);

    // read valid from
    mu_assert_double_eq(2009,pcv->tf[0]);
    mu_assert_double_eq(12,pcv->tf[1]);
    mu_assert_double_eq(14,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(0.,pcv->tu[0]);
    mu_assert_double_eq(0.,pcv->tu[1]);
    mu_assert_double_eq(0.,pcv->tu[2]);
    mu_assert_double_eq(0.,pcv->tu[3]);
    mu_assert_double_eq(0.,pcv->tu[4]);
    mu_assert_double_eq(0.,pcv->tu[5]);

    // add read of DAZI
    mu_assert_double_eq(0.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(15.0,pcv->zen[1]);
    mu_assert_double_eq(1.0,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(16, pcv->nz);
    // Test G01 PCO
    mu_assert_double_eq(-0.5450,pcv->pco[0][1]);
    mu_assert_double_eq(0.00000,pcv->pco[0][0]);
    mu_assert_double_eq(2.30690,pcv->pco[0][2]);

    // TEST G02 PCO
    mu_assert_double_eq(-0.54500,pcv->pco[1][1]);
    mu_assert_double_eq(0.00000,pcv->pco[1][0]);
    mu_assert_double_eq(2.30690,pcv->pco[1][2]);

    // TEST G01 PCV
    mu_assert_double_eq( 0.0019,pcv->satpcv[0][0]);
    mu_assert_double_eq( 0.0015,pcv->satpcv[0][1]);
    mu_assert_double_eq(-0.0011,pcv->satpcv[0][7]);
    mu_assert_double_eq( 0.0022,pcv->satpcv[0][15]);

    //TEST G02 PCV
    mu_assert_double_eq( 0.0002,pcv->satpcv[1][4]);
    mu_assert_double_eq(-0.0002,pcv->satpcv[1][5]);
    mu_assert_double_eq(-0.0016,pcv->satpcv[1][11]);
    mu_assert_double_eq( 0.0022,pcv->satpcv[1][15]);

}

MU_TEST(igs14_satellite_E01) {

    int status;
    double t1[6] = {2017,01,01,0,0,0};

    pcvacss_t ds_pcvs={0};
    pcvacs_t *pcv;

    const char atxfile[] = "../../cpp/src/test/data/antenna/igs14_2000.atx";
    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    printf("\nTesting the parsing of E01 (2017-01-01) Galileo from %s\t:",atxfile);
    status = readantexf(atxfile,&ds_pcvs);
    mu_assert_int_eq(1, status);

    /*----------------------------------------------------------
    // Test the values for the GPS01
    //----------------------------------------------------------*/
    pcv = findantenna("","E01",t1, &ds_pcvs,1);

    /* Do we get the right type back */
    mu_assert_string_eq("GALILEO-2           ", pcv->type);
    // Does it have a serial number
    mu_assert_string_eq("E01                 ", pcv->code);

    // read valid from
    mu_assert_double_eq(2016,pcv->tf[0]);
    mu_assert_double_eq(5,pcv->tf[1]);
    mu_assert_double_eq(24,pcv->tf[2]);
    mu_assert_double_eq(0.0,pcv->tf[3]);
    mu_assert_double_eq(0.0,pcv->tf[4]);
    mu_assert_double_eq(0.0,pcv->tf[5]);
    // read valid to
    mu_assert_double_eq(0.,pcv->tu[0]);
    mu_assert_double_eq(0.,pcv->tu[1]);
    mu_assert_double_eq(0.,pcv->tu[2]);
    mu_assert_double_eq(0.,pcv->tu[3]);
    mu_assert_double_eq(0.,pcv->tu[4]);
    mu_assert_double_eq(0.,pcv->tu[5]);

    // add read of DAZI
    mu_assert_double_eq(5.0, pcv->dazi);
    // read ZEN1 ZEN2 and DZEN
    mu_assert_double_eq(0.0,pcv->zen[0]);
    mu_assert_double_eq(20.0,pcv->zen[1]);
    mu_assert_double_eq(0.5,pcv->zen[2]);
    // Number of zenith intervals
    mu_assert_int_eq(41, pcv->nz);
    // Test E01 PCO
    mu_assert_double_eq( 0.12133,pcv->pco[0][1]);
    mu_assert_double_eq(-0.00801,pcv->pco[0][0]);
    mu_assert_double_eq( 0.72414,pcv->pco[0][2]);

    // TEST E05 PCO
    mu_assert_double_eq( 0.12214,pcv->pco[1][1]);
    mu_assert_double_eq(-0.00938,pcv->pco[1][0]);
    mu_assert_double_eq( 0.63631,pcv->pco[1][2]);

    // Crashes out from here...
    // TEST E06 PCO
    mu_assert_double_eq( 0.12204,pcv->pco[2][1]);
    mu_assert_double_eq(-0.00921,pcv->pco[2][0]);
    mu_assert_double_eq( 0.68983,pcv->pco[2][2]);

    // TEST E07 PCO
    mu_assert_double_eq( 0.12284,pcv->pco[3][1]);
    mu_assert_double_eq(-0.00898,pcv->pco[3][0]);
    mu_assert_double_eq( 0.67533,pcv->pco[3][2]);

    // TEST E08 PCO
    mu_assert_double_eq( 0.12248,pcv->pco[4][1]);
    mu_assert_double_eq(-0.00917,pcv->pco[4][0]);
    mu_assert_double_eq( 0.65582,pcv->pco[4][2]);

}
MU_TEST(test_radome2none) {

    char antennaType[21] = "LEIAR25.R3      JPLA";

    // commented out below as changed function to modify in place
    char radomeNoneAT[21];
    //strcpy(radomeNoneAT,antennaType);
    //mu_assert_string_eq(antennaType,radomeNoneAT);
    //radome2none(antennaType);
    //mu_assert_string_eq("LEIAR25.R3      NONE",radome2none(antennaType));

    //radome2none(radomeNoneAT);
    //mu_assert_string_eq("LEIAR25.R3      NONE",radomeNoneAT);
    radome2none(antennaType);
    mu_assert_string_eq("LEIAR25.R3      NONE",antennaType);

    strcpy(radomeNoneAT,antennaType);
    mu_assert_string_eq("LEIAR25.R3      NONE",radomeNoneAT);

    strcpy(antennaType,"LEIAR25.R3      DOME");
    mu_assert_string_eq("LEIAR25.R3      DOME",antennaType);

    radome2none(antennaType);
    mu_assert_string_eq("LEIAR25.R3      NONE",antennaType);

    // test an antenna type that only has 19 characters, should fail
    // TODO add function fails test to minunit.h
    strcpy(antennaType,"LEIAR25.R3     DOME");
    //radome2none(antennaType);
    //mu_assert_string_eq("LEIAR25.R3      NONE",antennaType);
}

//MU_TEST(test_antmodel) {
//    int status;
//    double t1[6] = {2000,01,01,0,0,0};
//    pcvacss_t ds_pcvs={0};
//    pcvacs_t *pcv;
//
//    /* ----------------------------------------------------------
//    * Does the ANTEX file parse correctly?
//    * -----------------------------------------------------------*/
//    status = readantexf("../../cpp/src/test/data/antenna/igs14_2000.atx",&ds_pcvs);
//    mu_assert_int_eq(1, status);
//
//    /*----------------------------------------------------------
//    / Test the values for the TRM59800.00     NONE
//    /----------------------------------------------------------*/
//    pcv = findantenna("TRM59800.00     NONE",NULL,t1,&ds_pcvs,0);
//    //extern pcvacs_t *findantenna(const char *type, const char *code, const double tc[6], const pcvacss_t *pcvs, const int id);
//    /* Do we get the right type back */
//    mu_assert_string_eq("TRM59800.00     NONE", pcv->type);
//    antmodel(opt->pcvr,opt->antdel,azel+i*2,opt->posopt[1],dantr);
//}

MU_TEST_SUITE(test_suite) {
    MU_RUN_TEST(test_read_antenna_igs14);

    // Find Antenna values for PRN01 at 2000
    MU_RUN_TEST(igs14_satellite_G01);

    // Find the antenna values for PRN01 at 2008
    MU_RUN_TEST(igs14_satellite_G01_t2);

    // Find the Antenna values for R01 at 2000
    MU_RUN_TEST(igs14_satellite_R01);
    // Find the Antenna values for R01 at 2010
    MU_RUN_TEST(igs14_satellite_R01_t2);

    // Test radome2none
    MU_RUN_TEST(test_radome2none);

    // Test antmodel call again
    //MU_RUN_TEST(test_antmodel);

    // Find the Antenna values for E01 at 2017
    // Chamber calibration values
    MU_RUN_TEST(igs14_satellite_E01);
}

int main(int argc, char *argv[]) {
    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}

