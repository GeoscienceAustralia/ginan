#include "minunit.h"

MU_TEST(test_read_antenna_igs14) {

    pcvs_t pcvs={0};
    gtime_t time={0};
    int status;
    pcv_t *pcv;

    status = readpcv("data/antenna/igs14_2000.atx", &pcvs);

    /* ----------------------------------------------------------
    * Does the ANTEX file parse correctly?
    * -----------------------------------------------------------*?
    mu_assert_int_eq(1, status);

    /*----------------------------------------------------------
    / Test the values for the TRM59800.00     NONE
    /----------------------------------------------------------*/
    pcv = searchpcv(0, "TRM59800.00     NONE", time, &pcvs);

    /* Do we get the right type back */
    mu_assert_string_eq("TRM59800.00     NONE", pcv->type);
    /* Does it have a serial number */
    mu_assert_string_eq("                    ", pcv->code);
    /* When is this calibraion valid from - important for satellites */
    /*mu_assert_string_eq("                    ", pcv->ts);
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
    // TODO add read of DAZI
    //mu_assert_string_eq("5.0", pcv->dazi);
    // TODO read ZEN1 ZEN2 and DZEN
    //mu_assert_string_eq("0.0", pcv->zen1);
    //mu_assert_string_eq("90.0", pcv->zen2);
    //mu_assert_string_eq("5.0", pcv->dzen);
    // TODO read number of frequencies 
    //mu_assert_string_eq("4", pcv->nfreq);
    // TODO read SINEX code
    //mu_assert_string_eq("IGS14_2000",pcv->sinex_code);

    // Check the PCO offsets for each frequency
    // in antex it is neu, in this data structure it is enu 
    printf("\n Testing TRM59800.00 NONE PCO G01\n");
    mu_assert_double_eq(0.00104,pcv->off[0][1]);
    mu_assert_double_eq(0.00070,pcv->off[0][0]);
    mu_assert_double_eq(0.08951,pcv->off[0][2]);
    // L2 offsets 
    printf("\n Testing TRM59800.00 NONE PCO G02\n");
    mu_assert_double_eq(0.000120,pcv->off[1][1]);
    mu_assert_double_eq(0.0000,pcv->off[1][0]);
    mu_assert_double_eq(0.11713,pcv->off[1][2]);
    // R1 offsets 
    printf("\n Testing TRM59800.00 NONE PCO R01\n");
    mu_assert_double_eq(0.00104,pcv->off[2][1]);
    mu_assert_double_eq(0.0007,pcv->off[2][0]);
    mu_assert_double_eq(0.08951,pcv->off[2][2]);
    // R1 offsets 
    printf("\n Testing TRM59800.00 NONE PCO R02\n");
    mu_assert_double_eq(0.00012,pcv->off[3][0]);
    mu_assert_double_eq(0.0,pcv->off[3][1]);
    mu_assert_double_eq(0.11713,pcv->off[3][2]);

    // Check the PCV variaions are correct at azimuth = 0 
    printf("\n Testing TRM59800.00 NONE PCV G01\n");
    mu_assert_double_eq(0.0,pcv->var[0][0]);
    mu_assert_double_eq(-0.00024,pcv->var[0][1]);
    mu_assert_double_eq(-0.00090,pcv->var[0][2]);
    mu_assert_double_eq(-0.00193,pcv->var[0][3]);
    mu_assert_double_eq(-0.00323,pcv->var[0][4]);
    mu_assert_double_eq(-0.00466,pcv->var[0][5]);
    mu_assert_double_eq(-0.00604,pcv->var[0][6]);
    mu_assert_double_eq(-0.00717,pcv->var[0][7]);
    mu_assert_double_eq(-0.00790,pcv->var[0][8]);
    mu_assert_double_eq(-0.00814,pcv->var[0][9]);
    mu_assert_double_eq(-0.00789,pcv->var[0][10]);
    mu_assert_double_eq(-0.00719,pcv->var[0][11]);
    mu_assert_double_eq(-0.00611,pcv->var[0][12]);
    mu_assert_double_eq(-0.00462,pcv->var[0][13]);
    mu_assert_double_eq(-0.00259,pcv->var[0][14]);
    mu_assert_double_eq(0.00024,pcv->var[0][15]);
    mu_assert_double_eq(0.00411,pcv->var[0][16]);
    mu_assert_double_eq(0.00917,pcv->var[0][17]);
    mu_assert_double_eq(0.01528,pcv->var[0][18]);
    */
    // Check G02 
    printf("\nTesting TRM59800.00 NONE PCV G02\n");
    mu_assert_double_eq(0.00,pcv->var[1][0]);
    mu_assert_double_eq(-0.00014,pcv->var[1][1]);
    mu_assert_double_eq(-0.00053,pcv->var[1][2]);
    mu_assert_double_eq(-0.00114,pcv->var[1][3]);
    mu_assert_double_eq(-0.00189,pcv->var[1][4]);
    mu_assert_double_eq(-0.00273,pcv->var[1][5]);
    mu_assert_double_eq(-0.00358,pcv->var[1][6]);
    mu_assert_double_eq(-0.00438,pcv->var[1][7]);
    mu_assert_double_eq(-0.00502,pcv->var[1][8]);
    mu_assert_double_eq(-0.00539,pcv->var[1][9]);
    mu_assert_double_eq(-0.00537,pcv->var[1][10]);
    mu_assert_double_eq(-0.00489,pcv->var[1][11]);
    mu_assert_double_eq(-0.00398,pcv->var[1][12]);
    mu_assert_double_eq(-0.00272,pcv->var[1][13]);
    mu_assert_double_eq(-0.00122,pcv->var[1][14]);
    mu_assert_double_eq(0.00055,pcv->var[1][15]);
    mu_assert_double_eq(0.00282,pcv->var[1][16]);
    mu_assert_double_eq(0.00604,pcv->var[1][17]);
    mu_assert_double_eq(0.01076,pcv->var[1][18]);
    /*
    // Check R01 
    printf("\nTesting TRM59800.00 NONE PCV R01\n");
    mu_assert_double_eq(0.00,pcv->var[2][0]);
    mu_assert_double_eq(-0.00025,pcv->var[2][1]);
    mu_assert_double_eq(-0.00094,pcv->var[2][2]);
    mu_assert_double_eq(-0.00202,pcv->var[2][3]);
    mu_assert_double_eq(-0.00338,pcv->var[2][4]);
    mu_assert_double_eq(-0.00488,pcv->var[2][5]);
    mu_assert_double_eq(-0.00636,pcv->var[2][6]);
    mu_assert_double_eq(-0.00760,pcv->var[2][7]);
    mu_assert_double_eq(-0.00844,pcv->var[2][8]);
    mu_assert_double_eq(-0.00879,pcv->var[2][9]);
    mu_assert_double_eq(-0.00862,pcv->var[2][10]);
    mu_assert_double_eq(-0.00800,pcv->var[2][11]);
    mu_assert_double_eq(-0.00699,pcv->var[2][12]);
    mu_assert_double_eq(-0.00559,pcv->var[2][13]);
    mu_assert_double_eq(-0.00370,pcv->var[2][14]);
    mu_assert_double_eq(-0.00103,pcv->var[2][15]);
    mu_assert_double_eq(0.00267,pcv->var[2][16]);
    mu_assert_double_eq(0.00765,pcv->var[2][17]);
    mu_assert_double_eq(0.01379,pcv->var[2][18]);
    // Check R02 
    printf("\nTesting TRM59800.00 NONE PCV R01\n");
    mu_assert_double_eq(0.00,pcv->var[3][0]);
    mu_assert_double_eq(-0.00015,pcv->var[3][1]);
    mu_assert_double_eq(-0.00059,pcv->var[3][2]);
    mu_assert_double_eq(-0.00128,pcv->var[3][3]);
    mu_assert_double_eq(-0.00213,pcv->var[3][4]);
    mu_assert_double_eq(-0.00308,pcv->var[3][5]);
    mu_assert_double_eq(-0.00404,pcv->var[3][6]);
    mu_assert_double_eq(-0.00496,pcv->var[3][7]);
    mu_assert_double_eq(-0.00575,pcv->var[3][8]);
    mu_assert_double_eq(-0.00629,pcv->var[3][9]);
    mu_assert_double_eq(-0.00645,pcv->var[3][10]);
    mu_assert_double_eq(-0.00616,pcv->var[3][11]);
    mu_assert_double_eq(-0.00540,pcv->var[3][12]);
    mu_assert_double_eq(-0.00424,pcv->var[3][13]);
    mu_assert_double_eq(-0.00278,pcv->var[3][14]);
    mu_assert_double_eq(-0.00103,pcv->var[3][15]);
    mu_assert_double_eq(0.00116,pcv->var[3][16]);
    mu_assert_double_eq(0.00410,pcv->var[3][17]);
    mu_assert_double_eq(0.00823,pcv->var[3][18]);
*/

}

MU_TEST(test_read_antenna_igs14_satellite) {

    pcvs_t pcvs={0};
    gtime_t time={0};
    int status;
    pcv_t *pcv;
   
    printf("\n\n"); 
    printf("===============================================\n");
    printf("Testing satellite antenna\n");
    printf("===============================================\n");
    status = readpcv("data/antenna/igs14_2000.atx", &pcvs);

    /* ----------------------------------------------------------
    *  Does the ANTEX file parse correctly?
    *  ----------------------------------------------------------*/
    mu_assert_int_eq(1, status);

    double ep[6];
    ep[0] = 2000;   /* year */
    ep[1] = 01;     /* month */
    ep[2] = 01;     /* dom */
    ep[3] = 0;      /* hour */
    ep[4] = 0;      /* minute */
    ep[5] = 0;      /* second */

    time = epoch2time(ep); 
    /*----------------------------------------------------------
    // Test the values for the GPS01
    //----------------------------------------------------------*/
    pcv = searchpcv(10, "", time, &pcvs);

    /* Do we get the right type back */
    mu_assert_string_eq("BLOCK IIA           ", pcv->type);
    /* Does it have a serial number
       mu_assert_string_eq("                    ", pcv->code);
       trace(4,"sat=%2d type=%20s code=%s off=%8.4f %8.4f %8.4f  %8.4f %8.4f %8.4f\n"    ,pcv->sat,pcv->type,pcv->code,pcv->off[0][0],pcv->off[0][1],pcv->off[0][2],pcv->off[1][0],pcv->off[1][1],pcv->off[1][2])*/
}

MU_TEST_SUITE(test_suite) {
    MU_RUN_TEST(test_read_antenna_igs14);
    MU_RUN_TEST(test_read_antenna_igs14_satellite);
}

int main(int argc, char *argv[]) {
    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}

