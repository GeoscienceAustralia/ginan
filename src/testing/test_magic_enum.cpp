/**
 * @file test_magic_enum.cpp
 * @brief Test file demonstrating magic_enum library usage with enums_magic.h
 *
 * This file demonstrates the three basic functions requested:
 * 1. For loop through all enums
 * 2. Conversion from string to enum
 * 3. Conversion from enum to string
 *
 * Now uses the actual converted enums from enums_magic.h
 *
 * Compile with: g++ -std=c++17 -o test_magic_enum test_magic_enum.cpp -I.
 */

#include "enums_magic.h"
#include <iostream>
#include <cassert>
#include <string>

/**
 * @brief Test 1: Loop through all enum values
 *
 * Demonstrates: magic_enum::enum_values<E>()
 * This is equivalent to BETTER_ENUM's _values() method
 */
void test_enum_iteration()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 1: For loop through all enums" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Test with E_Solution enum
    std::cout << "E_Solution values:" << std::endl;
    for (auto value : magic_enum::enum_values<E_Solution>())
    {
        std::cout << "  - " << magic_enum::enum_name(value)
                  << " = " << magic_enum::enum_integer(value) << std::endl;
    }

    // Verify the count
    auto count = magic_enum::enum_count<E_Solution>();
    std::cout << "Total: " << count << " values\n" << std::endl;

    // Test with E_Sys enum
    std::cout << "E_Sys values:" << std::endl;
    for (auto value : magic_enum::enum_values<E_Sys>())
    {
        std::cout << "  - " << magic_enum::enum_name(value)
                  << " = " << magic_enum::enum_integer(value) << std::endl;
    }
    std::cout << "Total: " << magic_enum::enum_count<E_Sys>() << " values\n" << std::endl;

    // Test with E_TropModel enum (troposphere model)
    std::cout << "E_TropModel values:" << std::endl;
    for (auto value : magic_enum::enum_values<E_TropModel>())
    {
        std::cout << "  - " << magic_enum::enum_name(value) << std::endl;
    }
    std::cout << "Total: " << magic_enum::enum_count<E_TropModel>() << " values\n" << std::endl;
}

/**
 * @brief Test 2: Convert from string to enum
 *
 * Demonstrates: magic_enum::enum_cast<E>(string)
 * This is equivalent to BETTER_ENUM's _from_string() and _from_string_nocase()
 */
void test_string_to_enum()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 2: Conversion from string to enum" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Test E_Solution
    std::cout << "E_Solution conversions:" << std::endl;

    auto ppp = magic_enum::enum_cast<E_Solution>("PPP");
    if (ppp.has_value())
    {
        std::cout << "  'PPP' -> " << magic_enum::enum_name(ppp.value())
                  << " (value: " << magic_enum::enum_integer(ppp.value()) << ")" << std::endl;
        assert(ppp.value() == E_Solution::PPP);
    }

    auto single = magic_enum::enum_cast<E_Solution>("SINGLE");
    if (single.has_value())
    {
        std::cout << "  'SINGLE' -> " << magic_enum::enum_name(single.value()) << std::endl;
        assert(single.value() == E_Solution::SINGLE);
    }

    // Test case-insensitive conversion (like _from_string_nocase)
    std::cout << "\nCase-insensitive conversions:" << std::endl;
    auto gps = magic_enum::enum_cast<E_Sys>("gps", magic_enum::case_insensitive);
    if (gps.has_value())
    {
        std::cout << "  'gps' (lowercase) -> " << magic_enum::enum_name(gps.value()) << std::endl;
        assert(gps.value() == E_Sys::GPS);
    }

    auto galileo = magic_enum::enum_cast<E_Sys>("GaLiLeO", magic_enum::case_insensitive);
    if (galileo.has_value())
    {
        std::cout << "  'GaLiLeO' (mixed case) -> " << magic_enum::enum_name(galileo.value()) << std::endl;
        assert(galileo.value() == E_Sys::GAL);
    }

    // Test invalid string
    std::cout << "\nInvalid string test:" << std::endl;
    auto invalid = magic_enum::enum_cast<E_Solution>("INVALID_MODE");
    if (!invalid.has_value())
    {
        std::cout << "  'INVALID_MODE' -> (no match, as expected)" << std::endl;
    }
    std::cout << std::endl;
}

/**
 * @brief Test 3: Convert from enum to string
 *
 * Demonstrates: magic_enum::enum_name(value)
 * This is equivalent to BETTER_ENUM's _to_string() method
 */
void test_enum_to_string()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 3: Conversion from enum to string" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Test E_Solution
    std::cout << "E_Solution conversions:" << std::endl;
    E_Solution sol1 = E_Solution::PPP;
    std::cout << "  E_Solution::PPP -> '" << magic_enum::enum_name(sol1) << "'" << std::endl;
    assert(magic_enum::enum_name(sol1) == "PPP");

    E_Solution sol2 = E_Solution::SINGLE_X;
    std::cout << "  E_Solution::SINGLE_X -> '" << magic_enum::enum_name(sol2) << "'" << std::endl;
    assert(magic_enum::enum_name(sol2) == "SINGLE_X");

    // Test E_Sys
    std::cout << "\nE_Sys conversions:" << std::endl;
    E_Sys sys1 = E_Sys::GPS;
    std::cout << "  E_Sys::GPS -> '" << magic_enum::enum_name(sys1) << "'" << std::endl;
    assert(magic_enum::enum_name(sys1) == "GPS");

    E_Sys sys2 = E_Sys::GAL;
    std::cout << "  E_Sys::GAL -> '" << magic_enum::enum_name(sys2) << "'" << std::endl;
    assert(magic_enum::enum_name(sys2) == "GAL");

    // Test KF (Kalman Filter state)
    std::cout << "\nKF conversions:" << std::endl;
    KF kf1 = KF::ONE;
    std::cout << "  KF::ONE -> '" << magic_enum::enum_name(kf1) << "'" << std::endl;
    assert(magic_enum::enum_name(kf1) == "ONE");

    KF kf2 = KF::REC_POS;
    std::cout << "  KF::REC_POS -> '" << magic_enum::enum_name(kf2) << "'" << std::endl;
    assert(magic_enum::enum_name(kf2) == "REC_POS");

    std::cout << std::endl;
}

/**
 * @brief Test 4: Integer to enum and enum to integer conversions
 *
 * Demonstrates: magic_enum::enum_cast<E>(int) and magic_enum::enum_integer(value)
 * This is equivalent to BETTER_ENUM's _from_integral() and _to_integral()
 */
void test_integer_conversions()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 4: Integer <-> Enum conversions" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Test enum to integer
    std::cout << "Enum to Integer:" << std::endl;
    E_ObsCode code = E_ObsCode::L1C;
    auto code_int = magic_enum::enum_integer(code);
    std::cout << "  E_ObsCode::L1C -> " << code_int << std::endl;
    assert(code_int == 1);

    // Test integer to enum
    std::cout << "\nInteger to Enum:" << std::endl;
    auto code_from_int = magic_enum::enum_cast<E_ObsCode>(25);
    if (code_from_int.has_value())
    {
        std::cout << "  25 -> " << magic_enum::enum_name(code_from_int.value()) << std::endl;
        assert(code_from_int.value() == E_ObsCode::L5Q);
    }

    // Test with RTCM message types (large values)
    std::cout << "\nRTCM Message Types (large values):" << std::endl;
    RtcmMessageType msg = RtcmMessageType::GPS_EPHEMERIS;
    auto msg_int = magic_enum::enum_integer(msg);
    std::cout << "  RtcmMessageType::GPS_EPHEMERIS -> " << msg_int << std::endl;
    assert(msg_int == 1019);

    auto msg_from_int = magic_enum::enum_cast<RtcmMessageType>(1042);
    if (msg_from_int.has_value())
    {
        std::cout << "  1042 -> " << magic_enum::enum_name(msg_from_int.value()) << std::endl;
        assert(msg_from_int.value() == RtcmMessageType::BDS_EPHEMERIS);
    }

    std::cout << std::endl;
}

/**
 * @brief Test 5: Validation checks
 *
 * Demonstrates: magic_enum::enum_contains<E>(value)
 * Useful for checking if an integer is a valid enum value
 */
void test_validation()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 5: Enum validation" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Test valid values
    std::cout << "Valid values:" << std::endl;
    bool valid1 = magic_enum::enum_contains<E_FilterMode>(1);
    std::cout << "  Is 1 a valid E_FilterMode? " << (valid1 ? "YES" : "NO") << std::endl;
    assert(valid1);

    // Test invalid values
    std::cout << "\nInvalid values:" << std::endl;
    bool valid2 = magic_enum::enum_contains<E_FilterMode>(999);
    std::cout << "  Is 999 a valid E_FilterMode? " << (valid2 ? "YES" : "NO") << std::endl;
    assert(!valid2);

    // Test with RTCM message type
    bool valid3 = magic_enum::enum_contains<RtcmMessageType>(1019);
    std::cout << "  Is 1019 a valid RtcmMessageType? " << (valid3 ? "YES" : "NO") << std::endl;
    assert(valid3);

    std::cout << std::endl;
}

/**
 * @brief Test 6: Working with enum ranges
 *
 * Demonstrates filtering enum values between BEGIN_CLOCK_STATES and END_CLOCK_STATES
 * This is useful for processing subsets of large enums
 */
void test_enum_ranges()
{
    std::cout << "========================================" << std::endl;
    std::cout << "Test 6: Enum ranges (KF clock states)" << std::endl;
    std::cout << "========================================\n" << std::endl;

    // Get the boundary values
    auto begin_value = magic_enum::enum_integer(KF::BEGIN_CLOCK_STATES);
    auto end_value = magic_enum::enum_integer(KF::END_CLOCK_STATES);

    std::cout << "Clock states range:" << std::endl;
    std::cout << "  BEGIN_CLOCK_STATES = " << begin_value << std::endl;
    std::cout << "  END_CLOCK_STATES   = " << end_value << std::endl;
    std::cout << std::endl;

    // Iterate through all KF values and filter those within the range
    std::cout << "Clock-related states (between BEGIN and END):" << std::endl;
    int count = 0;
    for (auto value : magic_enum::enum_values<KF>())
    {
        auto int_val = magic_enum::enum_integer(value);

        // Check if this value is within the clock states range
        if (int_val > begin_value && int_val < end_value)
        {
            std::cout << "  - " << magic_enum::enum_name(value)
                      << " = " << int_val << std::endl;
            count++;
        }
    }
    std::cout << "\nTotal clock states found: " << count << std::endl;

    // Test specific clock state conversions
    std::cout << "\nSpecific clock state tests:" << std::endl;

    KF rec_clock = KF::REC_CLOCK;
    std::cout << "  KF::REC_CLOCK -> " << magic_enum::enum_name(rec_clock) << std::endl;
    assert(magic_enum::enum_name(rec_clock) == "REC_CLOCK");

    // Note: REC_SYS_BIAS is an alias for REC_CLOCK
    KF rec_sys_bias = KF::REC_SYS_BIAS;
    std::cout << "  KF::REC_SYS_BIAS -> " << magic_enum::enum_name(rec_sys_bias) << std::endl;
    std::cout << "  (Note: REC_SYS_BIAS = REC_CLOCK, so name shows as "
              << magic_enum::enum_name(rec_sys_bias) << ")" << std::endl;

    KF sat_clock = KF::SAT_CLOCK;
    std::cout << "  KF::SAT_CLOCK -> " << magic_enum::enum_name(sat_clock) << std::endl;
    assert(magic_enum::enum_name(sat_clock) == "SAT_CLOCK");

    // Check if a value is a clock state
    std::cout << "\nChecking if values are clock states:" << std::endl;

    auto rec_pos_int = magic_enum::enum_integer(KF::REC_POS);
    bool is_rec_pos_clock = (rec_pos_int > begin_value && rec_pos_int < end_value);
    std::cout << "  Is REC_POS a clock state? " << (is_rec_pos_clock ? "YES" : "NO") << std::endl;
    assert(!is_rec_pos_clock);

    auto rec_clock_int = magic_enum::enum_integer(KF::REC_CLOCK);
    bool is_rec_clock_clock = (rec_clock_int > begin_value && rec_clock_int < end_value);
    std::cout << "  Is REC_CLOCK a clock state? " << (is_rec_clock_clock ? "YES" : "NO") << std::endl;
    assert(is_rec_clock_clock);

    auto trop_int = magic_enum::enum_integer(KF::TROP);
    bool is_trop_clock = (trop_int > begin_value && trop_int < end_value);
    std::cout << "  Is TROP a clock state? " << (is_trop_clock ? "YES" : "NO") << std::endl;
    assert(!is_trop_clock);

    std::cout << std::endl;
}

int main()
{
    std::cout << "\n╔════════════════════════════════════════╗" << std::endl;
    std::cout << "║  Magic Enum Test with enums_magic.h   ║" << std::endl;
    std::cout << "╚════════════════════════════════════════╝\n" << std::endl;

    // Run all tests
    test_enum_iteration();
    test_string_to_enum();
    test_enum_to_string();
    test_integer_conversions();
    test_validation();
    test_enum_ranges();

    std::cout << "╔════════════════════════════════════════╗" << std::endl;
    std::cout << "║      All tests passed! ✓               ║" << std::endl;
    std::cout << "║  enums_magic.h works correctly         ║" << std::endl;
    std::cout << "╚════════════════════════════════════════╝" << std::endl;

    return 0;
}
