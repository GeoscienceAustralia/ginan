/**
 * @file rinex.hpp
 * @brief RINEX file format processing and observation handling
 *
 * This file contains functions and structures for reading and processing RINEX
 * (Receiver Independent Exchange Format) files, including observation data,
 * navigation data, and station information. Supports both RINEX 2.x and 3.x formats
 * with comprehensive error handling and validation.
 *
 * Key features:
 * - RINEX 2.x and 3.x format support
 * - Observation type conversion and mapping
 * - Staging pattern for robust data processing
 * - Phase observation priority resolution
 * - Comprehensive validation and error handling
 *
 * @author Geoscience Australia
 * @date 2024
 * @version 1.0
 */
#pragma once

#include <iostream>
#include <map>
#include <string>
#include <vector>
#include "common/enums.h"
#include "common/observations.hpp"  /// @todo try to remove this dependency (trying minimising header dependencies)

using std::map;
using std::string;
using std::vector;

struct RinexStation;
struct Navigation;
struct ObsList;
struct GObs;
struct SatSys;
struct GTime;

// Forward declarations for ephemeris and navigation message types
struct Eph;   // GPS/Galileo/QZS/BeiDou ephemeris
struct Geph;  // GLONASS ephemeris
struct Seph;  // SBAS ephemeris
struct Ceph;  // CNVX ephemeris
struct STO;   // System Time Offset
struct EOP;   // Earth Orientation Parameters
struct ION;   // Ionospheric parameters

/**
 * @brief Ephemeris type enumeration for RINEX navigation data
 *
 * Defines the various types of ephemeris and auxiliary data that can be
 * found in RINEX navigation files. Used for parsing and processing
 * navigation messages from different GNSS systems.
 *
 * @note Values correspond to RINEX navigation message types
 */
enum class E_EphType : short int
{
    NONE,  ///< Unknown or uninitialized ephemeris type
    EPH,   ///< GPS/QZS LNAV, GAL IFNV, BDS D1D2 Ephemeris
    GEPH,  ///< GLONASS Ephemeris (frequency division)
    SEPH,  ///< SBAS Ephemeris (geostationary satellites)
    CEPH,  ///< GPS/QZS/BDS CNVX Ephemeris (civil navigation)
    STO,   ///< System Time Offset message
    EOP,   ///< Earth Orientation Parameters message
    ION    ///< Ionospheric parameters message
};
/**
 * @brief RINEX observation code type structure
 *
 * Encapsulates observation code information for both RINEX 2.x and 3.x formats.
 * Provides helper methods to determine format compatibility and effective codes.
 *
 * RINEX 2 uses 2-character codes (e.g., L1, C1, P1)
 * RINEX 3 uses 3-character codes (e.g., L1C, C1W, L2X)
 *
 * @note One of code or code2 should be set, but not necessarily both
 */
struct CodeType
{
    char      type = 0;  ///< Single character observation type identifier ('C', 'L', 'P', 'D', 'S')
    E_ObsCode code =
        E_ObsCode::NONE;   ///< RINEX 3 style observation code (3-character codes like L1C)
    E_ObsCode2 code2 =
        E_ObsCode2::NONE;  ///< RINEX 2 style observation code (2-character codes like L1)

    /**
     * @brief Get the effective observation code for processing
     *
     * Returns the RINEX 3 style code if available, otherwise attempts conversion
     * from RINEX 2 style code. Used to normalize observation codes across formats.
     *
     * @return E_ObsCode The effective observation code, or NONE if unavailable
     *
     * @note Future enhancement: implement conversion logic from code2 to code
     */
    E_ObsCode getEffectiveCode() const
    {
        if (code != E_ObsCode::NONE)
        {
            return code;
        }

        // For RINEX 2, would need conversion logic here
        // This is where conversion from code2 to code would happen if needed
        return E_ObsCode::NONE;
    }

    /**
     * @brief Check if this represents a RINEX 2 style observation code
     *
     * @return true if code2 is set and code is not set
     * @return false otherwise
     */
    bool isRinex2Style() const { return (code2 != E_ObsCode2::NONE) && (code == E_ObsCode::NONE); }

    /**
     * @brief Check if this represents a RINEX 3 style observation code
     *
     * @return true if code is set (regardless of code2 status)
     * @return false if code is not set
     */
    bool isRinex3Style() const { return (code != E_ObsCode::NONE); }
};

/**
 * @brief Decode RINEX 2.x observation data from input stream
 *
 * Processes observation data lines from RINEX 2.x format files. Handles satellite
 * identification from epoch header and maps observation types using system-specific
 * code type configurations.
 *
 * @param inputStream Input stream containing RINEX data
 * @param line Current observation data line to process
 * @param sysCodeTypes System-specific observation code type mappings
 * @param obs Output observation structure to populate
 * @param v2SatSys Satellite system identifier from RINEX 2 epoch header
 * @param rnxRec RINEX station information and configuration
 *
 * @return int Processing status (1 = success, 0 = failure)
 *
 * @note RINEX 2 satellite ID comes from epoch header, not observation line
 * @see decodeObsDataRinex3() for RINEX 3.x equivalent
 */
int decodeObsDataRinex2(
    std::istream&                   inputStream,
    string&                         line,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    GObs&                           obs,
    SatSys&                         v2SatSys,
    RinexStation&                   rnxRec
);

/**
 * @brief Decode RINEX 3.x observation data from input stream
 *
 * Processes observation data lines from RINEX 3.x format files. Extracts satellite
 * identification from observation line and uses staging pattern for robust data
 * processing and validation.
 *
 * @param inputStream Input stream containing RINEX data
 * @param line Current observation data line to process
 * @param sysCodeTypes System-specific observation code type mappings
 * @param obs Output observation structure to populate
 * @param rnxRec RINEX station information and configuration
 *
 * @return int Processing status (1 = success, 0 = failure)
 *
 * @note RINEX 3 satellite ID is embedded in each observation line
 * @note Uses staging pattern for conflict detection and resolution
 * @see decodeObsDataRinex2() for RINEX 2.x equivalent
 */
int decodeObsDataRinex3(
    std::istream&                   inputStream,
    string&                         line,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    GObs&                           obs,
    RinexStation&                   rnxRec
);

/**
 * @brief Read and parse complete RINEX file
 *
 * Main entry point for RINEX file processing. Automatically detects file type
 * (observation, navigation, etc.) and version, then dispatches to appropriate
 * processing functions.
 *
 * @param inputStream Input stream containing RINEX file data
 * @param type Output parameter for detected file type ('O', 'N', 'G', etc.)
 * @param obsList Output list of parsed observations
 * @param nav Output navigation data structure
 * @param rnxRec Output station information and metadata
 * @param ver Output RINEX version number
 * @param sys Output primary GNSS system
 * @param tsys Output time system used in file
 * @param sysCodeTypes Output observation code type mappings by system
 *
 * @return int Processing status (positive = success, negative = error)
 *
 * @note Automatically handles both RINEX 2.x and 3.x formats
 * @note File type detection is based on header information
 */
int readRnx(
    std::istream&                   inputStream,
    char&                           type,
    ObsList&                        obsList,
    Navigation&                     nav,
    RinexStation&                   rnxRec,
    double&                         ver,
    E_Sys&                          sys,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes
);

/**
 * @brief Get human-readable description of GNSS system
 *
 * Converts GNSS system enumeration to descriptive string for logging
 * and user interface purposes.
 *
 * @param sys GNSS system enumeration value
 * @return string Human-readable system description
 *
 * @note Used primarily for diagnostic output and error messages
 */
string rinexSysDesc(E_Sys sys);

// RINEX Header Processing Functions

/**
 * @brief Set string without trailing spaces
 *
 * Copies source string to destination buffer while removing trailing spaces.
 * Ensures proper null termination and prevents buffer overflow.
 *
 * @param dst Destination character buffer
 * @param src Source string to copy
 * @param n Maximum number of characters to copy
 *
 * @warning Assumes dst buffer is large enough to hold result
 */
void setstr(char* dst, const char* src, int n);

/**
 * @brief Decode RINEX observation file header
 *
 * Parses header lines from RINEX observation files, extracting station information,
 * observation types, antenna details, and system-specific configurations.
 *
 * @param inputStream Input stream containing RINEX header data
 * @param line Current header line being processed
 * @param ver RINEX version number (2.x or 3.x)
 * @param tsys Time system used in file (GPS, UTC, etc.)
 * @param sysCodeTypes Output map of observation code types by system
 * @param nav Navigation data structure (for ionospheric parameters)
 * @param rnxRec Output structure for station information and metadata
 */
void decodeObsH(
    std::istream&                   inputStream,
    string&                         line,
    double                          ver,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    Navigation&                     nav,
    RinexStation&                   rnxRec
);

/**
 * @brief Decode RINEX navigation file header
 *
 * Parses header lines from RINEX navigation files, extracting ionospheric
 * parameters, time system corrections, and other auxiliary navigation data.
 *
 * @param line Header line to decode
 * @param sys GNSS system identifier (GPS, GLO, GAL, BDS, etc.)
 * @param nav Navigation data structure to populate
 */
void decodeNavH(string& line, E_Sys sys, Navigation& nav);

/**
 * @brief Decode GLONASS navigation file header
 *
 * Processes header lines specific to GLONASS navigation files, including
 * system time corrections and GLONASS-specific parameters.
 *
 * @param line Header line to decode
 * @param nav Navigation data structure to populate
 */
void decodeGnavH(string& line, Navigation& nav);

/**
 * @brief Decode SBAS/Geostationary navigation file header
 *
 * Processes header lines specific to SBAS and geostationary satellite
 * navigation files with unique orbital parameters.
 *
 * @param line Header line to decode
 * @param nav Navigation data structure to populate
 */
void decodeHnavH(string& line, Navigation& nav);

/**
 * @brief Read RINEX file header section
 *
 * Reads and parses the complete header section with automatic file type
 * and version detection.
 *
 * @param inputStream Input stream containing RINEX file data
 * @param ver Output RINEX version number
 * @param type Output file type character ('O', 'N', 'G', etc.)
 * @param sys Output primary GNSS system
 * @param tsys Output time system
 * @param sysCodeTypes Output observation code type mappings
 * @param nav Navigation data structure
 * @param rnxRec Station information structure
 * @return int Processing status (0 = success, negative = error)
 */
int readRnxH(
    std::istream&                   inputStream,
    double&                         ver,
    char&                           type,
    E_Sys&                          sys,
    E_TimeSys&                      tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    Navigation&                     nav,
    RinexStation&                   rnxRec
);

// RINEX Observation Processing Functions

/**
 * @brief Decode observation epoch header information
 *
 * Parses epoch header lines to extract time stamps, epoch flags, and satellite lists.
 * Handles both RINEX 2.x and 3.x epoch formats.
 *
 * @param inputStream Input stream for reading continuation lines
 * @param line Current epoch header line
 * @param ver RINEX version number
 * @param tsys Time system for time stamp interpretation
 * @param time Output time stamp for this epoch
 * @param flag Output epoch flag (0=OK, 1=power failure, etc.)
 * @param sats Output vector of satellites in this epoch
 * @return int Number of satellites in epoch, or negative for error
 */
int decodeObsEpoch(
    std::istream&   inputStream,
    string&         line,
    double          ver,
    E_TimeSys       tsys,
    GTime&          time,
    int&            flag,
    vector<SatSys>& sats
);

/**
 * @brief Read RINEX observation data body section
 *
 * Processes observation epochs and individual satellite observations with
 * data validation and error recovery.
 *
 * @param inputStream Input stream containing observation data
 * @param ver RINEX version number
 * @param tsys Time system
 * @param sysCodeTypes Observation code type mappings
 * @param flag Output epoch flag from last processed epoch
 * @param obsList Output list of parsed observations
 * @param rnxRec Station information and configuration
 * @return int Number of epochs processed, or negative for error
 */
int readRnxObsB(
    std::istream&                   inputStream,
    double                          ver,
    E_TimeSys                       tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    int&                            flag,
    ObsList&                        obsList,
    RinexStation&                   rnxRec
);

/**
 * @brief Read complete RINEX observation file
 *
 * Main entry point for processing RINEX observation files with header
 * and data section coordination.
 *
 * @param inputStream Input stream containing complete observation file
 * @param ver RINEX version number
 * @param tsys Time system used in file
 * @param sysCodeTypes Observation code type mappings
 * @param obsList Output list of all parsed observations
 * @param rnxRec Station information and metadata
 * @return int Processing status (positive = success, negative = error)
 */
int readRnxObs(
    std::istream&                   inputStream,
    double                          ver,
    E_TimeSys                       tsys,
    map<E_Sys, map<int, CodeType>>& sysCodeTypes,
    ObsList&                        obsList,
    RinexStation&                   rnxRec
);

// RINEX Navigation/Ephemeris Processing Functions

/**
 * @brief Decode GPS/Galileo/QZS/BeiDou ephemeris parameters
 *
 * Parses Keplerian orbital elements from RINEX navigation data and converts
 * to standardized ephemeris structure.
 *
 * @param ver RINEX version number
 * @param Sat Satellite system identifier
 * @param toc Time of clock reference epoch
 * @param data Vector of decoded navigation parameters
 * @param eph Output ephemeris structure
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeEph(double ver, SatSys Sat, GTime toc, vector<double>& data, Eph& eph);

/**
 * @brief Decode GLONASS ephemeris parameters
 *
 * Parses GLONASS navigation message parameters using FDMA orbital
 * representation with position/velocity state vectors.
 *
 * @param ver RINEX version number
 * @param Sat GLONASS satellite identifier
 * @param toc Time of clock reference epoch
 * @param data Vector of GLONASS navigation parameters
 * @param geph Output GLONASS ephemeris structure
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeGeph(double ver, SatSys Sat, GTime toc, vector<double>& data, Geph& geph);

/**
 * @brief Decode SBAS/geostationary satellite ephemeris
 *
 * Parses ephemeris parameters for SBAS satellites using simplified
 * geostationary orbital models.
 *
 * @param ver RINEX version number
 * @param Sat SBAS satellite identifier
 * @param toc Time of clock reference epoch
 * @param data Vector of SBAS navigation parameters
 * @param seph Output SBAS ephemeris structure
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeSeph(double ver, SatSys Sat, GTime toc, vector<double>& data, Seph& seph);

/**
 * @brief Decode CNVX (Civil Navigation) ephemeris parameters
 *
 * Processes ephemeris data from civil navigation messages for modernized
 * GNSS signals with enhanced accuracy and integrity.
 *
 * @param ver RINEX version number
 * @param Sat Satellite identifier
 * @param type Navigation message type
 * @param toc Time of clock reference epoch
 * @param data Vector of CNVX navigation parameters
 * @param ceph Output CNVX ephemeris structure
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeCeph(
    double          ver,
    SatSys          Sat,
    E_NavMsgType    type,
    GTime           toc,
    vector<double>& data,
    Ceph&           ceph
);

/**
 * @brief Decode System Time Offset (STO) message
 *
 * Processes system time offset parameters for inter-system time conversions.
 *
 * @param ver RINEX version number
 * @param Sat Reference satellite system
 * @param type Navigation message type
 * @param toc Reference time epoch
 * @param data Vector of STO parameters
 * @param sto Output system time offset structure
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeSto(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, STO& sto);

/**
 * @brief Decode Earth Orientation Parameters (EOP) message
 *
 * Processes Earth orientation parameters for precise coordinate transformations.
 *
 * @param ver RINEX version number
 * @param Sat Reference satellite system
 * @param type Navigation message type
 * @param toc Reference time epoch
 * @param data Vector of EOP parameters
 * @param eop Output Earth orientation parameters
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeEop(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, EOP& eop);

/**
 * @brief Decode ionospheric parameters (ION) message
 *
 * Processes ionospheric delay model parameters for single-frequency correction.
 *
 * @param ver RINEX version number
 * @param Sat Reference satellite system
 * @param type Navigation message type
 * @param toc Reference time epoch
 * @param data Vector of ionospheric model parameters
 * @param ion Output ionospheric parameters
 * @return int Processing status (1 = success, 0 = error)
 */
int decodeIon(double ver, SatSys Sat, E_NavMsgType type, GTime toc, vector<double>& data, ION& ion);

/**
 * @brief Read RINEX navigation data body section
 *
 * Parses navigation file data section with automatic message type detection
 * and dispatching to appropriate decoders.
 *
 * @param inputStream Input stream containing navigation data
 * @param ver RINEX version number
 * @param sys Primary GNSS system
 * @param type Output ephemeris type detected
 * @param eph Output GPS/GAL/QZS/BDS ephemeris
 * @param geph Output GLONASS ephemeris
 * @param seph Output SBAS ephemeris
 * @param ceph Output CNVX ephemeris
 * @param sto Output system time offset
 * @param eop Output Earth orientation parameters
 * @param ion Output ionospheric parameters
 * @return int Processing status (1 = success, 0 = end, negative = error)
 */
int readRnxNavB(
    std::istream& inputStream,
    double        ver,
    E_Sys         sys,
    E_EphType&    type,
    Eph&          eph,
    Geph&         geph,
    Seph&         seph,
    Ceph&         ceph,
    STO&          sto,
    EOP&          eop,
    ION&          ion
);

/**
 * @brief Read complete RINEX navigation file
 *
 * Processes complete navigation files including GPS, GLONASS, Galileo,
 * BeiDou, and mixed-constellation files.
 *
 * @param inputStream Input stream containing navigation file
 * @param ver RINEX version number
 * @param sys Primary GNSS system
 * @param nav Output navigation data structure
 * @return int Processing status (positive = records, negative = error)
 */
int readRnxNav(std::istream& inputStream, double ver, E_Sys sys, Navigation& nav);

/**
 * @brief Read RINEX clock file
 *
 * Processes RINEX clock files containing high-precision satellite and
 * station clock corrections.
 *
 * @param inputStream Input stream containing clock file data
 * @param ver RINEX version number
 * @param nav Navigation data structure for clock corrections
 * @return int Processing status (positive = records, negative = error)
 */
int readRnxClk(std::istream& inputStream, double ver, Navigation& nav);

/**
 * @brief Container for parsed RINEX observation values
 *
 * Holds a single observation value along with its Loss of Lock Indicator (LLI).
 * Used as intermediate storage during RINEX parsing operations.
 *
 * @note LLI bits indicate phase break events and cycle slip detection
 */
struct ObservationValues
{
    double value;  ///< Numerical observation value (pseudorange, phase, doppler, SNR)
    double lli;    ///< Loss of Lock Indicator (0-3, phase observations only)
};

/**
 * @brief Find existing signal or create new one in signal list
 *
 * Template function that searches for a signal with matching observation code
 * in the provided signal list. Creates and appends a new signal if not found.
 *
 * @tparam SigList Type of signal list container (e.g., std::vector<RawSig>)
 * @param sigList Reference to signal list to search/modify
 * @param obsCode Observation code to find or create
 *
 * @return RawSig* Pointer to existing or newly created signal
 *
 * @note Template design allows use with different signal list types
 * @note Always returns valid pointer - creates new entry if needed
 */
template <typename SigList>
RawSig* findOrCreateSignal(SigList& sigList, E_ObsCode obsCode)
{
    // Find existing signal
    for (auto& sig : sigList)
    {
        if (sig.code == obsCode)
        {
            return &sig;
        }
    }

    // Create new signal if not found
    RawSig raw;
    raw.code = obsCode;
    sigList.push_back(raw);
    return &sigList.back();
}

/**
 * @brief Parse observation values from RINEX formatted text buffer
 *
 * Extracts numerical observation value and Loss of Lock Indicator from
 * RINEX formatted line at specified position with comprehensive bounds checking.
 *
 * @param buff Character buffer containing RINEX observation line
 * @param position Starting position in buffer (0-based index)
 *
 * @return ObservationValues Structure containing parsed value and LLI
 *
 * @note RINEX format: 14 chars value + 1 char LLI + 1 char signal strength
 * @note Returns zeros if parsing fails or position is out of bounds
 */
ObservationValues parseObservationValues(char* buff, int position);

/**
 * @brief Assign parsed observation value to appropriate signal field
 *
 * Routes observation values to correct RawSig field based on observation type.
 * Provides type-safe assignment with validation to prevent data corruption.
 *
 * @param signal Reference to RawSig structure to modify
 * @param observationType Single character observation type ('C', 'L', 'P', 'D', 'S')
 * @param value Numerical observation value to assign
 * @param lli Loss of Lock Indicator (used only for phase observations)
 *
 * @note Only assigns non-zero values to prevent overwriting existing data
 * @note LLI assignment limited to phase ('L') observations
 */
void assignObservationValue(RawSig& signal, char observationType, double value, double lli);

/**
 * @brief Staged observation data for deferred processing
 *
 * Container for observation data that requires validation and conflict resolution
 * before final commitment. Supports both direct observations and phase observations
 * with priority-based code resolution.
 *
 * @note Used in staging pattern to enable robust data processing
 */
struct StagedObservation
{
    double    value     = 0.0;              ///< Numerical observation value
    double    lli       = 0.0;              ///< Loss of Lock Indicator
    E_ObsCode obsCode   = E_ObsCode::NONE;  ///< Primary observation code
    E_FType   frequency = E_FType::NONE;    ///< Frequency type (F1, F2, F5, etc.)
    bool      isValid   = false;            ///< Validation status flag

    vector<E_ObsCode> priorityCodes;        ///< Priority-ordered codes for phase resolution
    bool isPhaseWithPriority = false;       ///< Flag indicating priority-based phase observation
};

/**
 * @brief Composite key for staging observation map
 *
 * Uniquely identifies staged observations using observation type, frequency,
 * and observation code. Implements comparison operator for use in std::map.
 *
 * @note Ensures unique identification of observations during staging
 */
struct ObservationKey
{
    char      obsType;    ///< Single character observation type ('C', 'L', 'P', 'D', 'S')
    E_FType   frequency;  ///< Frequency enumeration (F1, F2, F5, etc.)
    E_ObsCode obsCode;    ///< Observation code enumeration

    /**
     * @brief Comparison operator for std::map ordering
     *
     * Implements lexicographic ordering: obsType, then frequency, then obsCode.
     * Required for use as key in std::map containers.
     *
     * @param other Other ObservationKey to compare against
     * @return true if this key is less than other key
     */
    bool operator<(const ObservationKey& other) const
    {
        if (obsType != other.obsType)
            return obsType < other.obsType;
        if (frequency != other.frequency)
            return frequency < other.frequency;
        return obsCode < other.obsCode;
    }
};

/// Type alias for observation staging container
using ObservationStaging = std::map<ObservationKey, StagedObservation>;

/**
 * @brief Stream output operator for ObservationKey
 *
 * Enables logging and debugging output for ObservationKey structures.
 * Formats key components in human-readable form.
 *
 * @param os Output stream reference
 * @param key ObservationKey to output
 * @return std::ostream& Reference to output stream for chaining
 */
inline std::ostream& operator<<(std::ostream& os, const ObservationKey& key)
{
    os << "{obsType:" << key.obsType << ",freq:" << key.frequency
       << ",obsCode:" << enum_to_string(key.obsCode) << "}";
    return os;
}

/**
 * @brief Stage regular observation for deferred processing
 *
 * Adds observation to staging area for later validation and commitment.
 * Used for observations that can be directly resolved without priority logic.
 *
 * @param staging Reference to staging container map
 * @param obsType Single character observation type ('C', 'L', 'P', 'D', 'S')
 * @param obsCode Resolved observation code (e.g., L1C, C1W)
 * @param frequency Frequency type enumeration
 * @param value Numerical observation value
 * @param lli Loss of Lock Indicator
 *
 * @note Creates composite key for unique identification
 * @see stagePhaseObservation() for priority-based phase observations
 */
void stageObservation(
    ObservationStaging& staging,
    char                obsType,
    E_ObsCode           obsCode,
    E_FType             frequency,
    double              value,
    double              lli
);

/**
 * @brief Commit all staged observations to final structure
 *
 * Processes staged observations with two-pass algorithm: commit code observations
 * first to establish available codes, then resolve and commit phase observations
 * using priority-based code selection.
 *
 * @param staging Container of staged observations to process
 * @param obs Reference to output GObs structure to populate
 * @param codeMap RINEX 2->3 code conversion map (for reference)
 *
 * @note Phase resolution depends on code observations being processed first
 * @note Extensive debug logging for troubleshooting priority resolution
 */
void commitStagedObservations(
    const ObservationStaging&         staging,
    GObs&                             obs,
    const map<E_ObsCode2, E_ObsCode>& codeMap
);

/**
 * @brief Validate staged observations before commitment
 *
 * Performs basic validation checks on staged observation data to ensure
 * data quality and consistency before final processing.
 *
 * @param staging Container of staged observations to validate
 * @param satellite Satellite system for context-specific validation
 *
 * @return true if all validations pass
 * @return false if validation failures detected
 *
 * @note Basic validation - see validateStagedObservationsDetailed() for comprehensive checks
 */
bool validateStagedObservations(const ObservationStaging& staging, const SatSys& satellite);

/**
 * @brief Comprehensive validation report for staged observations
 *
 * Contains detailed statistics and validation results for staged observation
 * data. Provides metrics for data quality assessment and error diagnosis.
 */
struct ValidationReport
{
    int                 totalObservations = 0;  ///< Total number of observations processed
    int                 validObservations = 0;  ///< Number of observations passing validation
    int                 conflictingTypes  = 0;  ///< Number of observation type conflicts detected
    std::map<char, int> observationCounts;      ///< Count of observations by type ('C', 'L', etc.)
    bool                passed = false;         ///< Overall validation pass/fail status
};

/**
 * @brief Resolve conflicts in staged observations
 *
 * Analyzes staged observations for conflicts and inconsistencies, applying
 * resolution strategies to ensure data integrity. Modifies staging container
 * to resolve detected conflicts.
 *
 * @param staging Reference to staging container (modified in-place)
 *
 * @note Implements conflict resolution strategies for overlapping observations
 * @note May remove or modify staged observations to resolve conflicts
 */
void resolveObservationConflicts(ObservationStaging& staging);

/**
 * @brief Comprehensive validation with detailed reporting
 *
 * Performs thorough validation of staged observations with comprehensive
 * statistics collection and detailed error reporting. Provides enhanced
 * diagnostics for data quality assessment.
 *
 * @param staging Reference to staging container (may be modified for conflict resolution)
 * @param satellite Satellite system for context-specific validation rules
 *
 * @return ValidationReport Detailed validation results and statistics
 *
 * @note More comprehensive than validateStagedObservations()
 * @note May modify staging container during conflict resolution
 */
ValidationReport
validateStagedObservationsDetailed(ObservationStaging& staging, const SatSys& satellite);

/**
 * @brief Stage phase observation with priority-based code resolution
 *
 * Stages phase observation that requires priority-based code selection.
 * Used when phase observation can map to multiple possible codes based on
 * available code observations.
 *
 * @param staging Reference to staging container map
 * @param obsType Single character observation type (typically 'L')
 * @param priorityCodes Vector of observation codes in priority order
 * @param frequency Frequency type enumeration
 * @param value Numerical phase observation value
 * @param lli Loss of Lock Indicator
 *
 * @note Code resolution occurs during commitStagedObservations()
 * @note Uses first priority code as temporary staging key
 * @see commitStagedObservations() for priority resolution implementation
 */
void stagePhaseObservation(
    ObservationStaging&      staging,
    char                     obsType,
    const vector<E_ObsCode>& priorityCodes,
    E_FType                  frequency,
    double                   value,
    double                   lli
);
