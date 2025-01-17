#include "spaceWeather.hpp"

#include <iostream>
#include <fstream>
#include <sstream>


SpaceWeather spaceWeatherData;

/** Parser of each data line
 *
 */
void ParseSpaceWeatherData(
        const std::string&   line,      ///< Each line of the file
        SpaceWeatherData&     data)     ///< Space Weather Struct
{
    std::istringstream iss(line);
    iss >> data.year >> data.month >> data.day >> data.bsrn >> data.nd;
    for (int i = 0; i < 8; i++)
    {
        iss >> data.kp[i];
    }
    iss >> data.sum;
    for (int i = 0; i < 8; i++)
    {
        iss >> data.ap[i];
    }
    iss >> data.avg >> data.cp >> data.c9 >> data.isn >> data.adj_f10_7 >> data.q >> data.adj_ctr81 >> data.adj_lst81 >> data.obs_f10_7 >> data.obs_ctr81 >> data.obs_lst81;
}

/** Read Space Weather file into the data struct
 *
 */
void SpaceWeather::read(
        std::string  filepath)      ///< File path to Space Weather file
{
    std::ifstream file(filepath);
    std::string line;
    int numObservedPoints = 0;
    int numDailyPredictedPoints = 0;
    int numMonthlyPredictedPoints = 0;
    std::vector<SpaceWeatherData> dailyPredictedData;
    std::vector<SpaceWeatherData> monthlyPredictedData;
    SpaceWeatherData data;
    if (!file)
    {
        std::cout << "Failed to open Space Weather file." << std::endl;
        return;
    }
    while (std::getline(file, line))
    {
        if (line.find("NUM_OBSERVED_POINTS") != std::string::npos)
        {
            std::istringstream iss(line);
            std::string ignore;
            iss >> ignore >> numObservedPoints;
        } else if (line.find("NUM_DAILY_PREDICTED_POINTS") != std::string::npos)
        {
            std::istringstream iss(line);
            std::string ignore;
            iss >> ignore >> numDailyPredictedPoints;
        } else if (line.find("NUM_MONTHLY_PREDICTED_POINTS") != std::string::npos)
        {
            std::istringstream iss(line);
            std::string ignore;
            iss >> ignore >> numMonthlyPredictedPoints;
        } else if (line == "BEGIN OBSERVED"
                    || line == "BEGIN DAILY_PREDICTED"
                    || line == "BEGIN MONTHLY_PREDICTED")
        {
            // Skip section headers
            continue;
        } else if (line == "END OBSERVED"
                    || line == "END DAILY_PREDICTED"
                    || line == "END MONTHLY_PREDICTED")
        {
            // Skip section endings
            continue;
        } else if (line.empty())
        {
            // Skip empty lines
            continue;
        } else
        {
            ParseSpaceWeatherData(line, data);
            if (SpaceWeather.size() < numObservedPoints)
            {
                SpaceWeather.push_back(data);
            }
            else if (dailyPredictedData.size() < numDailyPredictedPoints)
            {
                dailyPredictedData.push_back(data);
            }
            else if (monthlyPredictedData.size() < numMonthlyPredictedPoints)
            {

                monthlyPredictedData.push_back(data);
            }
        }
    }
    return;
}

// Function to check if a SpaceWeatherData element matches a given date

bool dateMatches(const SpaceWeatherData& data, int year, int month, int day)
{

    return (data.year == year && data.month == month && data.day == day);

}