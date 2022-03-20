#include "../common/snx.hpp"
#include <iostream>
#include <unistd.h>

using std::cout;
using std::cin;
using std::endl;

int main(int argc, char* argv[])
{
	int choice;
	int result;
	int year, doy, sod;
	int yds[3];
	int len;
	string filepath, line, junk;
	snx_stn_snx_t station;

	cout << "welcome to test_sinex2. Please choose from the menu below:" << endl;

	do
	{
		cout << "1. Read a sinex file" << endl;
		cout << "2. Write a sinex file" << endl;
		cout << "3. Get a satellite meta data" << endl;
		cout << "4. Get a station meta data" << endl;
		cout << "5. Show a count of how many rows of data have been read" << endl;
		cout << "6. Show a count of how many satellites (by SVN) exist" << endl;
		cout << "7. Show a count of how many stations (by sitename) exist" << endl;
		cout << "8. Reset the Sinex object" << endl;
		cout << "0: exit test" << endl;
		cout << endl << "Enter your choice: ";

		getline(cin, line);

		sscanf(line.c_str(), "%d", &choice);

		switch (choice)
		{
			case 1:
				cout << "Enter path to sinex file: ";
				getline(cin, filepath);

				if (access(filepath.c_str(), F_OK) == 0)
				{
					result = read_sinex(filepath);
					cout << "read of " << filepath << " returned " << result << endl;
				}
				else
					cout << "Cannot find " << filepath << "!!!" << endl;

				break;

			case 2:
				cout << "Enter path to new sinex file: ";
				getline(cin, filepath);
				result = write_sinex(filepath);
				cout << "write of " << filepath << " returned " << result << endl;
				break;

			case 3:
				cout << "not implemented yet" << endl;
				break;

			case 4:
				station.estimates.clear();
				cout << "Enter station to get data for: ";
				getline(cin, line);

				if ((len = line.length()) != 4)
				{
					cout << "Only 4 character names allowed." << endl;
					break;
				}

				for (int i = 0; i < len; i++)
					line[i] = toupper(line[i]);

				cout << "Enter time for data (as year doy sod): ";
				cin >> year >> doy >> sod;
				// read another return key ...
				getline(cin, junk);

				if (year < 1950 || year > 2049 || doy < 1 || doy > 365 || sod < 0 || sod > 86399)
				{
					cout << "invalid time entry." << endl;
					break;
				}

				yds[0] = year;
				yds[1] = doy;
				yds[2] = sod;
				result = getstnsnx(line, yds, &station);

				if (result != E_SnxDataMissing::NONE_MISSING)
				{
					cout << "could not get all data for station " << line << ": error code " << result << endl;

					if (result == E_SnxDataMissing::SITE_ID)
						break;
				}

				// now show the extracted station parameters
				cout << "parameters for station " << line << ";" << endl;
				cout << "name : " << station.sitecode << endl;
				cout << "point : " << station.ptcode << endl;
				cout << "monument : " << station.monuid << endl;

				if (station.has_receiver)
				{
					cout << "receiver : " << station.rectype << endl;
					cout << "receiver s/n : " << station.recsn << endl;
					cout << "receiver firmware : " << station.recfirm << endl;
				}
				else
					cout << "No receiver data for station" << endl;

				if (station.has_antenna)
				{
					cout << "antenna : " << station.anttype << endl;
					cout << "antenna s/n: " << station.antsn << endl;
				}
				else
					cout << "No antenna data for station" << endl;

				if (station.has_ecc)
				{
					cout << "eccentricity type : " << station.eccrs << endl;
					cout << "eccentricity values : " << station.ecc[0] << ", " << station.ecc[1] << ", " << station.ecc[2] << endl;
				}
				else
					cout << "No eccentricity data for station" << endl;

				if (station.has_gps_pc)
				{
					cout << "GPS phase centers (L1) : " << station.gpsl1[0] << ", " << station.gpsl1[1] << ", " << station.gpsl1[2] << endl;
					cout << "GPS phase centers (L2) : " << station.gpsl2[0] << ", " << station.gpsl2[1] << ", " << station.gpsl2[2] << endl;
				}
				else
					cout << "No GPS Phase Centers for station" << endl;

				if (station.has_estimates)
				{
					cout << "Solution estimates: " << endl;

					for (list<snx_stn_soln_t>::iterator it = station.estimates.begin(); it != station.estimates.end(); it++)
					{
						cout << "type : " << it->type << ", unit : " << it->unit << ", value : " << it->pos << ", stddev : " << it->pstd << endl;
					}
				}
				else
					cout << "No solution estimates for station" << endl;

				break;

			case 5:
				result = sinex_report(std::cout);
				break;

			case 6:
				result = sinex_site_count();
				cout << "found " << result << " sites" << endl;
				break;

			case 7:
				result = sinex_sat_count();
				cout << "found " << result << " satellites" << endl;
				break;

			case 8:
				result = sinex_reset();
				break;

			default:
				break;
		}

	}
	while (choice != 0);

	return 0;
}

