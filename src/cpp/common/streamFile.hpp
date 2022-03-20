
#ifndef __ACS_FILESTREAM_HPP
#define __ACS_FILESTREAM_HPP


/** Interface to be used for file streams
*/
struct ACSFileStream
{
	struct FileState
	{
		long int&		filePos;
		std::ifstream	inputStream;
		
		FileState(
			string					path, 
			long int&				filePos, 
			std::ifstream::openmode	mode = std::ifstream::in) 
				: filePos {filePos}
		{
			if (filePos < 0)
			{
// 				BOOST_LOG_TRIVIAL(error) << "Error seeking to negative position in file at " << path << " to " << filePos
// 				<< std::endl;
				return;
			}
			
// 			std::cout << "Opening" << std::endl;
			inputStream.open(path, mode);
			
			if (!inputStream)
			{
				BOOST_LOG_TRIVIAL(error) << "Error opening file at " << path
				<< std::endl << " - " << strerror(errno);
				filePos = -1;
				return;
			}
			
// 			std::cout << "Seeking" << std::endl;
			inputStream.seekg(filePos);
			
// 			std::cout << "Using" << std::endl;
			
			if (!inputStream)
			{
				BOOST_LOG_TRIVIAL(error) << "Error seeking in file at " << filePos << " in " << path
				<< std::endl << " - " << strerror(errno);
				
				filePos = -1;
				return;
			}
		}
		
		~FileState()
		{
// 			std::cout << "Closed" << std::endl;
			if (inputStream)
			{
				filePos = inputStream.tellg();
				
				if (!inputStream)
				{
					BOOST_LOG_TRIVIAL(error) << "Error telling in file at " << filePos
					<< std::endl << " - " << strerror(errno);
					
					filePos = -1;
					return;
				}
				
				if (filePos < 0)
				{
					BOOST_LOG_TRIVIAL(error) << "Negative file pos in file at " << filePos
					<< std::endl << " - " << strerror(errno);
					return;
				}	
			}
			else
			{
// 				BOOST_LOG_TRIVIAL(error) << "InputStream is dead before destruction "
// 				<< std::endl;
				
				if (inputStream.eof())
				{
// 					BOOST_LOG_TRIVIAL(error) << "InputStream has end of file "
// 					<< std::endl;
				}
				filePos = -1;
				return;
			}
		}
	};
	
	string			path;
	long int		filePos = 0;

	ACSFileStream()
	{

	}

	void setPath(const string& path)
	{
		this->path = path;
	}

	FileState openFile()
	{
		return FileState(path, filePos);
	}
};


#endif
