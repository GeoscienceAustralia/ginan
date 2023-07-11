# Script for merging multiple QZSS operational history information files into a single SINEX file.
# Specifically merges "SATELLITE/ATTITUDE MODE" blocks.
# Individual OHI files available here: https://qzss.go.jp/en/technical/qzssinfo/index.html

from enum import Enum


class SatFile:
	def __init__(self, filename, satId):
		self.filename = filename
		self.satId = satId


class Block(Enum):
	MASS			= 1
	MANEUVER		= 2
	UNLOADING		= 3
	ATTITUDE_MODE	= 4
	NONE			= 5


def formatAttitude(satId, line):
	if "#+SATELLITE/ATTITUDE MODE" in line:
		return "+SATELLITE/ATTITUDE_MODE"
	if "#-SATELLITE/ATTITUDE MODE" in line:
		return "-SATELLITE/ATTITUDE_MODE\n"
	if "#DATE TIME START(UTC),END(UTC),ATTITUDE MODE" in line:
		return "*SVN_ DATE_TIME_START(UTC) END(UTC)___________ ATTITUDE_MODE"

	fields = line.split(',')
	widths = [20, 19, 9]
	formattedLine = " " + satId + " "
	for i, field in enumerate(fields):
		width = widths[i]
		formattedField = field[:width].ljust(width)
		if i != 0:
			formattedLine += " "
		formattedLine += formattedField
	return formattedLine


def mergeFiles(inputFiles, outputFile):
	try:
		with open(outputFile, 'w') as output:
			output.write("%=SNX\n")
			output.write("*-------------------------------------------------------------------------------\n")
			output.write("*This file was created from the following files using 'scripts/qzss_ohi_merge.py':\n")
			for file in inputFiles:
				output.write("*  '" + file.filename + "' (" + file.satId + ")\n")
			output.write("*-------------------------------------------------------------------------------\n")
			output.write("\n")

			for inputFile in inputFiles:
				with open(inputFile.filename, 'r') as file:
					currBlock = Block.NONE
					for line in file:
						line = line.strip()						
						#if "#+SATELLITE/MASS" in line:
						#	currBlock = Block.MASS
						#if "#+SATELLITE/MANEUVER" in line:
						#	currBlock = Block.MANEUVER
						#if "#+SATELLITE/UNLOADING" in line:
						#	currBlock = Block.UNLOADING
						if "#+SATELLITE/ATTITUDE MODE" in line:
							currBlock = Block.ATTITUDE_MODE

						if currBlock == Block.ATTITUDE_MODE:
							formattedLine = formatAttitude(inputFile.satId, line)
							output.write(formattedLine + "\n")

						if "#-SATELLITE/" in line:
							currBlock = Block.NONE

			output.write("%ENDSNX\n")

		print(f"Merged files into '{outputFile}' successfully.")
	except FileNotFoundError:
		print("Error: One or more input files not found.")
	except Exception as e:
		print(f"An error occurred: {str(e)}")




# Main script

inputFiles = []
inputFiles.append(SatFile(	"exampleConfigs/products/tables/qzss_ohi/ohi-qzs1.txt",		"J001"))
inputFiles.append(SatFile(	"exampleConfigs/products/tables/qzss_ohi/ohi-qzs2.txt",		"J002"))
inputFiles.append(SatFile(	"exampleConfigs/products/tables/qzss_ohi/ohi-qzs3.txt",		"J003"))
inputFiles.append(SatFile(	"exampleConfigs/products/tables/qzss_ohi/ohi-qzs4.txt",		"J004"))
inputFiles.append(SatFile(	"exampleConfigs/products/tables/qzss_ohi/ohi-qzs1r.txt",	"J005"))
outputFile =				'exampleConfigs/products/tables/qzss_yaw_modes.snx'

print("Merging the following files:")
for file in inputFiles:
	print("  Filename: '" + file.filename + "' (" + file.satId + ")")

mergeFiles(inputFiles, outputFile)
