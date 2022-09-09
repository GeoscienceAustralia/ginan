import requests
import json
import argparse

import xml.etree.ElementTree as ET

#----------------------------------------
# example --->  python cost_metadata.py -s MOBS
parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-s',dest='station',default='',help="station, 4 digits, bala")

args = parser.parse_args()

mdat = {}
#----------------------------------------

def getStationSetUp(station_ID,archive_url='https://gws.geodesy.ga.gov.au/setups/search/findCurrentByFourCharacterId?id='):
    """
    Example of how to get the latest meta data from the GA archive.

    This example will return the latest meta data for each station

    To get the latest set-up of a station you can run the following command from a computer:

        Ø  curl 'https://gws.geodesy.ga.gov.au/setups/search/findCurrentByFourCharacterId?id=MOBS' -
        
    """
    archive_url = archive_url+station_ID

    request = requests.get(archive_url)
    request.raise_for_status()
    jdat = json.loads(request._content)

    for dd in jdat['equipmentInUse']:
        if dd['content']['id']['equipmentType'] == "gnss receiver":
            mdat['rcvT'] = dd['content']['id']['type']
            mdat['rcvN'] = dd['content']['id']['serialNumber']
        elif dd['content']['id']['equipmentType'] == "gnss antenna":
            mdat['antT'] = dd['content']['id']['type']
            mdat['antN'] = dd['content']['id']['serialNumber']
            mdat['antdU'] = dd['content']['configuration']['markerArpUpEccentricity']
            mdat['antdN'] = dd['content']['configuration']['markerArpNorthEccentricity']
            mdat['antdE'] = dd['content']['configuration']['markerArpEastEccentricity']
    return 1 

def getStationLog(station_ID,archive_url='https://gws.geodesy.ga.gov.au/siteLogs/search/findByFourCharacterId?id='):
    """
    To get the full site log you can run the command:

        Ø  curl 'https://gws.geodesy.ga.gov.au/siteLogs/search/findByFourCharacterId?id=MOBS&format=geodesyml' -i
    """

    ns = { 
         'geo': 'urn:xml-gov-au:icsm:egeodesy:0.4', 
         'gml': 'http://www.opengis.net/gml/3.2', 
         'xlink': 'http://www.w3.org/1999/xlink', 
         'gmd': 'http://www.isotc211.org/2005/gmd', 
         'gmx': 'http://www.isotc211.org/2005/gmx', 
         'om': 'http://www.opengis.net/om/2.0', 
         'gco': 'ttp://www.isotc211.org/2005/gco', 
         'xsi': 'http://www.w3.org/2001/XMLSchema-instance', 
    }                                                      

    archive_url = archive_url+station_ID+'&format=geodesyml'
    request = requests.get(archive_url)
    request.raise_for_status()
    root = ET.fromstring(request._content)

    mdat['domes'] = root.findall('.//geo:siteIdentification//geo:iersDOMESNumber', ns)[0].text

    pos = root.findall('.//geo:siteLocation//geo:geodeticPosition//gml:pos', ns)[0].text.split()
   
    mdat['lat']    = pos[0]
    mdat['long']   = pos[1]
    mdat['height'] = pos[2]

getStationSetUp(args.station)
getStationLog(args.station)

print('{0:},{1:},{2:},{3:},{4:},{5:},{6:},{7:},{8:},{9:},{10:}'.format(args.station,mdat['domes'],mdat['lat'],mdat['long'],mdat['height'],mdat['rcvT'],mdat['rcvN'],mdat['antT'],mdat['antdU'],mdat['antdN'],mdat['antdE']))
