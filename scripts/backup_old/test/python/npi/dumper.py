import binascii
import io
import logging
import ctypes
import sys

from bitstring import BitArray

from npi.crc24q import crc24q
from npi.rtcm import RTCM3

logging.basicConfig(level=logging.INFO)

_log = logging.getLogger(__name__)


# def main():
#     # # data = bytearray.fromhex("d3 74 63")
#     #
#     # # _log.info("Hex is [%s]", binascii.hexlify(data))
#     # # _log.info("BIN is [%s]", binascii.)
#     #
#     # # data = [0xd3, 0x74, 0x63, 0x18, 0xd1, 0x94, 0x65, 0x0d, 0x43, 0x4c, 0x53, 0x14, 0xb1, 0x2c, 0x4f]
#     #
#     # # for b in data:
#     # #     _log.info("{b:02x} -> {b:08b}".format(b=b))
#     #
#     # data = bytearray.fromhex("74 63")
#     #
#     # _log.info("length=[%d]", int.from_bytes(data, "big") & 0x3ff)
#     #
#     # data = bytearray.fromhex("18 d1")
#     #
#     # id = int.from_bytes(data, "big") & 0x03ff
#     #
#     # _log.info("id = [%d]", id)
#     #
#     # # data = ["0x43","0xf0","0x00","0x24","0x91","0x40","0x22","0x00","0x28","0x60","0x90","0x03","0x80","0x00","0x00","0x00","0x00","0x20","0xc0","0x00","0x00","0x7f","0xff","0xf9","0x1d","0x29","0x35","0x31","0x24","0xfd","0x16","0x0f","0x5d","0x2a","0x54","0xa5","0xff","0x15","0x3c","0xae","0x3c","0x7b","0x8b","0x04","0xaf","0xfa","0x10","0x6f","0xbe","0x83","0x0b","0xd8","0x05","0xcf","0x71","0x36","0x30","0x7f","0x6d","0xc6","0xf6","0xea","0x6c","0x10","0x0e","0x41","0x96","0xf4","0x19","0x2d","0x8b","0x41","0x64","0xb9","0x6d","0x0b","0x92","0xdb","0xf4","0x88","0x00","0x07","0x58","0x00","0x1e","0x81","0x8d","0x5c","0x24","0xd6","0x82","0x3c","0x4f","0xcf","0x05","0x3d","0x97","0x6b","0xda","0x54","0xb3","0xf0","0xb3","0x44","0xbc","0xbd","0x74","0x1a","0x7d","0x8a","0x8b","0x3d","0x96","0x50","0x80","0x5d","0x65","0x40","0x7b","0x60","0x40","0x5d","0x08","0x42","0xaa","0x3d","0x42","0xbd","0xce","0x82","0xb8","0xc6","0xc0","0x1c","0xf3","0x40","0x2e","0x22","0x00","0x27","0x63","0x80","0x34","0x2a","0x40","0x4d","0xd6","0xc0","0x42","0x0c","0x7f","0x33","0x36","0x7f","0x44","0x58","0x7f","0x42","0xab","0x3c","0xf1","0x4b","0xfd","0x33","0x28","0x66","0xd9","0xb6","0x6d","0x91","0xa4","0x69","0x0a","0x81","0xa0","0x68","0x18","0x9a","0x25","0x88","0xe7","0x69","0xbe","0x6f","0x99","0xe6","0x59","0x92","0x3f","0x8b","0x40","0x00","0x02","0xc0","0xa7","0xa9","0xab","0x62","0xa9","0xab","0x27","0xca","0x3e","0x93","0xac","0x29","0x2a","0x36","0xe6","0xb4","0xed","0xdc","0xee","0xe8","0xb9","0x6c","0x08","0xf0","0x49","0xf8","0x97","0x71","0x07","0x5b","0x3f","0x36","0x5e","0x6d","0x37","0x00","0x4c","0x04","0xf8","0x09","0xfe","0xd2","0x2d","0xb0","0x1b","0x59","0xc2","0x50","0x84","0xd2","0x08","0xe1","0xe9","0x7b","0xd1","0x0f","0xad","0x6f","0x06","0x1e","0xc0","0x40"]
#     #
#     # _log.info("%s", (bin(int.from_bytes(bytearray.fromhex("18 d1"), "big"))))
#     #
#     # _log.info(int("001100011010", 2))
#
#     preamble = bytearray.fromhex("D3")
#
#     header = bytearray.fromhex("00 13")
#
#     data = bytearray.fromhex("3E D7 D3 02 02 98 0E DE EF 34 B4 BD 62 AC 09 41 98 6F 33")
#
#     crc = bytearray.fromhex("36 0B 98")
#
#     # Look at the frame header
#
#     # assert header[0] == b'D3'
#
#     _log.info("preamble=[%d]", preamble[0])
#
#     bits = BitArray(header)
#
#     length = bits[6:].uint
#
#     _log.info("length=[%d]", length)
#
#     # Look at the message
#
#     bits = BitArray(data)
#
#     message_number = bits[:12].uint
#
#     _log.info("message number=[%d]", message_number)
#
#     station_id = bits[12:24].uint
#     gps_indicator = bits[30]
#     glonass_indicator = bits[31]
#     galileo_indicator = bits[32]
#     reference_station_indicator = bits[33]
#     antenna_x = bits[34:72].int * 0.0001
#     single_received_oscillator = bits[72]
#     antenna_y = bits[74:112].int * 0.0001
#     # quarter cycle indicator 2-bits???
#     antenna_z = bits[114:152].int * 0.0001
#
#     # _log.info("Z: [%d] or BE=[%d] or LE=[%d] or NE=[%d] length=[%d]", bits[114:151].int, bits[114:151].intbe, bits[114:151].intle, bits[114:151].intne, bits[114:151].length)
#
#     _log.info("station id=[%d]", station_id)
#     _log.info("GPS=[%s] GLONASS=[%s] Galileo=[%s]", gps_indicator, glonass_indicator, galileo_indicator)
#     _log.info("Antena x=[%f] y=[%f] z=[%f]", antenna_x, antenna_y, antenna_z)
#
#     _log.info("calculated CRC=[%s]", crc24q(preamble + header + data))
#
#     _log.info("calculated CRC=[%s]", crc24q(preamble + bytearray((19).to_bytes(2, "big", signed=False)) + data))
#
#     _log.info("CRC = [%s]", binascii.hexlify((3541912).to_bytes(3, "big", signed=False)))
#
#     # _log.info("length of 19 is [%s]", (19).to_bytes(2, "big", signed=False))
#     # _log.info("length of 19 is [%s]", bytearray([19]))
#     #
#     # _log.info("length of 4095 is [%s]", (4095).to_bytes(2, "big", signed=False))
#     # _log.info("length of 4095 is [%s]", bytearray([4095]))
#
#     # Look at the CRC
#
#     # length, message_number, crc = RTCM3.get_next_message(io.BytesIO(data))
#     #
#     # _log.info("length=[%d] message number=[%d]", length, message_number)


# def main():
#
#     x("D3 00 13 3E D7 D3 02 02 98 0E DE EF 34 B4 BD 62 AC 09 41 98 6F 33 36 0B 98")
#     x("d3 74 63 18 d1 94 65 0d 43 4c 53 14 b1 2c 4f")
#     x("d3 f4 73 1c c6 d4 7d 1f 0e 43 74 2d 0b 37 cd c0 00 00 00 31 7b 67 13 d1 eb 78 9e c1 bc aa 96 76 73 63 66 ea cf 42 c0 71 5c ff 07 ad 2d 78 ae 9f b9 a6 59 29 4b a9 de dc 7a c5 fb 87 f6 f7 f0 97 e0 ee aa fd 55 7a be f4 ce ed e1 db 98 34 a0 68 61 05 a1 a9 44 d4 09 a9 13 54 25 ff d7 1f ae b1 db a3 b5 c7 f0 8b c2 17 92 30 94 63 59 19 d2 34 43 b0 87 5f 80 9f 4c 3a")
#     x("d3 01 00 43 f0 00 24 91 40 22 00 28 60 90 03 80 00 00 00 00 20 c0 00 00 7f ff f9 1d 29 35 31 24 fd 16 0f 5d 2a 54 a5 ff 15 3c ae 3c 7b 8b 04 af fa 10 6f be 83 0b d8 05 cf 71 36 30 7f 6d c6 f6 ea 6c 10 0e 41 96 f4 19 2d 8b 41 64 b9 6d 0b 92 db f4 88 00 07 58 00 1e 81 8d 5c 24 d6 82 3c 4f cf 05 3d 97 6b da 54 b3 f0 b3 44 bc bd 74 1a 7d 8a 8b 3d 96 50 80 5d 65 40 7b 60 40 5d 08 42 aa 3d 42 bd ce 82 b8 c6 c0 1c f3 40 2e 22 00 27 63 80 34 2a 40 4d d6 c0 42 0c 7f 33 36 7f 44 58 7f 42 ab 3c f1 4b fd 33 28 66 d9 b6 6d 91 a4 69 0a 81 a0 68 18 9a 25 88 e7 69 be 6f 99 e6 59 92 3f 8b 40 00 02 c0 a7 a9 ab 62 a9 ab 27 ca 3e 93 ac 29 2a 36 e6 b4 ed dc ee e8 b9 6c 08 f0 49 f8 97 71 07 5b 3f 36 5e 6d 37 00 4c 04 f8 09 fe d2 2d b0 1b 59 c2 50 84 d2 08 e1 e9 7b d1 0f ad 6f 06 1e c0 40 2a 9d 26")
#
#
# def x(s):
#     b = bytearray.fromhex(s)
#
#     crc1 = crc24q(b[:-3]).to_bytes(length=3, byteorder="big", signed=False)
#     crc2 = b[-3:]
#
#     _log.info("CRC=[%s] == [%s] == [%s]", binascii.hexlify(crc1), binascii.hexlify(crc2), crc1 == crc2)

def main():

    try:

        with open("data/COCO7.dat", "rb") as f:

            # Skip to where I know the first good frame starts

            f.read(293)

            data = bytearray()

            # Find the preamble byte

            count = 0

            b = None

            while b != b'\xd3':

                b = f.read(1)

                if not b:
                    _log.debug("Reached EOF")
                    raise EOFError

                count += 1

            _log.info("Preamble found at byte [%d]", count)

            data += b

            _log.info("data is [%s]", " ".join("{:02x}".format(x) for x in data))

            # Read 2 byte length

            d = f.read(2)

            if len(d) < 2:
                raise Exception("Couldn't read 2 bytes for length")

            # Length is 10-bits from the 2-bytes

            length = int.from_bytes(d, "big") & 0x3ff

            _log.info("The length is [%d]", length)

            data += d

            _log.info("data is [%s]", " ".join("{:02x}".format(x) for x in data))

            # Read data

            d = f.read(length)

            data += d

            _log.info("data is [%s]", " ".join("{:02x}".format(x) for x in data))

            message_number = RTCM3.extract_message_number(data[3:])

            _log.info("message number=[%d]", message_number)

            # Calculate the CRC

            calculated_crc = bytearray(crc24q(data).to_bytes(3, "big", signed=False))

            # Read CRC

            crc = f.read(3)

            data += crc

            _log.info("data is [%s]", " ".join("{:02x}".format(x) for x in data))

            _log.info("Read CRC is [%s] and calculated CRC is [%s] which is [%s]", " ".join("{:02x}".format(x) for x in crc), " ".join("{:02x}".format(x) for x in calculated_crc), crc == calculated_crc)

    except EOFError:

        _log.info("Reached EOF")


if __name__ == "__main__":
    main()


