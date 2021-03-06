#!/usr/bin/env python
# encoding: utf-8
"""
parser.py

Used for parsing the csv generated from bulkdownloading from Google App Engine

Created by Andrew Hendrickson on 2013-10-15; modified by Daniel Bennett 22/9/15
"""

import json
import csv
import sys
import getopt

reload(sys)  
sys.setdefaultencoding('Cp1252')

def unicode_csv_reader(unicode_csv_data, dialect=csv.excel, **kwargs):
    # csv.py doesn't do Unicode; encode temporarily as UTF-8:
    csv_reader = csv.reader(utf_8_encoder(unicode_csv_data),
                            dialect=dialect, **kwargs)
    for row in csv_reader:
        # decode UTF-8 back to Unicode, cell by cell:
        yield [unicode(cell, 'utf-8') for cell in row]

def utf_8_encoder(unicode_csv_data):
    for line in unicode_csv_data:
        yield line.encode('utf-8','replace')

help_message = '''
parser.py <input_file> <output_file>
'''


class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "ho:v", ["help", "output="])
        except getopt.error, msg:
            raise Usage(msg)
    
        # option processing
        for option, value in opts:
            if option == "-v":
                verbose = True
            if option in ("-h", "--help"):
                raise Usage(help_message)
            if option in ("-o", "--output"):
                output = value
    
    except Usage, err:
        print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, "\t for help use --help"
        return 2

    # test the number of command line args
    if (len(sys.argv) != 3):
        print "Please specify input and output file names"
        return 0

    # set the input and output file names based on command line args
    inputFileName = sys.argv[1]
    outputFileName = sys.argv[2]

    with open(inputFileName,'rb') as tsvin:
        
        # assumes a TAB-delimited input file
        tsvin = unicode_csv_reader(tsvin, delimiter='\t')
        

        # read in the headers of input CSV file
        orig_header = next(tsvin, None)
        del orig_header[0]

        # read first row to set up headers
        first_row = next(tsvin, None)
        a = json.loads(first_row[0])

        # column names for output csv file
        fieldnames = a.keys() + orig_header

        # initialize output csv file
        test_file = open(outputFileName,'wb')
        csvwriter = csv.DictWriter(test_file, delimiter=',', fieldnames=fieldnames)
        csvwriter.writerow(dict((fn,fn) for fn in fieldnames))

        # add additional original columns into the dict for the first row
        del first_row[0]
        a.update(zip(orig_header, first_row))

        # write the first row
        csvwriter.writerow(a)

        for row in tsvin:
            # decode JSON element 
            a = json.loads(row[0])
        
            # add additional original columns into dict
            del row[0]
            a.update(zip(orig_header, row))

            # convert to acceptable format (not ascii)
            # a = a.encode('utf-8', errors = 'ignore')

            # write row to output csv
            try:
              csvwriter.writerow(a)
            except(UnicodeEncodeError, UnicodeDecodeError):
              print UnicodeEncodeError

        test_file.close()
    print "Done parsing!"


if __name__ == "__main__":
    sys.exit(main())
