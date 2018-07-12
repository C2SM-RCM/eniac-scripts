#/usr/bin/env python3

from __future__ import print_function

from sys import exit, stdout, stderr, version_info

# Check Python version
if version_info < (3, 4):
    from platform import python_version

    print("create_tolerance_file: error: create_tolerance_file requires at least python 3.4 (detected %s)" % python_version(),
          file=stderr)
    exit(1)

import argparse
import serialbox as ser
from os import path
import json

# Default tolerance
TOL = 1e-12

def fatal_error(msg):
    """ Report an error message and exit with 1 """
    print("compare: error: {}".format(msg), file=stderr)
    exit(1)

# Parse command line arguments
parser = argparse.ArgumentParser(
    description=
    """
    Generate JSON tolerance file for Serialbox2's compare.py tool.
    """
)
parser.add_argument("META_FILE", help="Path to MetaData file of serialization data", nargs=1, type=str)
parser.add_argument("-o", "--output", dest="output", default=None, help="Output JSON tolerance file name", type=str)
parser.add_argument("-t", "--tolerance", dest="tolerance", metavar="TOL", default=TOL, 
                    help="set the tolerance used for comparison to 'TOL' (default : {})".format(
                            TOL))

args = parser.parse_args()

metafile = args.META_FILE[0]
# Check if file exits
if not path.exists(metafile):
    fatal_error("MetaData file '{}' does not exist".format(metafile))
# Extract path, filename, extension
metafile = path.abspath(metafile)
directory = path.dirname(metafile)
basename = path.basename(metafile)
filename, extension = path.splitext(basename)
prefix = filename[len("MetaData-"):]

# Create serialbox2 serializer
try:
    serializer = ser.Serializer(ser.OpenModeKind.Read, directory, prefix)
except ser.SerialboxError as e:
    fatal_error(e)

# Create JSON data dictionary
json_data = {f:float(args.tolerance) for f in serializer.fieldnames() if not f.startswith("ftg_")}

# Write JSON file to stdout or provided file
if args.output is None:
    json.dump(json_data, stdout, sort_keys=True, indent=4)
else:
    with open(args.output, 'w') as fp:
        json.dump(json_data, fp, sort_keys=True, indent=4)
