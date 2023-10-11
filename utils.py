import pandas
import geopandas
from path import Path
import os
import sys
import re
from functools import reduce


def _geo_merge(frame,geo_frames_dictionary,geo):
    if isinstance(frame.columns, pandas.core.index.MultiIndex):
        frame.columns = [
            '-'.join(x)
            for x in frame.columns.values
        ]
    current_geo = geo_frames_dictionary[geo]
    with_geo = current_geo.merge(frame, left_index=True,right_index=True).drop('',errors='ignore')
    return with_geo

# export function
def to_file(callback, desired_name, extension='csv'):
    current_files = sorted(outputs_path.files(desired_name + '*.' + extension))
    if current_files:
        last_file = current_files[-1]
        os.remove(last_file)
    final_name = '{}.{}'.format(desired_name, extension)
    callback(outputs_path / final_name)
    
    
# Import geos
def import_geos(path,index_name):
    shapes = geopandas.read_file(path)
    shapes.plot()
    output_frame = shapes[['label','geometry']]
    return output_frame

# lmi status
def LMI_status(row):
    if row.lowmodpct >= 51.00:
        val = 1
    else:
        val = 0
    return val