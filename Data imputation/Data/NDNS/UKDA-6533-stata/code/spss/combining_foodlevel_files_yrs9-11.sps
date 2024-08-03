* Encoding: windows-1252.
**syntax to combine three separate NDNS Year FoodLevel files into one.

file handle folder /name = "".

dataset close all.
get file = "folder\ndns_rp_yr9a_foodleveldietarydata_uk_20210831.sav".
dataset name Year9.

get file = "folder\ndns_rp_yr10a_foodleveldietarydata_uk_20210831.sav".
dataset name Year10.

get file = "folder\ndns_rp_yr11a_foodleveldietarydata_uk_20210831.sav".
dataset name Year11.

add files
    /file Year9
    /file Year10
    /file Year11.
exe.

show n.
*should be 382818.

save outfile = "folder\ndns_rp_yr9-11a_foodleveldietarydata_uk_20210831.sav".
