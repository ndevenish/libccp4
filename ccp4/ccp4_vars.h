/*
     This code is distributed under the terms and conditions of the
     CCP4 licence agreement as `Part i)' software.  See the conditions
     in the CCP4 manual for a copyright statement.
*/

/* Author: Martyn Winn */

/* Standard strings for certain quantites - for future use */

#ifndef __CCP4_VARS__
#define __CCP4_VARS__

#define MTZFILENAME "data::mtzfile::filename"
#define MTZTITLE "data::mtzfile::title"
#define MTZSPACEGROUP "data::mtzfile::spacegroup_num"        
#define MTZNUMREFLS "data::mtzfile::num_reflections"
#define MTZMNF "data::mtzfile::missing_number_flag"
#define MTZSORTORDER "data::mtzfile::sort_order"          

#define CRYSTALXTALNAME "data::crystal::crystal_name"
#define CRYSTALPNAME "data::crystal::project_name"
#define CRYSTALCELL "data::crystal::cell"

#define DATASETDNAME "data::crystal::dataset::dataset_name"
#define DATASETWAVELENGTH "data::crystal::dataset::wavelength"

#define COLUMNLABEL "data::crystal_i::dataset_i::column_i::label"
#define COLUMNTYPE "data::crystal_i::dataset_i::column_i::type"

#endif  /*!__CCP4_VARS__ */
