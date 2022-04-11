NHDPlusV2_flowline data can be found on the NAS at 

main/data/habitat/stream_network/norwest_team/NHDPlusV2Data/NHDPlus/NHDPlusV21_PN_17_NHDSnapshot_08.7z

Unzip the NHDPlusV21_PN_17_NHDSnapshot_08.7z file using 7-zip file manager and you'll find the necessary
flowline shapefiles buried in the unzipped folder. After that, the easiest way to pair down the flowlines to the watershed boundary
is to:

1) import the watershed boundary shapefile into QGIS (look in the companion watershed_boundary folder in this repo)
2) import the NHDFlowline.shp from the unzipped folder mentioned above
3) select the flowline features based on the watershed boundary ("select by location" option in QGIS and specify you want to select 
 flowline features overlapping the watershed boundary)
4) then save the selected features into this folder (right click on the "NHDFlowline.shp" layer -> Export -> Save feature as. ** Don't forget to click the
"Save only selected feature" box)